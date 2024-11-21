rm(list = ls())
library(tidyverse)

nhanes_1440_AC = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_AC.rds"))

wear = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_PAXPREDM.rds"))
flags = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_PAXFLGSM.rds"))
# replace nonwear with NA

wear_mat = wear %>% select(starts_with("min")) %>%
  mutate(across(.cols = everything(), ~if_else(.x != 3 & !is.na(.x), 1, 0))) %>%
  as.matrix()

# if flag, turn to NA

flag_mat =
  flags %>% select(starts_with("min")) %>%
  mutate(across(.cols = everything(), ~if_else(.x == FALSE, 1, NA_real_))) %>%
  as.matrix()

rm_mat = wear_mat * flag_mat
wear = rowSums(rm_mat, na.rm = TRUE)

wear_6a_10p =
  rowSums(rm_mat[,360:1440], na.rm = TRUE)


### calculate statistics

act_mat = nhanes_1440_AC %>% select(starts_with("min")) %>% as.matrix()

# compute within windows

window_list = seq(0, 1440, 120)
vars = c("wear", "tac", "tlac", "mlac", "st", "lipa", "mvpa")
nvars = length(vars)
result_mat = matrix(NA, nrow = nrow(act_mat), ncol = nvars * (length(window_list) - 1))
for(i in 1:(length(window_list)-1)){
  start = window_list[i]
  end = window_list[i + 1]
  mat_temp = act_mat[,start:end]
  tlac = rowSums(log(1 + mat_temp), na.rm = TRUE)
  tac = rowSums(mat_temp, na.rm = TRUE)
  wear = rowSums(!is.na(mat_temp))
  mlac = rowMeans(log(1 + mat_temp), na.rm = TRUE)
  st = rowSums(mat_temp < 2860, na.rm = TRUE)
  lipa = rowSums(mat_temp > 2860 & mat_temp < 3942, na.rm = TRUE)
  mvpa = rowSums(mat_temp >= 3942, na.rm = TRUE)

  # if wear is 0, mlac will the NAN but others will be 0, make NA
  st[is.nan(mlac)] <- NA_real_
  lipa[is.nan(mlac)] <- NA_real_
  mvpa[is.nan(mlac)] <- NA_real_
  tac[is.nan(mlac)] <- NA_real_
  tlac[is.nan(mlac)] <- NA_real_
  mlac[is.nan(mlac)] <- NA_real_
  result_mat[,((i * nvars)-(nvars-1)) : (i * nvars)] <- cbind(wear, tac, tlac, mlac, st, lipa, mvpa)
}
tac = rowSums(act_mat, na.rm = TRUE)
tlac = rowSums(log(1 + act_mat), na.rm = TRUE)
wear = rowSums(!is.na(act_mat))
mlac = rowMeans(log(1 + act_mat), na.rm = TRUE)
st = rowSums(act_mat < 2860, na.rm = TRUE)
lipa = rowSums(act_mat > 2860 & act_mat < 3942, na.rm = TRUE)
mvpa = rowSums(act_mat >= 3942, na.rm = TRUE)

res_df =
  result_mat %>%
  as_tibble() %>%
  magrittr::set_colnames(paste(rep(vars, length(window_list)),
                               rep(c("0_2", "2_4", "4_6", "6_8", "8_10", "10_12", "12_14", "14_16", "16_18", "18_20", "20_22", "22_24"), each = nvars),
                               sep = "_"))
res_df =
  res_df %>%
  mutate(SEQN = nhanes_1440_AC$SEQN,
         PAXDAYM = nhanes_1440_AC$PAXDAYM,
         wear = wear,
         wear_6a_10p = wear_6a_10p,
         tlac = tlac,
         tac = tac,
         mlac = mlac,
         st = st,
         lipa,
         mvpa = mvpa)


keep_days =
  res_df %>%
  filter(wear_6a_10p >= 600) %>%
  select(SEQN, PAXDAYM)


keep_subs =
  keep_days %>%
  group_by(SEQN) %>%
  count() %>%
  filter(n >= 3) %>%
  ungroup() %>%
  pull(SEQN)

keep_days =
  keep_days %>%
  filter(SEQN %in% keep_subs)

res_df_filt =
  res_df %>%
  right_join(keep_days, by = c("SEQN", "PAXDAYM"))

saveRDS(res_df_filt, here::here("data", "accelerometry", "summarized", "ac_summary.rds"), compress = "xz")

