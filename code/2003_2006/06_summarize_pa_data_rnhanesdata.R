# compare Andrew's data
# https://github.com/andrew-leroux/rnhanesdata/blob/master/vignettes/NHANES_accelerometry_introduction.Rmd
rm(list = ls())

# load 1440 data
load(here::here("data", "2003_2006","rnhanes_data", "PAXINTEN_D.rda"))
load(here::here("data",  "2003_2006","rnhanes_data","PAXINTEN_C.rda"))

PAX = bind_rows(PAXINTEN_C, PAXINTEN_D)
# load flags
load(here::here("data", "2003_2006", "rnhanes_data","Flags_D.rda"))
load(here::here("data", "2003_2006", "rnhanes_data", "Flags_C.rda"))

FLAGS =
  bind_rows(Flags_C, Flags_D)
# load covariates

# if nonwear, make NA
FLAGS = FLAGS %>%
  mutate(across(starts_with("MIN"), ~replace(.x, .x == 0, NA_real_)))

# replace nonwear with NA
PAX[,paste0("MIN",1:1440)] = PAX[,paste0("MIN",1:1440)]*FLAGS[,paste0("MIN",1:1440)]

flag_mat = FLAGS %>% select(starts_with("MIN")) %>% as.matrix()

wear = rowSums(flag_mat, na.rm = TRUE)

wear_6a_10p =
  rowSums(as.matrix(FLAGS %>% select(MIN360:MIN1440)), na.rm = TRUE)



# keep  = wear >= (10 * 60)
# keep_day = wear_6a_10p >= (10 * 60)

### calculate statistics

act_mat = PAX %>% select(starts_with("MIN")) %>% as.matrix()

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
  st = rowSums(mat_temp < 100, na.rm = TRUE)
  lipa = rowSums(mat_temp > 100 & mat_temp < 2020, na.rm = TRUE)
  mvpa = rowSums(mat_temp >= 2020, na.rm = TRUE)

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
st = rowSums(act_mat < 100, na.rm = TRUE)
lipa = rowSums(act_mat > 100 & act_mat < 2020, na.rm = TRUE)
mvpa = rowSums(act_mat >= 2020, na.rm = TRUE)

res_df =
  result_mat %>%
  as_tibble() %>%
  magrittr::set_colnames(paste(rep(vars, length(window_list)),
                               rep(c("0_2", "2_4", "4_6", "6_8", "8_10", "10_12", "12_14", "14_16", "16_18", "18_20", "20_22", "22_24"), each = nvars),
        sep = "_"))

res_df =
  res_df %>%
  mutate(SEQN = PAX$SEQN,
         PAXDAY = PAX$WEEKDAY,
         PAXCAL = PAX$PAXCAL,
         PAXSTAT = PAX$PAXSTAT,
         wear = wear,
         wear_6a_10p = wear_6a_10p,
         tlac = tlac,
         tac = tac,
         mlac = mlac,
         st = st,
         lipa = lipa,
         mvpa = mvpa)


keep_days =
  res_df %>%
  filter(PAXCAL == 1 & PAXSTAT == 1 & wear_6a_10p >= 600) %>%
  select(SEQN, PAXDAY)


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
  right_join(keep_days, by = c("SEQN", "PAXDAY"))

saveRDS(res_df_filt, here::here("data", "2003_2006", "accelerometry", "summarized", "paxinten_summary.rds"),
        compress = "xz")
########################
