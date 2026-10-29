# Step 6 (2003-2006): AC summaries (wear, TAC, TLAC, mean log AC, sedentary < 100,
# LiPA 100-2019, MVPA >= 2020) for the whole day and in 2-hr windows.
# Valid day = >= 600 min wear between minutes 360 and 1440; valid_accel = >= 3 valid days.
# Outputs: data/2003_2006/accelerometry/summarized/pa_summary_AC_C_D.rds (all days) and
#          pa_summary_subj_AC_C_D.rds (subject means over valid days, n_good_days)

# adapted from Andrew Leroux's rnhanesdata vignette:
# https://github.com/andrew-leroux/rnhanesdata/blob/master/vignettes/NHANES_accelerometry_introduction.Rmd
library(tidyverse)

# load 1440 data
pax_d = read_rds(here::here("data", "2003_2006", "accelerometry", "minute_level", "nhanes_1440_D_PAXINTEN.rds"))
pax_c = read_rds(here::here("data", "2003_2006", "accelerometry", "minute_level", "nhanes_1440_C_PAXINTEN.rds"))
# load(here::here("data", "2003_2006","rnhanes_data", "PAXINTEN_D.rda"))
# load(here::here("data",  "2003_2006","rnhanes_data","PAXINTEN_C.rda"))

PAX = bind_rows(pax_c, pax_d)


# load flags 
flag_d = read_rds(here::here("data", "2003_2006", "accelerometry", "minute_level", "nhanes_1440_D_PAXSTAT.rds"))
flag_c = read_rds(here::here("data", "2003_2006", "accelerometry", "minute_level", "nhanes_1440_C_PAXSTAT.rds"))

FLAGS =
  bind_rows(flag_c, flag_d)


# if nonwear, make NA
FLAGS = FLAGS %>%
  mutate(across(starts_with("MIN"), ~replace(.x, .x == 0, NA_real_)))

# replace nonwear with NA
PAX[,paste0("min_",sprintf(1:1440, fmt = "%04d"))] = PAX[,paste0("min_",sprintf(1:1440, fmt = "%04d"))]*FLAGS[,paste0("min_",sprintf(1:1440, fmt = "%04d"))]

# PAX[,paste0("MIN",1:1440)] = PAX[,paste0("MIN",1:1440)]*FLAGS[,paste0("MIN",1:1440)]

flag_mat = FLAGS %>% select(starts_with("min")) %>% as.matrix()

wear = rowSums(flag_mat, na.rm = TRUE)

wear_6a_10p =
  rowSums(as.matrix(FLAGS %>% select(min_0360:min_1440)), na.rm = TRUE)



# keep  = wear >= (10 * 60)
# keep_day = wear_6a_10p >= (10 * 60)

### calculate statistics

act_mat = PAX %>% select(starts_with("min")) %>% as.matrix()

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
         PAXDAY = PAX$PAXDAY,
         # PAXCAL = PAX$PAXCAL,
         # PAXSTAT = PAX$PAXSTAT,
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
  filter(wear_6a_10p >= 600) %>%
  select(SEQN, PAXDAY)



keep_subs =
  keep_days %>%
  group_by(SEQN) %>%
  count() %>%
  filter(n >= 3) %>%
  ungroup() %>%
  pull(SEQN)


res_df_filt =
  res_df %>%
  mutate(include_day = wear_6a_10p >= 600,
         valid_accel = SEQN %in% keep_subs) |> 
  select(SEQN, PAXDAY, everything())

write_rds(res_df_filt, here::here("data", "2003_2006", "accelerometry", "summarized", "pa_summary_AC_C_D.rds"),
          compress = "xz")
########################


## get by subject

res_df_day = 
  res_df_filt |> 
  filter(valid_accel) |> 
  group_by(SEQN) %>%
  summarize(n_good_days = n(),
            across(-c(PAXDAY), ~mean(.x, na.rm = TRUE)),
            .groups = "drop")

write_rds(res_df_day, here::here("data", "2003_2006", "accelerometry", "summarized", "pa_summary_subj_AC_C_D.rds"),
          compress = "xz")
