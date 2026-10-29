# Day- and subject-level PA summaries for NHANES 2011-2014.
# Inputs:  data/2011_2014/accelerometry/minute_level/nhanes_1440_{AC,PAXMTSM}.rds
#          and accelerometry/inclusion_g_h.rds (all from 00_download_data.R)
# AC summaries (wear, TAC, TLAC, Montoye sed/light/mod/vig minutes) and MIMS totals,
# for the whole day and in 2-hr windows.
# Outputs: data/2011_2014/accelerometry/summarized/pa_summary_AC_MIMS_G_H.rds (all days,
#          with inclusion columns; analogous to 2003_2006 pa_summary_AC_C_D.rds) and
#          pa_summary_subj_AC_MIMS_G_H.rds (subject means over valid days, n_good_days)

library(tidyverse)
library(slider)


## calculate AC summaries
ac = read_rds(here::here("data", "2011_2014", "accelerometry", "minute_level", "nhanes_1440_AC.rds"))
### set thresholds
sed = 2860
light = 3941
mod = 5613
hr_vec = c("00_02", "02_04", "04_06", "06_08", "08_10", "10_12", "12_14", "14_16", "16_18", "18_20", "20_22", "22_24")

act_mat = ac %>% select(starts_with("min")) %>% as.matrix()

# compute within 2 hr windows
window_list = seq(0, 1440, 120)
vars = c("wear", "tac", "tlac", "st", "lipa", "mpa", "mvpa", "vpa")
nvars = length(vars)
result_mat = matrix(NA, nrow = nrow(act_mat), ncol = nvars * (length(window_list) - 1))
for(i in 1:(length(window_list)-1)){
  start = window_list[i]
  end = window_list[i + 1]
  mat_temp = act_mat[,start:end]
  wear = rowSums(!is.na(mat_temp))
  tlac = rowSums(log(1 + mat_temp), na.rm = TRUE)
  tac = rowSums(mat_temp, na.rm = TRUE)
  st = rowSums(mat_temp < sed, na.rm = TRUE)
  lipa = rowSums(mat_temp >= sed & mat_temp <= light, na.rm = TRUE)
  mpa = rowSums(mat_temp > light & mat_temp <= mod, na.rm = TRUE)
  mvpa = rowSums(mat_temp > light, na.rm = TRUE)
  vig = rowSums(mat_temp > mod, na.rm = TRUE)
  result_mat[,((i * nvars)-(nvars-1)) : (i * nvars)] <- cbind(wear, tac, tlac, st, lipa, mpa, mvpa, vig)
}
total_df =
  tibble(
    wear = rowSums(!is.na(act_mat)),
    tac = rowSums(act_mat, na.rm = TRUE),
    tlac = rowSums(log(1 + act_mat), na.rm = TRUE),
    st = rowSums(act_mat < sed, na.rm = TRUE),
    lipa = rowSums(act_mat >= sed & act_mat <= light, na.rm = TRUE),
    mpa = rowSums(act_mat > light & act_mat <= mod, na.rm = TRUE),
    mvpa = rowSums(act_mat > light, na.rm = TRUE),
    vpa = rowSums(act_mat > mod, na.rm = TRUE)
  )

window_df =
  result_mat %>%
  as_tibble() %>%
  magrittr::set_colnames(paste(rep(vars, length(window_list)-1),
                               rep(hr_vec, each = nvars),
                               sep = "_"))

summary_df_ac =
  ac %>%
  select(SEQN, PAXDAYM, PAXDAYWM) %>%
  bind_cols(total_df) %>%
  bind_cols(window_df)

# ### calculate bout-based metrics
bout_summary_ac =
  ac %>%
  pivot_longer(cols = starts_with("min")) %>%
  group_by(SEQN, PAXDAYM) %>%
  # Apply a sliding window over the "value" column to create 10-minute windows
  # mutate(
  mutate(
    # check if 8 minutes in 10 minute window are within threshold
    mvpa_10min_bout = slide_dbl(value, .f = function(x){sum(x > light, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE),
    vpa_10min_bout = slide_dbl(value, .f = function(x){sum(x > mod, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE)
  ) %>%
  summarize(across(contains("bout"), ~sum(.x, na.rm = TRUE)),
            .groups = "drop")


mims = read_rds(here::here("data", "2011_2014", "accelerometry", "minute_level", "nhanes_1440_PAXMTSM.rds"))
act_mat = mims %>% select(starts_with("min")) %>% as.matrix()

# compute within 2 hr windows
window_list = seq(0, 1440, 120)
vars = c("tmims", "tlmims")
nvars = length(vars)
result_mat = matrix(NA, nrow = nrow(act_mat), ncol = nvars * (length(window_list) - 1))
for(i in 1:(length(window_list)-1)){
  start = window_list[i]
  end = window_list[i + 1]
  mat_temp = act_mat[,start:end]
  tlmims = rowSums(log(1 + mat_temp), na.rm = TRUE)
  tmims = rowSums(mat_temp, na.rm = TRUE)
  result_mat[,((i * nvars)-(nvars-1)) : (i * nvars)] <- cbind(tmims, tlmims)
}
total_df =
  tibble(
    tlmims = rowSums(log(1 + act_mat), na.rm = TRUE),
    tmims = rowSums(act_mat, na.rm = TRUE)
  )

window_df =
  result_mat %>%
  as_tibble() %>%
  magrittr::set_colnames(paste(rep(vars, length(window_list)-1),
                               rep(hr_vec, each = nvars),
                               sep = "_"))

summary_df_mims =
  mims %>%
  select(SEQN, PAXDAYM, PAXDAYWM) %>%
  bind_cols(total_df) %>%
  bind_cols(window_df)




summary_df =
  summary_df_ac %>%
  # left_join(summary_df_ac, by = c("SEQN", "PAXDAYM", "PAXDAYWM")) %>%
  # left_join(bout_summary_ac, by = c("SEQN", "PAXDAYM")) %>%
  left_join(summary_df_mims, by = c("SEQN", "PAXDAYM", "PAXDAYWM")) 


# previously read inclusion_summary.csv.gz (from nhanes_80hz pipeline)
# inclusion = read_csv(here::here("data", "2011_2014", "accelerometry", "inclusion_summary.csv.gz"),
#                  col_types = cols(SEQN = col_character()))
inclusion = read_rds(here::here("data", "2011_2014", "accelerometry", "inclusion_g_h.rds"))

# include_days =
#   inclusion %>%
#   filter(include) %>%
#   select(SEQN, PAXDAYM)

summary_df_filt =
  summary_df %>%
  left_join(inclusion, by = c("SEQN", "PAXDAYM"))

if (!dir.exists(here::here("data", "2011_2014", "accelerometry", "summarized"))) dir.create(here::here("data", "2011_2014", "accelerometry", "summarized"))
write_rds(summary_df_filt,
        here::here("data", "2011_2014", "accelerometry", "summarized", "pa_summary_AC_MIMS_G_H.rds"))

## subject level

summary_df_day =
  summary_df_filt %>%
  filter(include) |> 
  group_by(SEQN) %>%
  summarize(n_good_days = n(),
            across(-c(PAXDAYM, PAXDAYWM), ~mean(.x, na.rm = TRUE)),
            .groups = "drop")

write_rds(summary_df_day,
        here::here("data", "2011_2014", "accelerometry", "summarized", "pa_summary_subj_AC_MIMS_G_H.rds"))



