library(tidyverse)
library(survey)
if(!require("fastFMM")) install.packages("fastFMM", dependencies = TRUE)
library(fastFMM)
library(svylme)
library(tidyfun)
n_cores = parallel::detectCores() - 1

if(!file.exists( here::here("results", "dfmat_1440.rds"))){
  logac = readRDS(here::here("data", "2011_2014", "accelerometry", "minute_level", "nhanes_1440_LAC.rds"))
  # summarize at subject level

  inclusion = read_csv(here::here("data", "2011_2014", "accelerometry", "inclusion_summary.csv.gz"),
                       col_types = cols(SEQN = col_character()))

  demo = readRDS(here::here("data", "2011_2014", "demographics", "covariates_mortality_G_H_tidy.rds"))
  source(here::here("code", "rewt_accel.R"))


  demo_small =
    demo %>%
    select(SEQN, gender, age = age_in_years_at_screening, masked_variance_pseudo_psu,
           masked_variance_pseudo_stratum, full_sample_2_year_interview_weight, full_sample_2_year_mec_exam_weight)



  include_days =
    inclusion %>%
    filter(include) %>%
    select(SEQN, PAXDAYM)

  inc = inclusion %>%
    filter(include) %>%
    group_by(SEQN) %>%
    count() %>%
    ungroup() %>%
    filter(n >= 3) %>%
    pull(SEQN)

  logac_filt =
    logac %>% right_join(include_days, by = c("SEQN", "PAXDAYM")) %>%
    left_join(demo %>% select(SEQN, gender, age = age_in_years_at_screening), by = "SEQN") %>%
    filter(age >= 18 & age <= 79 & SEQN %in% inc)

  svy_df =
    demo %>%
    filter(SEQN %in% unique(logac_filt$SEQN))

  svy_df_rwt = reweight_accel(data = svy_df,
                              demo = demo)

  logac_filt = logac_filt %>%
    left_join(svy_df_rwt %>% select(SEQN, contains("masked"), wtmec4yr_adj_norm), by = "SEQN")

  logac_filt_subj =
    logac_filt %>%
    group_by(SEQN, age, gender, masked_variance_pseudo_psu, masked_variance_pseudo_stratum, wtmec4yr_adj_norm) %>%
    summarize(across(starts_with("min"), ~mean(.x, na.rm = TRUE))) %>%
    ungroup()

  act_mat =
    logac_filt_subj %>%
    select(starts_with("min")) %>%
    as.matrix()

  dfmat_1440 =
    logac_filt_subj %>%
    select(-starts_with("min")) %>%
    mutate(ac = act_mat) %>%
    as.data.frame()

  write_rds(dfmat_1440, here::here("results", "dfmat_1440.rds"), compress = "xz")

} else {
  dfmat_1440 = readRDS(here::here("results", "dfmat_1440.rds"))
}
