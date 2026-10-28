# Join subject-level PA with demographics/mortality covariates and derive
# categorical covariates (race, education, self-reported health, obesity).
# Outputs: data/2011_2014/covariates_pa_all.rds, covariates_pa_valid.rds (>= 3 valid days)

library(tidyverse)

pa = readRDS(here::here("data", "2011_2014", "accelerometry", "summarized", "pa_summary_subj_AC_MIMS_G_H.rds"))
demo = readRDS(here::here("data", "2011_2014", "demographics", "covariates_mortality_G_H_tidy.rds"))

# previously read inclusion_summary.csv.gz (from nhanes_80hz pipeline)
# inclusion = read_csv(here::here("data", "2011_2014", "accelerometry", "inclusion_summary.csv.gz"),
#                  col_types = cols(SEQN = col_character()))
inclusion = read_rds(here::here("data", "2011_2014", "accelerometry", "inclusion_g_h.rds"))
accel_ids = inclusion %>%
  select(SEQN) %>%
  distinct() %>%
  pull()

joined = demo %>%
  left_join(pa, by = "SEQN") %>%
  mutate(valid_accel = n_good_days >= 3,
         has_accel = SEQN %in% accel_ids)



## add some more vars

joined = joined %>%
  mutate(cat_race = forcats::fct(case_when(
    race_hispanic_origin %in% c("Non-Hispanic Black", "Non-Hispanic White", "Mexican American") ~ race_hispanic_origin,
    is.na(race_hispanic_origin) ~ NA_character_,
    .default = "Other"
  )),
  cat_education = forcats::fct(case_when(
    education_level_adults_20 %in% c("College graduate or above", "Some college or AA degree") ~ "More than HS",
    education_level_adults_20 %in% c("High school graduate/GED or equi") ~ "HS",
    is.na(education_level_adults_20) ~ NA_character_,
    .default = "Less than HS"
  )),
  cat_self_reported_health = forcats::fct(case_when(
    general_health_condition %in% c("Excellent", "Good", "Very good") ~ "Good or better",
    general_health_condition %in% c("Fair", "Poor") ~ "Fair or poor",
    .default = general_health_condition
  )),
  bin_obese = factor(case_when(
    cat_bmi == "Obese" ~ 1,
    cat_bmi != "Obese" ~ 0,
    .default = NA_real_
  ))
  )


saveRDS(joined, here::here("data", "2011_2014", "covariates_pa_all.rds"))


valid_accel =
  joined %>%
  filter(valid_accel)

saveRDS(valid_accel, here::here("data", "2011_2014", "covariates_pa_valid.rds"))
