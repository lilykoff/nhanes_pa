library(tidyverse)

pa = read_rds(here::here("data", "2003_2006", "accelerometry", "summarized", "pa_summary_subj_AC_C_D.rds")) |> 
  mutate(SEQN = as.character(SEQN))
demo = read_rds(here::here("data", "2003_2006", "demographics", "processed", "subset_C_D_tidy.rds"))

# previously read inclusion_summary.csv.gz (from nhanes_80hz pipeline)
# inclusion = read_csv(here::here("data", "2011_2014", "accelerometry", "inclusion_summary.csv.gz"),
#                  col_types = cols(SEQN = col_character()))
inclusion = read_rds(here::here("data", "2003_2006", "accelerometry", "inclusion_c_d.rds"))

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
    race_ethnicity_recode %in% c("Non-Hispanic Black", "Non-Hispanic White", "Mexican American") ~ race_ethnicity_recode,
    is.na(race_ethnicity_recode) ~ NA_character_,
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


write_rds(joined, here::here("data", "2003_2006", "covariates_pa_all.rds"))


valid_accel =
  joined %>%
  filter(valid_accel)

write_rds(valid_accel, here::here("data", "2003_2006", "covariates_pa_valid.rds"))
