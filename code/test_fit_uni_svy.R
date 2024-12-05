library(tidyverse)
library(survey)
if(!require("fastFMM")) install.packages("fastFMM", dependencies = TRUE)
library(fastFMM)
library(svylme)
n_cores = parallel::detectCores() - 1

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

logac_filt =
  logac %>% right_join(include_days, by = c("SEQN", "PAXDAYM")) %>%
  left_join(demo %>% select(SEQN, gender, age = age_in_years_at_screening), by = "SEQN") %>%
  filter(age >= 18 & age <= 79)

svy_df =
  demo %>%
  filter(SEQN %in% unique(logac_filt$SEQN))

svy_df_rwt = reweight_accel(data = svy_df,
                            demo = demo)

logac_filt = logac_filt %>%
  left_join(svy_df_rwt %>% select(SEQN, contains("masked"), wtmec4yr_adj_norm), by = "SEQN")
# logac_filt_subj =
#   logac %>%
#   group_by(SEQN) %>%
#   summarize(across(starts_with("min"), ~mean(.x, na.rm = TRUE))) %>%
#   left_join(demo_small) %>%
#   ungroup()

act_mat =
  logac_filt %>%
  select(starts_with("min")) %>%
  as.matrix()

dfmat_1440 =
  logac_filt %>%
  select(-starts_with("min")) %>%
  mutate(ac = act_mat) %>%
  as.data.frame()

data = dfmat_1440
design = survey::svydesign(id = ~ masked_variance_pseudo_psu,
                           strata = ~ masked_variance_pseudo_stratum,
                           weights = ~ wtmec4yr_adj_norm,
                           data = data,
                           nest = TRUE)

model_formula = ac ~ age + gender + (1|SEQN)



  l = 1
  out_index <- grep(paste0("^", model_formula[2]), names(data))
  data$Yl <- unclass(data[,out_index][,l])
  designtmp = survey::svydesign(id = ~ masked_variance_pseudo_psu,
                                strata = ~ masked_variance_pseudo_stratum,
                                weights = ~ wtmec4yr_adj_norm,
                                data = data,
                                nest = TRUE)
  # rep_designtemp = as.svrepdesign(designtmp)
  # lme4:: used to circumvent library(lme4) during parallelization

    fit_uni <- suppressMessages(
      lme4::lmer(
        formula = stats::as.formula(paste0("Yl ~ ", model_formula[3])),
        data = data,
        control = lme4::lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 5000)
        )
      )
    )
  print(fit_uni)
  fit_uni_svy = suppressMessages(
      svy2lme(
        formula = stats::as.formula(paste0("Yl ~ ", model_formula[3])),
        design = designtmp
      )
    )
  print(fit_uni_svy)
    # fit_uni_svy = suppressMessages(
    #   svy2lme(
    #     formula = stats::as.formula(paste0("Yl ~ ", model_formula[3])),
    #     design = rep_designtemp
    #   )
    # )


  # Fixed effects estimates
