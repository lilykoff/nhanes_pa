library(tidyverse)
library(survey)
library(broom)
library(paletteer)
rm(list = ls())

joined = readRDS(here::here("data", "2011_2014", "covariates_pa_valid.rds"))

joined = joined %>%
  mutate(age_cat = cut(age_in_years_at_screening,
                       breaks=c(18, 30, seq(40, 70, 10), 79), include.lowest = TRUE))

variable = "tmims"
age_group = "[18,30]"

run_models = function(variable, df, age_group){
  df_temp =
    joined %>%
    filter(age_cat == age_group) %>%
    mutate(weight = full_sample_2_year_mec_exam_weight / 2,
           weight_norm = weight / mean(weight))
  svy_design =
    survey::svydesign(
      id = ~ masked_variance_pseudo_psu,
      strata = ~ masked_variance_pseudo_stratum,
      weights = ~ weight_norm,
      data = df_temp,
      nest = TRUE
    )

  f1 = paste0(variable, " ~ gender + wear")
  f2 = paste0(variable, " ~ gender + wear + age_in_years_at_screening")
  f3 = paste0(variable, " ~ gender + wear + age_in_years_at_screening + cat_race + cat_education")
  f4 =paste0(variable, " ~ gender + wear + age_in_years_at_screening + cat_race + cat_education + bin_obese + cat_self_reported_health")

  m1 = svyglm(formula(f1),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m2 = svyglm(formula(f2),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m3 = svyglm(formula(f3),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m4 = svyglm(formula(f4),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  all_results =
    m1 %>%  broom::tidy(conf.int = TRUE) %>% mutate(model = "M1") %>%
    bind_rows(m2 %>% broom::tidy(conf.int = TRUE) %>% mutate(model = "M2")) %>%
    bind_rows(m3 %>% broom::tidy(conf.int = TRUE)%>% mutate(model = "M3")) %>%
    bind_rows(m4 %>% broom::tidy(conf.int = TRUE) %>% mutate(model = "M4")) %>%
    mutate(pa_var = variable,
           age = age_group)

  all_results
}




all = map_dfr(.x = unique(joined$age_cat[!is.na(joined$age_cat)]),
              .f = run_models,
              variable = "tmims",
              df = joined)

all %>%
  filter(grepl("gender", term)) %>%
  ggplot(aes(x = age, y = estimate, color = model, ymin = conf.low, ymax = conf.high)) +
  geom_point(position= position_dodge(width = .4)) +
  geom_errorbar(width = 0, position= position_dodge(width = .4)) +
  geom_hline(aes(yintercept = 0), col = "darkgrey", linetype = "dashed") +
  scale_color_brewer(palette = "Dark2", labels = c("M1: sex + wear", "M2: M1 vars + age", "M3: M2 vars + race + education",
                                  "M4: M3 vars + BMI + self reported health"))+
  theme(legend.position = "bottom") +
  labs(x = "Age", y = "Coefficient (95% CI)", title = "Total MIMS")

pa_vars = c("tmims", "tlmims", "tac", "tlac", "st", "lipa", "mpa", "mvpa", "vpa", "mvpa_10min_bout", "vpa_10min_bout")

grid = expand_grid(pa = pa_vars,
                   age = unique(joined$age_cat[!is.na(joined$age_cat)]))

all = map2_dfr(.x = grid$pa,
               .y = grid$age,
               .f = run_models,
               df = joined)


pdf(here::here("figures", "linear_regressions.pdf"))
for(v in pa_vars){
  p = all %>%
    filter(grepl("gender", term) & pa_var == v) %>%
    ggplot(aes(x = age, y = estimate, color = model, ymin = conf.low, ymax = conf.high)) +
    geom_point(position= position_dodge(width = .4)) +
    geom_errorbar(width = 0, position= position_dodge(width = .4)) +
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype = "dashed") +
    scale_color_brewer(palette = "Dark2", labels = c("M1: sex + wear", "M2: M1 vars + age", "M3: M2 vars + race + education",
                                                     "M4: M3 vars + BMI + self reported health"))+
    theme(legend.position = "bottom") +
    labs(x = "Age", y = "Male Coefficient (95% CI)", title = paste0(v))

  print(p)
}

dev.off()

## take into acct time of day

tod = "_00_02"
variable = "tmims"
age_group = "[18,30]"
run_models_tod = function(variable, df, age_group, tod){
  df_temp =
    joined %>%
    filter(age_cat == age_group) %>%
    mutate(weight = full_sample_2_year_mec_exam_weight / 2,
           weight_norm = weight / mean(weight))
  svy_design =
    survey::svydesign(
      id = ~ masked_variance_pseudo_psu,
      strata = ~ masked_variance_pseudo_stratum,
      weights = ~ weight_norm,
      data = df_temp,
      nest = TRUE
    )

  f1 = paste0(variable, tod, " ~ gender + wear", tod)
  f2 = paste0(variable, tod, " ~ gender + wear", tod, " + age_in_years_at_screening")
  f3 = paste0(variable, tod, " ~ gender + wear", tod," + age_in_years_at_screening + cat_race + cat_education")
  f4 =paste0(variable, tod, " ~ gender + wear", tod, " + age_in_years_at_screening + cat_race + cat_education + bin_obese + cat_self_reported_health")

  m1 = svyglm(formula(f1),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m2 = svyglm(formula(f2),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m3 = svyglm(formula(f3),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m4 = svyglm(formula(f4),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  all_results =
    m1 %>%  broom::tidy(conf.int = TRUE) %>% mutate(model = "M1") %>%
    bind_rows(m2 %>% broom::tidy(conf.int = TRUE) %>% mutate(model = "M2")) %>%
    bind_rows(m3 %>% broom::tidy(conf.int = TRUE)%>% mutate(model = "M3")) %>%
    bind_rows(m4 %>% broom::tidy(conf.int = TRUE) %>% mutate(model = "M4")) %>%
    mutate(pa_var = variable,
           age = age_group,
           time = tod)

  all_results
}

grid = expand_grid(pa = pa_vars,
                   age = unique(joined$age_cat[!is.na(joined$age_cat)]),
                   tod =
                     paste("_", c("00_02", "02_04", "04_06", "06_08", "08_10", "10_12", "12_14", "14_16", "16_18", "18_20", "20_22", "22_24"), sep = "")
            )

all_tod = pmap_dfr(.l = list(grid$pa, grid$age, grid$tod),
                   .f = run_models_tod,
                  df = joined)

v = "vpa"
pdf(here::here("figures", "linear_regressions_tod.pdf"))
for(v in pa_vars){
  p = all_tod %>%
    filter(grepl("gender", term) & pa_var == v) %>%
    mutate(time = factor(time,
           labels = c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24"))) %>%
    ggplot(aes(x = time, y = estimate, color = age, ymin = conf.low, ymax = conf.high)) +
    geom_point(position= position_dodge(width = .6)) +
    geom_errorbar(width = 0, position= position_dodge(width = .6)) +
    facet_wrap(.~model) +
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype = "dashed") +
    scale_color_paletteer_d("ggthemr::flat") +
    theme(legend.position = "bottom") +
    labs(x = "Age", y = "Male Coefficient (95% CI)", title = paste0(v))

  print(p)
}
dev.off()


### same but for wake only
run_models = function(variable, df, age_group){
  df_temp =
    joined %>%
    filter(age_cat == age_group) %>%
    mutate(weight = full_sample_2_year_mec_exam_weight / 2,
           weight_norm = weight / mean(weight))
  svy_design =
    survey::svydesign(
      id = ~ masked_variance_pseudo_psu,
      strata = ~ masked_variance_pseudo_stratum,
      weights = ~ weight_norm,
      data = df_temp,
      nest = TRUE
    )

  f1 = paste0(variable, " ~ gender + wake_wear")
  f2 = paste0(variable, " ~ gender + wake_wear + age_in_years_at_screening")
  f3 = paste0(variable, " ~ gender + wake_wear + age_in_years_at_screening + cat_race + cat_education")
  f4 =paste0(variable, " ~ gender + wake_wear + age_in_years_at_screening + cat_race + cat_education + bin_obese + cat_self_reported_health")

  m1 = svyglm(formula(f1),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m2 = svyglm(formula(f2),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m3 = svyglm(formula(f3),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  m4 = svyglm(formula(f4),
              data = df_temp,
              family = gaussian(),
              design = svy_design)

  all_results =
    m1 %>%  broom::tidy(conf.int = TRUE) %>% mutate(model = "M1") %>%
    bind_rows(m2 %>% broom::tidy(conf.int = TRUE) %>% mutate(model = "M2")) %>%
    bind_rows(m3 %>% broom::tidy(conf.int = TRUE)%>% mutate(model = "M3")) %>%
    bind_rows(m4 %>% broom::tidy(conf.int = TRUE) %>% mutate(model = "M4")) %>%
    mutate(pa_var = variable,
           age = age_group)

  all_results
}



pa_vars = paste("wake_",
                c("tac", "tlac", "st", "lipa", "mpa", "mvpa", "vpa"),
                sep = "")

grid = expand_grid(pa = pa_vars,
                   age = unique(joined$age_cat[!is.na(joined$age_cat)]))

all = map2_dfr(.x = grid$pa,
               .y = grid$age,
               .f = run_models,
               df = joined)


pdf(here::here("figures", "linear_regressions_wake.pdf"))
for(v in pa_vars){
  p = all %>%
    filter(grepl("gender", term) & pa_var == v) %>%
    ggplot(aes(x = age, y = estimate, color = model, ymin = conf.low, ymax = conf.high)) +
    geom_point(position= position_dodge(width = .4)) +
    geom_errorbar(width = 0, position= position_dodge(width = .4)) +
    geom_hline(aes(yintercept = 0), col = "darkgrey", linetype = "dashed") +
    scale_color_brewer(palette = "Dark2", labels = c("M1: sex + wear", "M2: M1 vars + age", "M3: M2 vars + race + education",
                                                     "M4: M3 vars + BMI + self reported health"))+
    theme(legend.position = "bottom") +
    labs(x = "Age", y = "Male Coefficient (95% CI)", title = paste0(v))

  print(p)
}

dev.off()

