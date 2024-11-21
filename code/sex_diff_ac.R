library(tidyverse)
library(survey)
library(gt)
library(gtsummary)
rm(list = ls())
mvpa_subj = readRDS(here::here("data", "accelerometry", "mvpa_data.rds"))
mvpa_subj_y = readRDS(here::here("data", "accelerometry", "mvpa_data_paxy.rds"))

# make table of characteristics by sex

tab_df =
  mvpa_subj %>%
  mutate(mvpa_l150 = (vig_montoye_bout * 7) < 150,
         mvpa_150_299 = between(vig_montoye_bout * 7, 150, 299),
         mvpa_300 = (vig_montoye_bout * 7) >= 300)

# survey weighted table
df_svy =
  tab_df %>%
  filter(age_in_years_at_screening >= 18) %>%
  select(
    gender,
    mod_montoye,
    vig_montoye,
    vig_montoye_bout,
    mvpa_montoye_bout,
    total,
    total_PAXMTSM,
    # mean,
    mvpa_l150,
    mvpa_150_299,
    mvpa_300,
    # data_release_cycle,
    masked_variance_pseudo_psu, masked_variance_pseudo_stratum,
    full_sample_2_year_mec_exam_weight
  )  %>%
  mutate(WTMEC4YR = full_sample_2_year_mec_exam_weight/2,
         WTMEC4YR_norm = WTMEC4YR/mean(WTMEC4YR, na.rm = TRUE)) %>%
  select(-full_sample_2_year_mec_exam_weight) %>%
  svydesign(ids = ~masked_variance_pseudo_psu, weights = ~WTMEC4YR_norm,
            strata = ~masked_variance_pseudo_stratum, data=., nest=TRUE)

# survey weighted table
df_svy %>%
  tbl_svysummary(
    by = gender,
    include = -c(masked_variance_pseudo_psu, masked_variance_pseudo_stratum, WTMEC4YR,
                 WTMEC4YR_norm),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing",
  ) %>%
  add_overall() %>%
  add_p()

survey_design =
  tab_df %>%
  filter(age_in_years_at_screening >= 18) %>%
  mutate(age_cat = cut(age_in_years_at_screening,
                       breaks=c(18, 30, seq(40, 80, 10)))) %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2,
         weight_norm = weight / mean(weight)) %>%
  ungroup()  %>%
  survey::svydesign(
    id = ~ masked_variance_pseudo_psu,
    strata = ~ masked_variance_pseudo_stratum,
    weights = ~ weight_norm,
    data = .,
    nest = TRUE
  )

calc_by_age =
  function(var, df) {
    formula = as.formula(paste("~", var))
    total_by_age_gender = svyby(formula,
                                ~ age_cat + gender,
                                df,
                                svymean) %>%
      rename(mean = contains(var)) %>%
      mutate(metric = var)
  }

means_df =
  map_dfr(.x = mvpa_subj %>% select(contains("mvpa")) %>% colnames(),
          .f = calc_by_age, df = survey_design)


means_df %>%
  filter(metric == metric[1]) %>%
  pivot_wider(names_from = gender, values_from = c(mean, se)) %>%
  select(-metric)

# test = svyttest(mvpa_montoye ~ gender, survey_design)
pvals = c()
for(cat in unique(means_df$age_cat)){
  survey_design =
    tab_df %>%
    filter(age_in_years_at_screening >= 18) %>%
    mutate(age_cat = cut(age_in_years_at_screening,
                         breaks=c(18, 30, seq(40, 80, 10)))) %>%
    filter(age_cat == cat) %>%
    mutate(weight = full_sample_2_year_mec_exam_weight / 2,
           weight_norm = weight / mean(weight)) %>%
    ungroup()  %>%
    survey::svydesign(
      id = ~ masked_variance_pseudo_psu,
      strata = ~ masked_variance_pseudo_stratum,
      weights = ~ weight_norm,
      data = .,
      nest = TRUE
    )
  test = svyttest(mvpa_montoye ~ gender, survey_design)
  pvals = append(pvals, unname(test$p.value))
}



means_df %>%
  filter(metric == metric[1]) %>%
  pivot_wider(names_from = gender, values_from = c(mean, se)) %>%
  select(-metric) %>%
  mutate(p = pvals)





mvpa_subj %>%
  filter(age_in_years_at_screening >= 18) %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa_montoye, color = gender)) +
  geom_point(alpha = .1) +
  geom_smooth(se = F) +
  facet_wrap(.~data_release_cycle)

mvpa_subj %>%
  filter(age_in_years_at_screening >= 18) %>%
  ggplot(aes(x = age_in_years_at_screening, y = light_motoye, color = gender)) +
  geom_point(alpha = .1) +
  geom_smooth(se = F) +
  facet_wrap(.~data_release_cycle)

mvpa_subj %>%
  filter(age_in_years_at_screening >= 18) %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa_2000, color = gender)) +
  geom_point(alpha = .1) +
  geom_smooth(se = F) +
  facet_wrap(.~data_release_cycle)

mvpa_subj %>%
  filter(age_in_years_at_screening >= 18) %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa_6000, color = gender)) +
  geom_point(alpha = .1) +
  geom_smooth(se = F) +
  facet_wrap(.~data_release_cycle)


# survey weighting
survey_design =
  mvpa_subj %>%
  filter(age_in_years_at_screening >= 18 & age_in_years_at_screening < 80) %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2,
         weight_norm = weight / mean(weight)) %>%
  ungroup()  %>%
  survey::svydesign(
    id = ~ masked_variance_pseudo_psu,
    strata = ~ masked_variance_pseudo_stratum,
    weights = ~ weight_norm,
    data = .,
    nest = TRUE
  )

calc_by_age =
  function(var, df) {
    # var = "total_oaksteps"
    formula = as.formula(paste("~", var))
    total_by_age_gender = svyby(formula,
                                ~ age_in_years_at_screening + gender,
                                df,
                                svymean) %>%
      rename(mean = contains(var)) %>%
      mutate(metric = var)
  }

means_df =
  map_dfr(.x = mvpa_subj %>% select(contains("mvpa")) %>% colnames(),
          .f = calc_by_age, df = survey_design)

# get smooths for means and confidence intervals
models = means_df %>%
  mutate(lb = mean - 1.96 * se,
         ub = mean + 1.96 * se) %>%
  tidyr::nest(data = -c(metric, gender)) %>%
  dplyr::mutate(
    # Perform loess calculation on each group
    m = purrr::map(data, loess,
                   formula = mean ~ age_in_years_at_screening, span = .75),
    # Retrieve the fitted values from each model
    fitted_mean = purrr::map(m, `[[`, "fitted"),
    l = purrr::map(data, loess,
                   formula = lb ~ age_in_years_at_screening, span = .75),
    # Retrieve the fitted values from each model
    fitted_lb = purrr::map(l, `[[`, "fitted"),
    u = purrr::map(data, loess,
                   formula = ub ~ age_in_years_at_screening, span = .75),
    # Retrieve the fitted values from each model
    fitted_ub = purrr::map(u, `[[`, "fitted")
  )

# Apply fitted y's as a new column
results = models %>%
  dplyr::select(-m, -l, -u) %>%
  tidyr::unnest(cols = c(data, contains("fitted")))

results %>%
    ggplot(aes(x = age_in_years_at_screening, y = fitted_mean,
             ymin = fitted_lb, ymax = fitted_ub, color = gender, fill = gender)) +
  facet_grid(. ~ metric) +
  geom_line(linewidth = 1) +
  geom_ribbon(alpha = .2, linetype = 0) +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text =element_text(size = 14),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank()) +
  labs(x = "Age (years)", y = "Mean Daily Minutes",
       title = "Smoothed survey weighted mean daily minutes in MVPA by age ")+
  scale_x_continuous(breaks=seq(20,80,10)) +
  scale_y_continuous(breaks=seq(0,600,60))

p1



### pax y
day_level_pa_paxy = readRDS(here::here("data", "accelerometry", "nhanes_day_level_pa_paxy.rds"))
include_paxy =
  day_level_pa_paxy %>%
  filter(include_day) %>%
  select(SEQN, PAXDAYM)

ac_paxy = readRDS(here::here("data", "accelerometry", "nhanes_1440_AC_paxy.rds"))

ac_paxyfilt =
  ac_paxy %>%
  right_join(include_paxy, by = c("SEQN", "PAXDAYM"))

demo_y = read_csv(here::here("data", "demographics", "processed", "subset_Y_DEMO_BMX_DIQ_MCQ_PAQ_PFQ_raw.csv.gz"))
demo_y_small =
  demo_y %>%
  select(SEQN, age_in_years_at_screening = RIDAGEYR, sex = RIAGENDR)

mvpa_sum_paxy =
  ac_paxyfilt %>%
  pivot_longer(cols = starts_with("min")) %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(mvpa_montoye = sum(value >= 3941, na.rm = TRUE),
            light_motoye = sum(between(value, 2860, 3940), na.rm = TRUE),
            mvpa_2000 = sum(value >= 2000, na.rm = TRUE),
            mvpa_6000 = sum(value >= 6000, na.rm = TRUE),
            total = sum(value, na.rm = TRUE))

mvpa_subj_paxy =
  mvpa_sum_paxy %>%
  group_by(SEQN) %>%
  summarize(across(mvpa_montoye:total, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  left_join(demo_y_small %>% mutate(SEQN = as.character(SEQN)))


mvpa_subj_paxy %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa_montoye, color = sex)) +
  geom_jitter(alpha = .1, width = .25) +
  geom_smooth(se = F) +
  scale_x_continuous(breaks=seq(0,20,1))

mvpa_subj_paxy %>%
  ggplot(aes(x = age_in_years_at_screening, y = light_motoye, color = sex)) +
  geom_jitter(alpha = .1, width = .25) +
  geom_smooth(se = F) +
  scale_x_continuous(breaks=seq(0,20,1))

mvpa_subj_paxy %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa_2000, color =sex)) +
  geom_jitter(alpha = .1, width = .25) +
  geom_smooth(se = F) +
  scale_x_continuous(breaks=seq(0,20,1))

mvpa_subj_paxy %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa_6000, color = sex)) +
  geom_jitter(alpha = .1, width = .25) +
  geom_smooth(se = F) +
  scale_x_continuous(breaks=seq(0,20,1))



### old

mvpa_subj %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa_2000, color = gender)) +
  geom_point(alpha = .1) +
  geom_smooth(se = F)

mvpa_sum_2k =
  ac %>%
  pivot_longer(cols = starts_with("min")) %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(mvpa = sum(value >= mvpa_thresh, na.rm = TRUE))

df = mvpa_sum %>%
  group_by(SEQN) %>%
  summarize(across(mvpa, ~median(.x, na.rm = TRUE))) %>%
  left_join(demo %>% select(SEQN, age_in_years_at_screening, gender))

df %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa, color = gender)) +
  geom_point() +
  geom_smooth(se = F)

df = mvpa_sum_2k %>%
  group_by(SEQN) %>%
  summarize(across(mvpa, ~mean(.x, na.rm = TRUE))) %>%
  left_join(demo %>% select(SEQN, age_in_years_at_screening, gender))

df %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa, color = gender)) +
  geom_jitter(width = .2, alpha = .2) +
  geom_smooth(se = F)

mvpa_sum_6k =
  ac %>%
  pivot_longer(cols = starts_with("min")) %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(mvpa = sum(value >= 6000, na.rm = TRUE))

df = mvpa_sum_6k %>%
  group_by(SEQN) %>%
  summarize(across(mvpa, ~mean(.x, na.rm = TRUE))) %>%
  left_join(demo %>% select(SEQN, age_in_years_at_screening, gender))

df %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa, color = gender)) +
  geom_point() +
  geom_smooth(se = F)

df = mvpa_sum_6k %>%
  group_by(SEQN) %>%
  summarize(across(mvpa, ~mean(.x, na.rm = TRUE))) %>%
  left_join(demo %>% select(SEQN, age_in_years_at_screening, gender, data_release_cycle))

df %>%
  ggplot(aes(x = age_in_years_at_screening, y = mvpa, color = gender)) +
  geom_point(alpha = .1) +
  geom_smooth(se = F) +
  facet_wrap(.~data_release_cycle)
