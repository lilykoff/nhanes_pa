library(tidyverse)
rm(list = ls())
library(survey)
if(!require("fastFMM")) install.packages("fastFMM", dependencies = TRUE)
library(fastFMM)
library(svylme)
library(svrep)
library(tidyfun)
# library(future)
# library(furrr)
n_cores = parallel::detectCores() - 1
source(here::here("code", "prepare_data_functional.R"))
source(here::here("code", "nhanes_boot_fui.R"))
source(here::here("code", "nhanes_boot_fui_svy.R"))
source(here::here("code", "nhanes_boot_fui_svy2.R"))

nhanes_df =
  dfmat_1440 %>%
  mutate(
    AC_tf = matrix(ac, ncol = 1440),
    AC_tf = tfd(AC_tf, arg = seq(1/60, 24, length = 1440)))

min_regressions =
  nhanes_df %>%
  select(SEQN, age, gender, contains("masked"), contains("wt"), AC_tf) %>%
  tf_unnest(AC_tf) %>%
  rename(epoch = AC_tf_arg, AC = AC_tf_value) %>%
  nest(data = -epoch) %>%
  mutate(
    model = map(.x = data,
                .f = function(x){
                  des = survey::svydesign(
                    id = ~ masked_variance_pseudo_psu,
                    strata = ~ masked_variance_pseudo_stratum,
                    weights = ~ wtmec4yr_adj_norm,
                    data = x,
                    nest = TRUE
                  )
                  fit = svyglm(AC ~ age + gender,
                               family = gaussian(),
                               design = des)
                  fit}),
    result = map(model, broom::tidy)
  ) %>%
  select(epoch, result) %>%
  unnest(result)

fui_bootstrap_results =
  tibble(iteration = 1:50) %>%
  mutate(
    boot_res = map(iteration, nhanes_boot_fui_svy, df = nhanes_df)
  ) %>%
  unnest(boot_res)
saveRDS(fui_bootstrap_results, here::here("results", "fui_boot.rds"))

min_regressions_boot =
  nhanes_df %>%
  select(SEQN, age, gender, contains("masked"), contains("wt"), AC_tf) %>%
  tf_unnest(AC_tf) %>%
  rename(epoch = AC_tf_arg, AC = AC_tf_value) %>%
  nest(data = -epoch) %>%
  mutate(
    model = map(.x = data,
                .f = nhanes_boot_fui_svy2,
                n_boot = 50,
                seed = 1)
  ) %>%
  select(epoch, model) %>%
  unnest(model)

fui_bootstrap_results_v2 =
  min_regressions_boot %>%
  nest(data = -boot) %>%
  mutate(
    data = map(data, ~ tf_nest(.x, coef, .id = term, .arg = epoch)),
    data = map(data, ~ mutate(.x, smooth_coef = tf_smooth(coef)))
  ) %>%
  unnest(data) %>%
  select(iteration = boot, term, smooth_coef)

saveRDS(fui_bootstrap_results_v2, here::here("results", "fui_boot2.rds"))

pdf(here::here("results", "boot_res.pdf"))
fui_bootstrap_results %>%
  group_by(term) %>%
  summarize(
    est = mean(smooth_coef),
    se = sd(smooth_coef)) %>%
  mutate(
    ub = est + 1.96 * se,
    lb = est - 1.96 * se) %>%
  ggplot(aes(y = est, color = term)) +
  geom_spaghetti(lwd = 1.2, alpha = .9) +
  geom_spaghetti(
    data = filter(fui_bootstrap_results, iteration <= 25),
    aes(y = smooth_coef), alpha = .25, lwd = .5) +
  geom_errorband(aes(ymax = ub, ymin = lb, fill = term), alpha = .15)  +
  facet_wrap(~term, scales = "free")  +
  scale_x_continuous(
    breaks = seq(0, 24, length = 5),
    labels = str_c(seq(0, 24, length = 5), ":00")) +
  labs(x = "Time of day (hours)", y = "Coefficient")

fui_bootstrap_results_v2 %>%
  group_by(term) %>%
  summarize(
    est = mean(smooth_coef),
    se = sd(smooth_coef)) %>%
  mutate(
    ub = est + 1.96 * se,
    lb = est - 1.96 * se) %>%
  ggplot(aes(y = est, color = term)) +
  geom_spaghetti(lwd = 1.2, alpha = .9) +
  geom_spaghetti(
    data = filter(fui_bootstrap_results, iteration <= 25),
    aes(y = smooth_coef), alpha = .25, lwd = .5) +
  geom_errorband(aes(ymax = ub, ymin = lb, fill = term), alpha = .15)  +
  facet_wrap(~term, scales = "free")  +
  scale_x_continuous(
    breaks = seq(0, 24, length = 5),
    labels = str_c(seq(0, 24, length = 5), ":00")) +
  labs(x = "Time of day (hours)", y = "Coefficient")
dev.off()
