library(tidyverse)
library(tidyfun)
library(refund)
acc = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_C_PAXINTEN.rds"))
acd = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_D_PAXINTEN.rds"))
ac =
  bind_rows(acc, acd)

days = readRDS(here::here("data", "accelerometry", "inclusion_c_d.rds"))
demo = readRDS(here::here("data", "demographics", "processed", "subset_C_D_tidy.rds"))

include_days =
  days %>%
  filter(include_day) %>%
  select(SEQN, PAXDAY)

ids = unique(include_days$SEQN)


ac_filt = ac %>% right_join(include_days, by = c("SEQN", "PAXDAY"))
ac_df =
  ac_filt %>%
  filter(SEQN %in% ids[1:250]) %>%
  group_by(SEQN) %>%
  summarize(across(contains("min"), ~mean(.x, na.rm = TRUE))) %>%
  ungroup()

AC = as.matrix(ac_df %>% select(-SEQN))  %>% unname()

nhanes_df =
  demo %>%
  select(SEQN, gender, age = age_in_years_at_screening) %>%
  filter(SEQN %in% ids[1:250]) %>%
  mutate(AC_mat = AC) %>%
  mutate(
    age_cat =
      cut(age, breaks = c(18, 35, 50, 65, 80),
          include.lowest = TRUE),
    SEQN = as.factor(SEQN)) %>%
  drop_na(age, age_cat) %>%
  tibble()

nhanes_df =
  nhanes_df %>%
  mutate(
    AC_tf = matrix(AC_mat, ncol = 1440),
    AC_tf = tfd(AC_tf, arg = seq(1/60, 24, length = 1440)))

nhanes_df %>%
  group_by(age_cat, gender) %>%
  summarize(mean_ac = mean(AC_tf)) %>%
  ggplot(aes(y = mean_ac, color = age_cat)) +
  geom_spaghetti() +
  facet_grid(.~gender) +
  scale_x_continuous(
    breaks = seq(0, 24, length = 5),
    labels = str_c(seq(0, 24, length = 5), ":00")) +
  labs(x = "Time of day (hours)", y = "Average AC")

# nhanes_df =
#   nhanes_df %>%
#   mutate(
#     AC_hour =
#       tf_smooth(AC_tf, method = "rollmean", k = 60, align = "center"),
#     AC_hour = tfd(AC_hour, arg = seq(.5, 23.5, by = 1)))

nhanes_famm_df =
  nhanes_df %>%
  mutate(
    gender = as.numeric(gender == "Female"),
    AC_hour_tf =
      tf_smooth(AC_tf, method = "rollmean", k = 60, align = "center"),
    AC_hour_tf = tfd(AC_hour_tf, arg = seq(.5, 23.5, by = 1))) %>%
  tf_unnest(AC_hour_tf) %>%
  rename(epoch = AC_hour_tf_arg, AC_hour = AC_hour_tf_value) %>%
  pivot_wider(names_from = epoch, values_from = AC_hour)
## setting fill = 'extend' for start/end values.
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `MIMS_hour_tf = tf_smooth(MIMS_tf, method = "rollmean", k = 60,
##   align = "center")`.
## Caused by warning:
## ! non-equidistant arg-values in 'MIMS_tf' ignored by rollmean.

AC_hour_mat <-
  nhanes_famm_df %>%
  select(as.character(seq(0.5, 23.5, by = 1))) %>%
  as.matrix()

nhanes_famm_df <-
  nhanes_famm_df %>%
  select(SEQN, gender, age) %>%
  mutate(AC_hour_mat = I(AC_hour_mat))

nhanes_famm =
  nhanes_famm_df %>%
  pffr(AC_hour_mat ~ age + gender + s(SEQN, bs = "re"),
       data = ., algorithm = "bam", discrete = TRUE,
       bs.yindex = list(bs = "ps", k = 15, m = c(2, 1)))

pffrcoef_to_tf = function(coef) {

  coef =
    coef %>%
    mutate(
      id = "ID"
    ) %>%
    tf_nest(value:se, .id = id, .arg = yindex.vec) %>%
    select(coef = value, se)

  coef

}

pffr_coef_df =
  tibble(
    term = c("(Intercept)", "age", "genderFemale"),
    raw_coef = coef(nhanes_famm)$smterms[1:3]
  ) %>%
  mutate(
    raw_coef = map(raw_coef, "coef"),
    coef = map(raw_coef, pffrcoef_to_tf)
  ) %>%
  select(term, coef) %>%
  unnest(coef) %>%
  mutate(
    coef = coef + c(nhanes_famm$coef[1], 0, 0),
    ub = coef + 1.96 * se,
    lb = coef - 1.96 * se)

pffr_coef_df %>%
  ggplot(aes(y = coef, color = term)) +
  geom_spaghetti() +
  geom_errorband(aes(ymax = ub, ymin = lb, fill = term))  +
  facet_wrap(~term, scales = "free") +
  scale_x_continuous(breaks = seq(0, 24, length = 5)) +
  labs(x = "Time of day (hours)", y = "Average MIMS") +
  scale_x_continuous(
    breaks = seq(0, 24, length = 5),
    labels = str_c(seq(0, 24, length = 5), ":00")) +
  labs(x = "Time of day (hours)", y = "Coefficient")

