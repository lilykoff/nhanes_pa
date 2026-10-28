library(tidyverse)
library(survey)
library(slider)

# thresholds
# https://pmc.ncbi.nlm.nih.gov/articles/PMC8150960/#sec3-sensors-21-03326
# https://journals.lww.com/acsm-msse/fulltext/2020/01000/estimating_sedentary_time_from_a_hip__and.25.aspx


ac = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_AC.rds"))

days = read_csv(here::here("data", "accelerometry", "inclusion_summary.csv.gz"),
                col_types = list(SEQN = col_character()))

include_days =
  days %>%
  filter(include) %>%
  select(SEQN, PAXDAYM)

ac_filt = ac %>% right_join(include_days, by = c("SEQN", "PAXDAYM"))

## calculate distributions

ac_long =
  ac_filt %>%
  pivot_longer(cols = starts_with("min"))


quantile(ac_long$value, c(.01, 0.05, seq(.1, .9, .1), .99, .995), na.rm = TRUE)


# joined =
#   ac_long %>%
#   rename(AC = value) %>%
#   inner_join(mims_long %>% rename(MIMS = value), by = c("PAXDAYM", "SEQN", "name"))
#
# joined %>%
#   ggplot(aes(x = AC, y = MIMS)) +
#   geom_point(alpha = .05, size = .2) +
#   geom_smooth(se = FALSE) +
#   geom_abline(intercept = 0, slope = 1)

# 1%         5%        10%        20%        30%
# 0.0000     0.0000     0.0000     0.0000     0.0000
# 40%        50%        60%        70%        80%
# 133.6600   652.0767  1391.0521  2460.8425  3909.6587
# 90%        99%      99.5%
# 5894.5787 11422.5038 13700.6775


ranges =
  ac_long %>%
  mutate(cut_AC = cut(value, breaks = c(0, 100, 500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, Inf),
                      include.lowest = TRUE)) %>%
  group_by(SEQN, PAXDAYM) %>%
  count(cut_AC) %>%
  ungroup() %>%
  pivot_wider(names_from = cut_AC, values_from = n) %>%
  janitor::clean_names() %>%
  rename(SEQN = seqn,
         PAXDAYM = paxdaym) %>%
  mutate(across(starts_with("x"), ~replace_na(.x, 0)))


mvpa_sum =
  ac_long %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(sed_montoye = sum(value <2860, na.rm = TRUE),
            light_montoye = sum(between(value, 2860, 3941), na.rm = TRUE),
            mod_montoye = sum(between(value, 3942, 5613), na.rm = TRUE),
            vig_montoye = sum(value > 5613, na.rm = TRUE),
            mvpa_montoye = sum(value >= 3492, na.rm = TRUE),
            total = sum(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            .groups = "drop")

mvpa_boutsum =
  ac_long %>%
  group_by(SEQN, PAXDAYM) %>%
  # Apply a sliding window over the "value" column to create 10-minute windows
  # mutate(
  #   # Calculate a rolling 10-minute average (assuming 1 observation per minute)
  #   bout_avg = slide_dbl(value, mean, .before = 9, .complete = TRUE)
  # ) %>%
  mutate(
    # Calculate a rolling 10-minute average (assuming 1 observation per minute)
    mvpa_montoye = slide_dbl(value, .f = function(x){sum(x > 3941, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE),
    vig_montoye = slide_dbl(value, .f = function(x){sum(x > 5613, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE)
  ) %>%
  # mutate(
  #   mvpa_montoye = bout_avg > 3941,
  #   vig_montoye = bout_avg > 5613
  # ) %>%
  summarize(across(contains("montoye"), ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rename_at(vars(-c(SEQN, PAXDAYM)), ~str_c(.x, "_bout"))


# mvpa_subj =
#   mvpa_sum %>%
#   group_by(SEQN) %>%
#   summarize(across(c(total, mean, contains("montoye")), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup()
#
# mvpa_boutsubj =
#   mvpa_boutsum %>%
#   group_by(SEQN) %>%
#   summarize(across(contains("montoye"), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   rename_at(vars(-SEQN), ~str_c(.x, "_bout")) %>%
#   ungroup()

mvpa =
  mvpa_sum %>%
  left_join(mvpa_boutsum, by = c("SEQN", "PAXDAYM")) %>%
  left_join(ranges, by = c("SEQN", "PAXDAYM"))

saveRDS(mvpa, here::here("data", "accelerometry", "summarized", "pa_summary_AC.rds"))


rm(list = ls())

mims = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_PAXMTSM.rds"))

days = read_csv(here::here("data", "accelerometry", "inclusion_summary.csv.gz"),
                col_types = list(SEQN = col_character()))

include_days =
  days %>%
  filter(include) %>%
  select(SEQN, PAXDAYM)

mims_filt = mims %>% right_join(include_days, by = c("SEQN", "PAXDAYM"))



mims_long =
  mims_filt %>%
  pivot_longer(cols = starts_with("min"))


quantile(mims_long$value, c(.01, 0.05, seq(.1, .9, .1), .99, .995), na.rm = TRUE)

# 1%     5%    10%    20%    30%    40%    50%    60%
#   0.000  0.000  0.000  0.015  0.191  1.930  5.012  8.946
# 70%    80%    90%    99%  99.5%
# 14.060 20.420 28.674 52.772 64.059

# joined =
#   ac_long %>%
#   rename(AC = value) %>%
#   inner_join(mims_long %>% rename(MIMS = value), by = c("PAXDAYM", "SEQN", "name"))
#
# joined %>%
#   ggplot(aes(x = AC, y = MIMS)) +
#   geom_point(alpha = .05, size = .2) +
#   geom_smooth(se = FALSE) +
#   geom_abline(intercept = 0, slope = 1)

# 1%         5%        10%        20%        30%
# 0.0000     0.0000     0.0000     0.0000     0.0000
# 40%        50%        60%        70%        80%
# 133.6600   652.0767  1391.0521  2460.8425  3909.6587
# 90%        99%      99.5%
# 5894.5787 11422.5038 13700.6775


ranges =
  mims_long %>%
  mutate(cut_PAXMTSM = cut(value, breaks = c(0, 2, 5, 10, 15 ,20, 30, 40, 50, 60, Inf),
                           include.lowest = TRUE)) %>%
  group_by(SEQN, PAXDAYM) %>%
  count(cut_PAXMTSM) %>%
  ungroup() %>%
  pivot_wider(names_from = cut_PAXMTSM, values_from = n) %>%
  janitor::clean_names() %>%
  rename(SEQN = seqn,
         PAXDAYM = paxdaym) %>%
  mutate(across(starts_with("x"), ~replace_na(.x, 0)))

mvpa_sum =
  mims_long %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(total = sum(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            .groups = "drop")



# mvpa_subj =
#   mvpa_sum %>%
#   group_by(SEQN) %>%
#   summarize(across(c(total, mean, contains("montoye")), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup()
#
# mvpa_boutsubj =
#   mvpa_boutsum %>%
#   group_by(SEQN) %>%
#   summarize(across(contains("montoye"), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   rename_at(vars(-SEQN), ~str_c(.x, "_bout")) %>%
#   ungroup()

mvpa =
  mvpa_sum %>%
  left_join(ranges, by = c("SEQN", "PAXDAYM"))




saveRDS(mvpa, here::here("data", "accelerometry", "summarized", "pa_summary_PAXMTSM.rds"))

rm(list = ls())
include_paxy = read_csv(here::here("data", "accelerometry", "inclusion_summary_paxy.csv.gz"))

include_days =
  include_paxy %>%
  filter(include) %>%
  select(SEQN, PAXDAYM) %>%
  mutate(SEQN = as.character(SEQN))

ac_paxy = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_AC_paxy.rds"))

ac_paxyfilt =
  ac_paxy %>%
  mutate(SEQN = as.character(SEQN)) %>%
  right_join(include_days, by = c("SEQN", "PAXDAYM"))

ac_long =
  ac_paxyfilt %>%
  pivot_longer(cols = starts_with("min"))

ranges =
  ac_long %>%
  mutate(cut_AC = cut(value, breaks = c(0, 100, 500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, Inf),
                      include.lowest = TRUE)) %>%
  group_by(SEQN, PAXDAYM) %>%
  count(cut_AC) %>%
  ungroup() %>%
  pivot_wider(names_from = cut_AC, values_from = n) %>%
  janitor::clean_names() %>%
  rename(SEQN = seqn,
         PAXDAYM = paxdaym) %>%
  mutate(across(starts_with("x"), ~replace_na(.x, 0)))



mvpa_sum =
  ac_long %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(sed_montoye = sum(value <2860, na.rm = TRUE),
            light_montoye = sum(between(value, 2860, 3941), na.rm = TRUE),
            mod_montoye = sum(between(value, 3942, 5613), na.rm = TRUE),
            vig_montoye = sum(value > 5613, na.rm = TRUE),
            mvpa_montoye = sum(value >= 3492, na.rm = TRUE),
            total = sum(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            .groups = "drop")

mvpa_boutsum =
  ac_long %>%
  group_by(SEQN, PAXDAYM) %>%
  # Apply a sliding window over the "value" column to create 10-minute windows
  # mutate(
  #   # Calculate a rolling 10-minute average (assuming 1 observation per minute)
  #   bout_avg = slide_dbl(value, mean, .before = 9, .complete = TRUE)
  # ) %>%
  mutate(
    # Calculate a rolling 10-minute average (assuming 1 observation per minute)
    mvpa_montoye = slide_dbl(value, .f = function(x){sum(x > 3941, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE),
    vig_montoye = slide_dbl(value, .f = function(x){sum(x > 5613, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE)
  ) %>%
  # mutate(
  #   mvpa_montoye = bout_avg > 3941,
  #   vig_montoye = bout_avg > 5613
  # ) %>%
  summarize(across(contains("montoye"), ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rename_at(vars(-c(SEQN, PAXDAYM)), ~str_c(.x, "_bout"))


# mvpa_subj =
#   mvpa_sum %>%
#   group_by(SEQN) %>%
#   summarize(across(c(total, mean, contains("montoye")), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup()
#
# mvpa_boutsubj =
#   mvpa_boutsum %>%
#   group_by(SEQN) %>%
#   summarize(across(contains("montoye"), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   rename_at(vars(-SEQN), ~str_c(.x, "_bout")) %>%
#   ungroup()

mvpa =
  mvpa_sum %>%
  left_join(mvpa_boutsum, by = c("SEQN", "PAXDAYM")) %>%
  left_join(ranges, by = c("SEQN", "PAXDAYM"))

saveRDS(mvpa, here::here("data", "accelerometry", "summarized", "pa_summary_AC_paxy.rds"))

