# create mvpa data
# For adults and older adolescents (ages 18 or older), intensity thresholds were calculated as a weighted average of criteria determined
# from four studies that based criteria on treadmill or track walking (4,10,14,35).
# The resulting intensity-threshold criteria were 2020 counts for moderate intensity (equivalent to 3 METs) and
# 5999 counts for vigorous intensity (6 METs).
# For youth ages 6-17 yr, the age-specific criteria of the Freedson group, as published by Trost et al. (29),
# were used with thresholds for moderate activity of 4 METs and vigorous activity of 7 METs.

library(tidyverse)
library(survey)
library(slider)
acc = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_C_PAXINTEN.rds"))
acd = readRDS(here::here("data", "accelerometry", "minute_level", "nhanes_1440_D_PAXINTEN.rds"))
ac =
  bind_rows(acc, acd)

days = readRDS(here::here("data", "accelerometry", "inclusion_c_d.rds"))

include_days =
  days %>%
  filter(include_day) %>%
  select(SEQN, PAXDAY)

ac_filt = ac %>% right_join(include_days, by = c("SEQN", "PAXDAY"))

ac_long =
  ac_filt %>%
  pivot_longer(cols = starts_with("min"))


quantile(ac_long$value, c(.01, 0.05, seq(.1, .9, .1), .99, .995), na.rm = TRUE)

# 1%    5%   10%   20%   30%   40%   50%   60%   70%
# 0     0     0     0     0     0     0     3    46
# 80%   90%   99% 99.5%
#   191   613  2974  3914

# ac_long %>%
#   filter(SEQN == 26729) %>%
#   mutate(cut_AC = cut(value,
#                       breaks = c(0, 5, 10, 25, 50, 75, 100, 150, 200, 500, 1000, 2000, 4000, 6000, Inf),
#                       include.lowest = TRUE)) %>%
#   group_by(SEQN, PAXDAY) %>%
#   count(cut_AC) %>%
#   pivot_wider(names_from = cut_AC, values_from = n) %>%
#   janitor::clean_names() %>%
#   rename(SEQN = seqn,
#          PAXDAY = paxday) %>%
#   mutate(across(starts_with("x"), ~replace_na(.x, 0)))

ranges =
  ac_long %>%
  mutate(cut_AC = cut(value,
                      breaks = c(0, 5, 10, 25, 50, 75, 100, 150, 200, 500, 1000, 2000, 4000, 6000, Inf),
                      include.lowest = TRUE)) %>%
  group_by(SEQN, PAXDAY) %>%
  count(cut_AC) %>%
  ungroup() %>%
  pivot_wider(names_from = cut_AC, values_from = n) %>%
  janitor::clean_names() %>%
  rename(SEQN = seqn,
         PAXDAY = paxday) %>%
  mutate(across(starts_with("x"), ~replace_na(.x, 0)))


mvpa_sum =
  ac_long %>%
  group_by(SEQN, PAXDAY) %>%
  summarize(mod = sum(between(value, 2020, 5999), na.rm = TRUE),
            mvpa = sum(value >= 2020, na.rm = TRUE),
            vig = sum(value > 5999, na.rm = TRUE),
            total = sum(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            logtotal = sum(log10(value + 1), na.rm = TRUE),
            logmean = mean(log10(value + 1), na.rm = TRUE),
            logmedian = median(log10(value + 1), na.rm = TRUE),
            .groups = "drop")

mvpa_boutsum =
  ac_long %>%
  group_by(SEQN, PAXDAY) %>%
  # Apply a sliding window over the "value" column to create 10-minute windows
  # mutate(
  #   # Calculate a rolling 10-minute average (assuming 1 observation per minute)
  #   bout_avg = slide_dbl(value, mean, .before = 9, .complete = TRUE)
  # ) %>%
  mutate(
    # Calculate a rolling 10-minute average (assuming 1 observation per minute)
    mvpa = slide_dbl(value, .f = function(x){sum(x >= 2020, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE),
    vig = slide_dbl(value, .f = function(x){sum(x > 5999, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE)
  ) %>%
  # mutate(
  #   mvpa_montoye = bout_avg > 3941,
  #   vig_montoye = bout_avg > 5613
  # ) %>%
  summarize(across(c(mvpa, vig), ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rename_at(vars(-c(SEQN, PAXDAY)), ~str_c(.x, "_bout"))


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
  left_join(mvpa_boutsum, by = c("SEQN", "PAXDAY")) %>%
  left_join(ranges,  by = c("SEQN", "PAXDAY"))

saveRDS(mvpa, here::here("data", "accelerometry", "summarized", "pa_summary_AC_C_D.rds"))



#
# mvpa_sum =
#   ac_filt %>%
#   pivot_longer(cols = starts_with("min")) %>%
#   group_by(SEQN, PAXDAY) %>%
#   summarize(mod = sum(between(value, 2020, 5999), na.rm = TRUE),
#             mvpa = sum(value >= 2020, na.rm = TRUE),
#             vig = sum(value > 5999, na.rm = TRUE),
#             total = sum(value, na.rm = TRUE),
#             mean = mean(value, na.rm = TRUE),
#             range_00_05 = sum(value < 500, na.rm = TRUE),
#             range_05_10 = sum(between(value, 500, 1000), na.rm = TRUE),
#             range_10_15 = sum(between(value, 1001, 1500), na.rm = TRUE),
#             range_15_20 = sum(between(value, 1501, 2000), na.rm = TRUE),
#             range_20_25 = sum(between(value, 2001, 2500), na.rm = TRUE),
#             range_25_30 = sum(between(value, 2501, 3000), na.rm = TRUE),
#             range_30_35 = sum(between(value, 3000, 3500), na.rm = TRUE),
#             range_35_40 = sum(between(value, 3501, 4000), na.rm = TRUE),
#             range_40_45 = sum(between(value, 4001, 4500), na.rm = TRUE),
#             range_45_50 = sum(between(value, 4501, 5000), na.rm = TRUE),
#             range_50_pl = sum(value > 5000, na.rm = TRUE),
#             .groups = "drop")
#
# mvpa_boutsum =
#   ac_filt %>%
#   pivot_longer(cols = starts_with("min")) %>%
#   group_by(SEQN, PAXDAY) %>%
#   # Apply a sliding window over the "value" column to create 10-minute windows
#   # mutate(
#   #   # Calculate a rolling 10-minute average (assuming 1 observation per minute)
#   #   bout_avg = slide_dbl(value, mean, .before = 9, .complete = TRUE)
#   # ) %>%
#   mutate(
#     # Calculate a rolling 10-minute average (assuming 1 observation per minute)
#     mvpa = slide_dbl(value, .f = function(x){sum(x >= 2020, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE),
#     vig = slide_dbl(value, .f = function(x){sum(x > 5999, na.rm = TRUE) >= 8}, .before = 9, .complete = TRUE)
#   ) %>%
#   # mutate(
#   #   mvpa_montoye = bout_avg > 3941,
#   #   vig_montoye = bout_avg > 5613
#   # ) %>%
#   summarize(across(c(mvpa, vig), ~sum(.x, na.rm = TRUE))) %>%
#   ungroup()
#
#
#
# mvpa_subj =
#   mvpa_sum %>%
#   group_by(SEQN) %>%
#   summarize(across(c(total, mean, contains("mvpa"), contains("vig"), contains("mod"), contains("range")), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup()
#
# mvpa_boutsubj =
#   mvpa_boutsum %>%
#   group_by(SEQN) %>%
#   summarize(across(c(mvpa, vig), ~mean(.x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   rename_at(vars(-SEQN), ~str_c(.x, "_bout")) %>%
#   ungroup()
#
# mvpa =
#   mvpa_subj %>%
#   left_join(mvpa_boutsubj, by = "SEQN") %>%
#   mutate(SEQN = as.character(SEQN)) %>%
#   left_join(demo, by = "SEQN")
#
# saveRDS(mvpa, here::here("data", "pa_covariates.rds"))
