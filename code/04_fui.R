library(tidyverse)
if(!require("fastFMM")) install.packages("fastFMM", dependencies = TRUE)
library(fastFMM)

logac = readRDS(here::here("data", "2011_2014", "accelerometry", "minute_level", "nhanes_1440_LAC.rds"))
# summarize at subject level

inclusion = read_csv(here::here("data", "2011_2014", "accelerometry", "inclusion_summary.csv.gz"),
                     col_types = cols(SEQN = col_character()))

demo = readRDS(here::here("data", "2011_2014", "demographics", "covariates_mortality_G_H_tidy.rds"))
demo_small =
  demo %>%
  select(SEQN, gender, age = age_in_years_at_screening)

include_days =
  inclusion %>%
  filter(include) %>%
  select(SEQN, PAXDAYM)

logac_filt =
  logac %>% right_join(include_days, by = c("SEQN", "PAXDAYM"))

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
  left_join(demo_small, by = "SEQN") %>%
  select(-starts_with("min")) %>%
  mutate(ac = act_mat) %>%
  as.data.frame()

# test_df = dfmat_1440 %>%
#   slice(1:200)

fit_fui = fastFMM::fui(ac ~ age + gender + (1|SEQN),
                           data = dfmat_1440,
                           var = TRUE,
                           argvals = seq(from = 1, to = 1440, by = 10),
                           family = "gaussian",
                           analytic = FALSE,
                           boot_type = "case",
                           num_boots = 500)

saveRDS(fit_fui, here::here("results", "fui_res.rds"))
plot_obj = fastFMM::plot_fui(fit_fui, return = TRUE)

#
#
#
# plot_obj %>%
#   keep(is.data.frame) %>%
#   bind_rows(.id = "var") %>%
#   mutate(sig = CI.lower.pointwise > 0 | CI.upper.pointwise < 0) %>%
#   ggplot(aes(x = s, y = beta.hat, group = 1)) +
#   geom_ribbon(aes(ymin = CI.lower.pointwise, ymax = CI.upper.pointwise),
#               fill = "lightgrey") +
#   geom_line(aes(col = sig), linewidth = .8) +
#   facet_wrap( ~ var, scales = "free_y")
