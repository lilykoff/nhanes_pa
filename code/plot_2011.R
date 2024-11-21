library(tidyverse)
col1 = "#CC5151"; col2 = "#422CB2"

together =
  readRDS(here::here("data", "accelerometry", "summarized", "ac_summary.rds"))

together_sum =
  together %>%
  group_by(SEQN) %>%
  summarize(across(-c(PAXDAYM), ~median(.x, na.rm = TRUE)),
            .groups = "drop")


covariates =
  readRDS(here::here("data", "demographics", "covariates_accel_mortality_df.rds"))

vars = c("wear", "tlac", "tac", "mlac", "st", "lipa", "mvpa")

together_sum %>%
  left_join(covariates, by = "SEQN") %>%
  filter(!is.na(age_in_years_at_screening)) %>%
  mutate(age_cat = cut(age_in_years_at_screening, breaks = c(0, 18, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("0-18", "18-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))) %>%
  group_by(age_cat, gender) %>%
  summarize(across(all_of(vars), ~mean(.x))) %>%
  pivot_wider(names_from = gender, values_from = all_of(vars))


strings = together_sum %>% select(starts_with("wear")) %>% colnames() %>% sub(".*wear\\_", "", .)

together_sum %>%
  left_join(covariates, by = "SEQN") %>%
  filter(age_in_years_at_screening >= 18) %>%
  select(all_of(vars), age = age_in_years_at_screening, gender) %>%
  pivot_longer(cols = all_of(vars)) %>%
  mutate(name = factor(name, labels = c("LIPA (min)", "Mean Log AC", "MVPA (min)", "Sedentary time (min)","Total AC (TAC)",  "Total Log AC (TLAC)", "Wear time (min)"))) %>%
  ggplot(aes(x = age, y = value, color = gender)) +
  geom_jitter(alpha = .1, size = .7, width = .25) +
  geom_smooth(se = F) +
  facet_wrap(name~., scales = "free_y") +
  labs(x = "Age", y = "Value", color = "", title = "NHANES 2011-2014")+
  scale_color_manual(values = c(col1, col2)) +
  theme(legend.position = c(0.9, 0.2),
        legend.title = element_blank())

together_sum %>%
  left_join(covariates, by = "SEQN") %>%
  filter(age_in_years_at_screening >= 18) %>%
  select(all_of(vars), age = age_in_years_at_screening, gender, data_release_cycle) %>%
  pivot_longer(cols = all_of(vars)) %>%
  mutate(name = factor(name, labels = c("LIPA (min)", "Mean Log AC", "MVPA (min)", "Sedentary time (min)","Total AC (TAC)",  "Total Log AC (TLAC)", "Wear time (min)"))) %>%
  ggplot(aes(x = age, y = value, color = gender)) +
  geom_jitter(alpha = .1, size = .7, width = .25) +
  geom_smooth(se = F) +
  facet_grid(name ~ data_release_cycle, scales = "free_y") +
  labs(x = "Age", y = "Value", color = "", title = "NHANES 2011-2014")+
  scale_color_manual(values = c(col1, col2)) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pdf(here::here("figures", "two_hour_chunks_2011.pdf"))

for(string in strings[1:12]){
  p = together_sum %>%
    left_join(covariates, by = "SEQN") %>%
    filter(!is.na(age_in_years_at_screening)) %>%
    mutate(age_cat = cut(age_in_years_at_screening, breaks = c(0, 18, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("0-18", "18-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))) %>%
    group_by(age_cat, gender) %>%
    summarize(across(ends_with(string), ~mean(.x, na.rm = TRUE))) %>%
    pivot_wider(names_from = gender, values_from = ends_with(string))
  # print(p)


  p = together_sum %>%
    left_join(covariates, by = "SEQN") %>%
    filter(age_in_years_at_screening >= 18) %>%
    select(ends_with(string), age = age_in_years_at_screening, gender) %>%
    pivot_longer(cols = contains(string)) %>%
    mutate(name = factor(name, labels = c("LIPA (min)", "Mean Log AC", "MVPA (min)", "Sedentary time (min)", "Total AC (TAC)", "Total Log AC (TLAC)", "Wear time (min)"))) %>%
    ggplot(aes(x = age, y= value, color = gender)) +
    geom_jitter(alpha = .1, size = .7, width = .25) +
    geom_smooth(se = F) +
    facet_wrap(name~., scales = "free_y") +
    labs(x = "Age", y = "Value", color = "", title = paste0("Hours: ", string))+
    scale_color_manual(values = c(col1, col2)) +
    theme(legend.position = c(0.9, 0.2),
          legend.title = element_blank())
  print(p)
}
dev.off()

