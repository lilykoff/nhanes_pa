rm(list = ls())

library(tidyverse)

load(here::here("data", "accelerometry", "rnhanes_data", "Covariate_D.rda"))
load(here::here("data", "accelerometry", "rnhanes_data", "Covariate_C.rda"))

together =
  readRDS( here::here("data", "accelerometry", "summarized", "paxinten_summary.rds"))

together_sum =
  together %>%
  group_by(SEQN) %>%
  summarize(across(-c(PAXDAY,PAXCAL,  PAXSTAT), ~median(.x, na.rm = TRUE)),
            .groups = "drop")


covariates =
  bind_rows(Covariate_C, Covariate_D)

vars = c("wear", "tlac", "tac", "mlac", "st", "lipa", "mvpa")


together_sum %>%
  left_join(covariates, by = "SEQN") %>%
  filter(!is.na(RIDAGEYR)) %>%
  mutate(age_cat = cut(RIDAGEYR, breaks = c(0, 18, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("0-18", "18-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))) %>%
  group_by(age_cat, Gender) %>%
  summarize(across(all_of(vars), ~mean(.x))) %>%
  pivot_wider(names_from = Gender, values_from = c(wear, tac, tlac, mlac, st, mvpa))


strings = together_sum %>% select(starts_with("wear")) %>% colnames() %>% sub(".*wear\\_", "", .)
col1 = "#CC5151"; col2 = "#422CB2"

together_sum %>%
  left_join(covariates, by = "SEQN") %>%
  filter(RIDAGEYR >= 18) %>%
  select(all_of(vars), age = RIDAGEYR, Gender) %>%
  pivot_longer(cols = all_of(vars)) %>%
  mutate(name = factor(name, labels = c("LIPA (min)", "Mean Log AC", "MVPA (min)", "Sedentary time (min)","Total AC (TAC)",  "Total Log AC (TLAC)", "Wear time (min)"))) %>%
  ggplot(aes(x = age, y = value, color = Gender)) +
  geom_jitter(alpha = .1, size = .7, width = .25) +
  geom_smooth(se = F) +
  facet_wrap(name~., scales = "free_y") +
  labs(x = "Age", y = "Value", color = "")+
  scale_color_manual(values = c(col1, col2)) +
  theme(legend.position = c(0.9, 0.2),
        legend.title = element_blank())

together_sum %>%
  left_join(covariates, by = "SEQN") %>%
  filter(RIDAGEYR >= 18) %>%
  select(all_of(vars), age = RIDAGEYR, Gender, SDDSRVYR) %>%
  pivot_longer(cols = all_of(vars)) %>%
  mutate(name = factor(name, labels = c("LIPA (min)", "Mean Log AC", "MVPA (min)", "Sedentary time (min)","Total AC (TAC)",  "Total Log AC (TLAC)", "Wear time (min)")),
         Gender = factor(Gender, levels = c("Female", "Male"))) %>%
  ggplot(aes(x = age, y = value, color = Gender)) +
  geom_jitter(alpha = .1, size = .7, width = .25) +
  geom_smooth(se = F) +
  facet_grid(name ~ SDDSRVYR, scales = "free_y") +
  labs(x = "Age", y = "Value", color = "", title = "NHANES 2003-2006")+
  scale_color_manual(values = c(col1, col2)) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pdf(here::here("figures", "two_hour_chunks_2003.pdf"))
for(string in strings[1:12]){
  # p = together_sum %>%
  #   left_join(covariates, by = "SEQN") %>%
  #   filter(!is.na(RIDAGEYR)) %>%
  #   mutate(age_cat = cut(RIDAGEYR, breaks = c(0, 18, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("0-18", "18-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))) %>%
  #   group_by(age_cat, Gender) %>%
  #   summarize(across(ends_with(string), ~mean(.x, na.rm = TRUE))) %>%
  #   pivot_wider(names_from = Gender, values_from = ends_with(string))
  # print(p)


  p = together_sum %>%
    left_join(covariates, by = "SEQN") %>%
    filter(RIDAGEYR >= 18) %>%
    select(ends_with(string), age = RIDAGEYR, Gender) %>%
    pivot_longer(cols = contains(string)) %>%
    mutate(name = factor(name, labels = c("LIPA (min)", "Mean Log AC", "MVPA (min)", "Sedentary time (min)","Total AC (TAC)",  "Total Log AC (TLAC)", "Wear time (min)")),
           Gender = factor(Gender, levels = c("Female", "Male"))) %>%
    ggplot(aes(x = age, y= value, color = Gender)) +
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
