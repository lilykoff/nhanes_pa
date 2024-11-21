# plot distributions
# load data

library(tidyverse)
ac1440 = readRDS(here::here("data", "lily", "data", "nhanes_1440_AC.rds"))
mims1440 = readRDS(here::here("data", "lily", "data", "nhanes_1440_PAXMTSM.rds"))
ac1440_c = readRDS(here::here("data", "lily", "data", "nhanes_1440_C_PAXINTEN.rds"))
ac1440_d = readRDS(here::here("data", "lily", "data", "nhanes_1440_D_PAXINTEN.rds"))
ac1440_cd = ac1440_c %>% bind_rows(ac1440_d)


pdf(here::here("data", "lily", "data", "distribution_plots.pdf"))
ac1440 %>%
  pivot_longer(cols = starts_with("min")) %>%
  ggplot(aes(x = value)) +
  geom_histogram(col = "black") +
  labs(x = "Minute level AC", y = "Count", title = "AC: 2011-2014")


ac1440 %>%
  pivot_longer(cols = starts_with("min")) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  labs(x = "Minute level AC", y = "Density", title = "AC: 2011-2014")


ac1440 %>%
  pivot_longer(cols = starts_with("min")) %>%
  group_by(SEQN, PAXDAYM) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(col = "black") +
  labs(x = "Minute level AC", y = "Count", title = "AC: 2011-2014")


mims1440 %>%
  pivot_longer(cols = starts_with("min")) %>%
  ggplot(aes(x = value)) +
  geom_histogram(col = "black") +
  labs(x = "Minute level MIMS", y = "Count", title = "MIMS: 2011-2014")

mims1440 %>%
  pivot_longer(cols = starts_with("min")) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  labs(x = "Minute level MIMS", y = "Density", title = "MIMS: 2011-2014")

ac1440_cd %>%
  pivot_longer(cols = starts_with("min")) %>%
  ggplot(aes(x = value)) +
  geom_histogram(col = "black") +
  labs(x = "Minute level Counts", y = "Count", title = "AC: 2003-2006")

ac1440_cd %>%
  pivot_longer(cols = starts_with("min")) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  labs(x = "Minute level Counts", y = "Density", title = "AC: 2003-2006")

dev.off()

