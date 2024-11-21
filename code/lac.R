library(tidyverse)

ac = readRDS(here::here("data", "2011_2014", "accelerometry", "minute_level", "nhanes_1440_AC.rds"))
lac = ac %>%
  mutate(across(starts_with("min"), ~log(1 + .x)))

write_rds(lac, here::here("data", "2011_2014", "accelerometry", "minute_level", "nhanes_1440_LAC.rds"),
          compress = "xz")
