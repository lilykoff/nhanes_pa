library(tidyverse)
library(haven)
force = TRUE

pax_d = haven::read_xpt(here::here("data",  "2003_2006", "accelerometry", "raw", "paxraw_d.xpt"))

pax_d =
  pax_d %>%
  mutate(across(c(PAXINTEN, PAXSTEP), ~if_else(PAXSTAT != 1, NA_real_, .x))) %>% # if not calibrated or not wearing, repalce with NA
  mutate(min = sprintf((PAXHOUR * 60 + PAXMINUT + 1), fmt = "%04d"))

if(!dir.exists(here::here("data", "2003_2006", "accelerometry", "minute_level"))) {
  dir.create(here::here("data",  "2003_2006","accelerometry", "minute_level"), recursive = TRUE)
}


# var = "PAXINTEN"
# wave = "D"
# df = pax_d
make_longer = function(df, var, wave){
  outname = paste0("nhanes_1440_", wave, "_", var, ".rds")
  if(!file.exists(here::here("data", "2003_2006", "accelerometry", "minute_level", outname)) | force){
    df =
      df %>%
      select(SEQN, PAXDAY, min, v = all_of(var)) %>%
      pivot_wider(names_from = min, values_from = v, names_prefix = "min_")

    write_rds(df, here::here("data",  "2003_2006", "accelerometry", "minute_level", outname), compress = "xz")
  }
}

map(.x = c("PAXINTEN", "PAXCAL", "PAXSTAT", "PAXSTEP"),
    .f = make_longer,
    df = pax_d,
    wave = "D")

# rm(list = ls())

pax_c = haven::read_xpt(here::here("data",  "2003_2006", "accelerometry", "raw", "paxraw_c.xpt"))

pax_c =
  pax_c %>%
  mutate(across(c(PAXINTEN), ~if_else(PAXSTAT != 1, NA_real_, .x))) %>% # if not calibrated or not wearing, repalce with NA
  mutate(min = sprintf((PAXHOUR * 60 + PAXMINUT + 1), fmt = "%04d"))


# var = "PAXINTEN"
# wave = "D"
# df = pax_d


map(.x = c("PAXINTEN", "PAXCAL", "PAXSTAT"),
    .f = make_longer,
    df = pax_c,
    wave = "C")


