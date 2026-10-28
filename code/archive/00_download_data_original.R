# Download PhysioNet minute-level NHANES 2011-2014 data (AC, MIMS, steps, wear, flags)
# plus covariates, derive valid days, and save minute-level files.
# Mirrors 2003_2006 steps 01-03 (download -> minute_level -> inclusion).
# Outputs (data/2011_2014/):
#   accelerometry/raw/nhanes_1440_*.csv.xz                  downloaded files
#   accelerometry/minute_level/nhanes_1440_{AC,PAXMTSM,scsslsteps,PAXPREDM,PAXFLGSM}.rds
#       all days; nonwear/flagged minutes set to NA for AC, MIMS, steps
#   accelerometry/inclusion_g_h.rds                          day-level wear + include_day
#   accelerometry/minute_level/nhanes_1440_{PAXMTSM,scsslsteps}_valid.rds
#       valid days only (subjects with >= 3 valid days)
#   accelerometry/valid_days_per_subj.rds, mims_covariates.rds
# Adapted from lilykoff/nhanes_steps_mortality.

### process nhanes data to get minute-level steps per subject

library(tidyverse)
force = FALSE

acc_dir = here::here("data", "2011_2014", "accelerometry")
raw_dir = file.path(acc_dir, "raw")
ml_dir = file.path(acc_dir, "minute_level")
demo_dir = here::here("data", "2011_2014", "demographics")
for (d in c(raw_dir, ml_dir, demo_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# check if data is downloaded
data_files = file.path(raw_dir, c("nhanes_1440_scsslsteps.csv.xz",
                                  "nhanes_1440_PAXPREDM.csv.xz",
                                  "nhanes_1440_PAXFLGSM.csv.xz",
                                  "nhanes_1440_PAXMTSM.csv.xz",
                                  "nhanes_1440_AC.csv.xz"))


# fn to check if files exist and download
check_exists_download_file = function(f){
  # f = data_files[1]
  if (!file.exists(f)){
    url = paste0(
      "https://physionet.org/files/minute-level-step-count-nhanes/1.0.2/csv/",
      basename(f)
    )
    curl::curl_download(url, destfile = f)
  }
}

# download covariates file too (from GH)
covar_file = file.path(demo_dir, "covariates_accel_mortality_df.rds")

if (!file.exists(covar_file)){
  curl::curl_download(
    "https://raw.githubusercontent.com/lilykoff/nhanes_steps_mortality/main/data/covariates_accel_mortality_df.rds",
    destfile = covar_file
  )
}


# download files
if (!all(file.exists(data_files))) {
  lapply(data_files, check_exists_download_file)
}

# target files
mims_ml = file.path(ml_dir, "nhanes_1440_PAXMTSM_valid.rds")
mims_sl = here::here("data", "2011_2014", "mims_covariates.rds")
inclusion_file = file.path(acc_dir, "inclusion_g_h.rds")

if (!all(file.exists(c(mims_ml, mims_sl, inclusion_file))) || force) {
  if (!all(file.exists(c(mims_ml, inclusion_file))) || force) {
    steps = read_csv(file.path(raw_dir, "nhanes_1440_scsslsteps.csv.xz"))
    wear = read_csv(file.path(raw_dir, "nhanes_1440_PAXPREDM.csv.xz"))
    flags = read_csv(file.path(raw_dir, "nhanes_1440_PAXFLGSM.csv.xz"))
    mims = read_csv(file.path(raw_dir, "nhanes_1440_PAXMTSM.csv.xz"))
    ac = read_csv(file.path(raw_dir, "nhanes_1440_AC.csv.xz"))

    all.equal(wear %>% select(SEQN, PAXDAYM), flags %>% select(SEQN, PAXDAYM))
    all.equal(mims %>% select(SEQN, PAXDAYM), flags %>% select(SEQN, PAXDAYM))
    all.equal(steps %>% select(SEQN, PAXDAYM), flags %>% select(SEQN, PAXDAYM))
    all.equal(ac %>% select(SEQN, PAXDAYM), flags %>% select(SEQN, PAXDAYM))


    mims_mat = mims %>% select(starts_with("min")) %>% as.matrix()
    wear_mat = wear %>% select(starts_with("min")) %>% as.matrix()
    flag_mat = flags %>% select(starts_with("min")) %>% as.matrix()
    ## replace entires in wear mat with NA if flag is TRUE
    wear_mat_filt = ifelse(flag_mat, NA, wear_mat)
    ## replace entries in mims mat with NA if flag is TRUE
    mims_mat_filt = ifelse(flag_mat, NA, mims_mat)

    # create logical matrix where wear is 1, 2, or 4
    wear_mat_lgl = matrix(wear_mat_filt %in% c(1, 2, 4),
                          nrow = nrow(wear_mat_filt), ncol = ncol(wear_mat_filt))
    # create logical matrix where true if MIMS > 0
    mims_mat_lgl = matrix(mims_mat_filt > 0,
                          nrow = nrow(mims_mat_filt), ncol = ncol(mims_mat_filt))

    wake_mat_lgl = matrix(wear_mat_filt == 1,
                          nrow = nrow(wear_mat_filt), ncol = ncol(wear_mat_filt))

    # use rowsums to quickly get total number of minute per day with wear
    wear_summary_vec = rowSums(wear_mat_lgl, na.rm = TRUE)
    # use rowsums to get total number of minutes with nonzero MIMS
    mims_summary_vec = rowSums(mims_mat_lgl, na.rm = TRUE)
    # same for wake
    wake_summary_vec = rowSums(wake_mat_lgl, na.rm = TRUE)

    # get wear summary
    wear_summary =
      wear %>%
      select(SEQN, PAXDAYM) %>%
      bind_cols(non_flag_wear = wear_summary_vec) %>%
      bind_cols(non_zero_MIMS = mims_summary_vec) %>%
      bind_cols(wake_wear = wake_summary_vec) %>%
      mutate(include = non_flag_wear >= 1368 & wake_wear >= 420 & non_zero_MIMS >= 420)

    # day-level inclusion file (analogous to 2003_2006 inclusion_c_d.rds)
    wear_summary %>%
      mutate(SEQN = as.character(SEQN), include_day = include) %>%
      write_rds(inclusion_file)

    # minute-level files for all days (analogous to 2003_2006 step 02);
    # nonwear (PAXPREDM == 3) and flagged minutes are set to NA
    nonwear_mat = flag_mat | wear_mat == 3
    save_minute_level = function(df, name, mask = TRUE) {
      mat = df %>% select(starts_with("min")) %>% as.matrix()
      if (mask) mat = ifelse(nonwear_mat, NA, mat)
      df %>%
        select(-starts_with("min")) %>%
        mutate(SEQN = as.character(SEQN)) %>%
        bind_cols(as_tibble(mat)) %>%
        write_rds(file.path(ml_dir, paste0("nhanes_1440_", name, ".rds")), compress = "xz")
    }
    save_minute_level(ac, "AC")
    save_minute_level(mims, "PAXMTSM")
    save_minute_level(steps, "scsslsteps")
    save_minute_level(wear, "PAXPREDM", mask = FALSE)
    save_minute_level(flags, "PAXFLGSM", mask = FALSE)


    include_days =
      wear_summary %>%
      group_by(SEQN) %>%
      mutate(days_per_sub = sum(include)) %>%
      ungroup() %>%
      mutate(include_final = days_per_sub >= 3 & include) %>%
      filter(include_final) %>%
      select(SEQN, PAXDAYM)

    steps_small =
      steps %>%
      right_join(include_days, by = c("SEQN", "PAXDAYM"))

    mims_small =
      mims %>%
      right_join(include_days, by = c("SEQN", "PAXDAYM"))

    wear_small =
      wear %>%
      right_join(include_days, by = c("SEQN", "PAXDAYM"))

    flags_small =
      flags %>%
      right_join(include_days, by = c("SEQN", "PAXDAYM"))

    # get rid of steps minutes that are nonwear OR flagged
    wear_mat = wear_small %>% select(starts_with("min")) %>% as.matrix()
    flag_mat = flags_small %>% select(starts_with("min")) %>% as.matrix()
    mims_mat = mims_small %>% select(starts_with("min")) %>% as.matrix()
    steps_mat = steps_small %>% select(starts_with("min")) %>% as.matrix()

    wear_mat_lgl = matrix(wear_mat == 3,
                          nrow = nrow(wear_mat), ncol = ncol(wear_mat))
    ## replace entires in wear mat with NA if flag is TRUE
    steps_mat_filt = ifelse(wear_mat_lgl, NA, steps_mat)
    ## replace entries in mims mat with NA if flag is TRUE
    steps_mat_filt2 = ifelse(flag_mat, NA, steps_mat_filt)

    mims_mat_filt = ifelse(wear_mat_lgl, NA, mims_mat)
    mims_mat_filt2 = ifelse(flag_mat, NA, mims_mat_filt)

    steps_final =
      steps_small %>%
      select(-starts_with("min")) %>%
      bind_cols(steps_mat_filt2)

    mims_final =
      mims_small %>%
      select(-starts_with("min")) %>%
      bind_cols(mims_mat_filt2)


    write_rds(steps_final, file.path(ml_dir, "nhanes_1440_scsslsteps_valid.rds"), compress = "xz")
    write_rds(mims_final, mims_ml, compress = "xz")

    mims_persub =
      mims_final %>%
      group_by(SEQN) %>%
      summarize(across(starts_with("min"), ~mean(.x, na.rm = TRUE)), .groups = "drop")

    steps_persub =
      steps_final %>%
      group_by(SEQN) %>%
      summarize(across(starts_with("min"), ~mean(.x, na.rm = TRUE)), .groups = "drop")

  } else {
    mims_final = read_rds(mims_ml)
  }

  covars = read_rds(covar_file)

  mims_persub =
    mims_final %>%
    group_by(SEQN) %>%
    summarize(across(starts_with("min"), ~mean(.x, na.rm = TRUE)), .groups = "drop")


  pa_df_persub =
    mims_persub %>% mutate(SEQN = as.character(SEQN)) %>%
    left_join(covars, by = "SEQN") %>%
    rename(psu = masked_variance_pseudo_psu,
           strata = masked_variance_pseudo_stratum) %>%
    select(SEQN, data_release_cycle, psu, strata,
           gender, age_in_years_at_screening, full_sample_2_year_interview_weight,
           full_sample_2_year_mec_exam_weight, race_hispanic_origin,
           cat_bmi, starts_with("min_"))

  write_rds(pa_df_persub, mims_sl, compress = "xz")
}


## write out days per subj
mims_ml = read_rds(mims_ml)

n_days =
  mims_ml |>
  group_by(SEQN) |>
  summarize(n_days = n()) |>
  mutate(SEQN = as.character(SEQN)) |>
  ungroup()


write_rds(n_days, file.path(acc_dir, "valid_days_per_subj.rds"))
