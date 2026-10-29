# Download PhysioNet minute-level NHANES 2011-2014 data (AC, MIMS, steps, wear, flags)
# plus covariates, derive valid days, and save minute-level files.
# Mirrors 2003_2006 steps 01-03 (download -> minute_level -> inclusion).
# Outputs (data/2011_2014/):
#   accelerometry/raw/nhanes_1440_*.csv.xz        downloaded files
#   accelerometry/minute_level/nhanes_1440_{AC,LAC,PAXMTSM,scsslsteps,PAXPREDM,PAXFLGSM}.rds
#       all days; nonwear/flagged minutes set to NA for AC, LAC (log(1 + AC)), MIMS, steps
#   accelerometry/inclusion_g_h.rds               day-level wear + include
#   demographics/covariates_accel_mortality_df.rds
# The longer original version (with valid-day-only and MIMS + covariates outputs)
# is in code/archive/00_download_data_original.R.

library(tidyverse)
force = FALSE  # TRUE rebuilds outputs even if they already exist

acc_dir = here::here("data", "2011_2014", "accelerometry")
raw_dir = file.path(acc_dir, "raw")
ml_dir = file.path(acc_dir, "minute_level")
demo_dir = here::here("data", "2011_2014", "demographics")
for (d in c(raw_dir, ml_dir, demo_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# Download (skips files that already exist) -----------------------------------

vars = c("AC", "PAXMTSM", "scsslsteps", "PAXPREDM", "PAXFLGSM")
raw_files = set_names(file.path(raw_dir, paste0("nhanes_1440_", vars, ".csv.xz")), vars)

physionet_url = "https://physionet.org/files/minute-level-step-count-nhanes/1.0.2/csv/"
walk(raw_files, function(f) {
  if (!file.exists(f)) curl::curl_download(paste0(physionet_url, basename(f)), destfile = f)
})

covar_file = file.path(demo_dir, "covariates_accel_mortality_df.rds")
if (!file.exists(covar_file)) {
  curl::curl_download(
    "https://raw.githubusercontent.com/lilykoff/nhanes_steps_mortality/main/data/covariates_accel_mortality_df.rds",
    destfile = covar_file
  )
}

# Process ----------------------------------------------------------------------

inclusion_file = file.path(acc_dir, "inclusion_g_h.rds")
ml_files = file.path(ml_dir, paste0("nhanes_1440_", vars, ".rds"))

if (!all(file.exists(c(inclusion_file, ml_files))) || force) {
  dfs = map(raw_files, read_csv)
  # make log ac file 
  dfs$LAC =  dfs$AC |> 
    mutate(across(starts_with("min"), ~log(1 + .x)))
  
  # all files should have the same subject-days in the same order
  ids = dfs$PAXFLGSM %>% select(SEQN, PAXDAYM)
  walk(dfs, ~ stopifnot(isTRUE(all.equal(select(.x, SEQN, PAXDAYM), ids))))

  get_mat = function(df) df %>% select(starts_with("min")) %>% as.matrix()
  wear_mat = get_mat(dfs$PAXPREDM)  # 1 = wake wear, 2 = sleep wear, 3 = nonwear, 4 = unknown
  flag_mat = get_mat(dfs$PAXFLGSM)  # TRUE = flagged minute

  # valid days: >= 1368 min non-flagged wear, >= 420 min wake wear, >= 420 min with MIMS > 0
  wear_mat_filt = ifelse(flag_mat, NA, wear_mat)
  mims_mat_filt = ifelse(flag_mat, NA, get_mat(dfs$PAXMTSM))

  inclusion =
    ids %>%
    mutate(
      SEQN = as.character(SEQN),
      # %in% drops matrix dims, so rebuild the matrix before rowSums
      non_flag_wear = rowSums(matrix(wear_mat_filt %in% c(1, 2, 4), nrow = nrow(wear_mat_filt))),
      non_zero_MIMS = rowSums(mims_mat_filt > 0, na.rm = TRUE),
      wake_wear = rowSums(wear_mat_filt == 1, na.rm = TRUE),
      include = non_flag_wear >= 1368 & wake_wear >= 420 & non_zero_MIMS >= 420,
    )
  write_rds(inclusion, inclusion_file)

  # minute-level files for all days
  # nonwear and flagged minutes are set to NA
  nonwear_mat = flag_mat | wear_mat == 3
  save_minute_level = function(df, name, mask = TRUE) {
    mat = get_mat(df)
    if (mask) mat = ifelse(nonwear_mat, NA, mat)
    df %>%
      select(-starts_with("min")) %>%
      mutate(SEQN = as.character(SEQN)) %>%
      bind_cols(as_tibble(mat)) %>%
      write_rds(file.path(ml_dir, paste0("nhanes_1440_", name, ".rds")), compress = "xz")
  }
  save_minute_level(dfs$AC, "AC")
  save_minute_level(dfs$PAXMTSM, "PAXMTSM")
  save_minute_level(dfs$scsslsteps, "scsslsteps")
  save_minute_level(dfs$LAC, "LAC")
  save_minute_level(dfs$PAXPREDM, "PAXPREDM", mask = FALSE)
  save_minute_level(dfs$PAXFLGSM, "PAXFLGSM", mask = FALSE)
  
}
