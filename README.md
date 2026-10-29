# nhanes_pa

Sex differences in accelerometer-measured physical activity (PA) across age in NHANES,
using 2003-2006 (ActiGraph counts) and 2011-2014 (ActiGraph counts and MIMS) data.
Analyses include survey-weighted summaries, per-age-group regressions, and
function-on-scalar regression of minute-level activity on age and sex.

All paths use `here::here()`, so open `nhanes_pa.Rproj` (or run from the repo root).
`data/` is gitignored; every file in it is downloaded or created by the scripts below.

## Layout

```
code/
  2003_2006/   pipeline for NHANES 2003-2006 (waves C/D)
  2011_2014/   pipeline for NHANES 2011-2014 (waves G/H)
  functions/   helper functions (survey reweighting, bootstrap FUI)
  utils.R      NHANES download helpers (used by 2003_2006/04)
  archive/     older/exploratory scripts, not maintained (see archive/README.md)
docs/          Quarto reports (.qmd) and their rendered output
data/          downloaded and generated data (not tracked in git)
figures/       PDF figures written by the 2011_2014/03_* scripts
results/       model fits and bootstrap results
```

## 2003-2006 pipeline (`code/2003_2006/`)

Run in order. Outputs are under `data/2003_2006/`.

| Script | Does | Writes |
|---|---|---|
| `01_download_raw_pa_data_2003_2006.R` | Downloads and unzips PAXRAW_C/D from CDC | `accelerometry/raw/paxraw_{c,d}.*` |
| `02_process_raw_pa_data_2003_2006.R` | Reshapes to one row per subject-day, 1440 minute columns; AC set to NA when not calibrated | `accelerometry/minute_level/nhanes_1440_{C,D}_{PAXINTEN,PAXCAL,PAXSTAT}.rds` (+ `D_PAXSTEP`) |
| `03_get_wear_criteria_2003_2006.R` | Nonwear (>= 60 min of zeros) and valid days (> 10 h wear) | `accelerometry/inclusion_c_d.rds` |
| `04_download_survey_data_2003_2006.R` | Downloads and combines questionnaire/exam XPTs | `demographics/raw/`, `demographics/processed/*.csv.gz` |
| `05_create_analytic_survey_data_2003_2006.R` | Cleans and derives covariates | `demographics/processed/subset_C_D_tidy.rds` |
| `06_summarize_pa_data.R` | Day-level AC summaries (wear, TAC, TLAC, mean log AC, sedentary < 100, LiPA 100-2019, MVPA >= 2020), overall and in 2-hr windows; valid day = >= 600 min wear from minute 360 to 1440; then subject-level means over valid days | `accelerometry/summarized/pa_summary_AC_C_D.rds`, `pa_summary_subj_AC_C_D.rds` |
| `07_join_with_covars.R` | Joins subject-level PA with covariates; derives categorical covariates | `covariates_pa_{all,valid}.rds` |

## 2011-2014 pipeline (`code/2011_2014/`)

Run in order. Outputs are under `data/2011_2014/`.

| Script | Does | Writes |
|---|---|---|
| `00_download_data.R` | Downloads PhysioNet minute-level AC, MIMS, steps, wear and flags, plus covariates; derives valid days; saves minute-level files with nonwear/flagged minutes set to NA (AC, LAC, MIMS, steps) | `accelerometry/raw/`, `accelerometry/minute_level/nhanes_1440_{AC,LAC,PAXMTSM,scsslsteps,PAXPREDM,PAXFLGSM}.rds`, `accelerometry/inclusion_g_h.rds`, `demographics/covariates_accel_mortality_df.rds` |
| `01_get_pa_summaries.R` | Day-level AC summaries (wear, TAC, TLAC, Montoye sedentary/light/moderate/vigorous minutes) and MIMS totals, overall and in 2-hr windows; then subject-level means over valid days | `accelerometry/summarized/pa_summary_AC_MIMS_G_H.rds`, `pa_summary_subj_AC_MIMS_G_H.rds` |
| `02_join_with_covars.R` | Joins subject-level PA with covariates; derives categorical covariates | `covariates_pa_{all,valid}.rds` |
| `03_regressions.R` | Survey-weighted regressions of PA on sex by age group (models M1-M4), whole day and by 2-hr window | `figures/linear_regressions*.pdf` |
| `03_distributions.R` | Distributions by sex and age; time-of-day regressions | `figures/distributions*.pdf` |

Valid day (2011-2014): >= 1368 min non-flagged wear, >= 420 min wake wear and >= 420 min with MIMS > 0.

## Output names: 2003-2006 vs 2011-2014

Paths are relative to `data/2003_2006/` or `data/2011_2014/`.

| | 2003-2006 (waves C/D) | 2011-2014 (waves G/H) |
|---|---|---|
| Raw downloads | `accelerometry/raw/paxraw_{c,d}.*` | `accelerometry/raw/nhanes_1440_*.csv.xz` |
| Minute level | `accelerometry/minute_level/nhanes_1440_{C,D}_PAXINTEN.rds`, ... | `accelerometry/minute_level/nhanes_1440_AC.rds`, `_LAC`, `_PAXMTSM`, ... |
| Valid days | `accelerometry/inclusion_c_d.rds` | `accelerometry/inclusion_g_h.rds` |
| Day-level PA | `accelerometry/summarized/pa_summary_AC_C_D.rds` | `accelerometry/summarized/pa_summary_AC_MIMS_G_H.rds` |
| Subject-level PA | `accelerometry/summarized/pa_summary_subj_AC_C_D.rds` | `accelerometry/summarized/pa_summary_subj_AC_MIMS_G_H.rds` |
| Covariates | `demographics/processed/subset_C_D_tidy.rds` | `demographics/covariates_accel_mortality_df.rds` |
| PA + covariates | `covariates_pa_{all,valid}.rds` | `covariates_pa_{all,valid}.rds` |

Day-level files keep all days, with a column marking valid days (`include_day` for 2003-2006,
`include` for 2011-2014). Subject-level files average over valid days and include `n_good_days`.
2011-2014 minute-level files combine both waves (PhysioNet ships them combined), so the wave letter
isn't in the file name. Minute columns are `min_0001` to `min_1440` in both.

## Reports (`docs/`)

- `sex_differences.qmd`: survey-weighted PA by sex and age, NHANES 2003-2006.
- `sex_differences2.qmd`: extended version covering 2003-2006, 2011-2014 (AC and MIMS) and NNYFS.

`*_cache/` and `*_files/` folders are build output and are gitignored.

## Known issues

- `03_regressions.R` and `03_distributions.R` still use the 10-min bout variables
  (`mvpa_10min_bout`, `vpa_10min_bout`), and `03_regressions.R` also uses the wake-only variables
  (`wake_*`). `01_get_pa_summaries.R` no longer adds those to its outputs, so those parts will error.
- `03_distributions.R` calls `run_models()`, defined in `03_regressions.R`. Run that first in the same session.
- In 2003-2006, `06_summarize_pa_data.R` sets its own valid-day rule; `inclusion_c_d.rds` (step 03) is
  only used in `07` to flag who has accelerometry.
- The reports in `docs/` still read from an older data layout (`data/accelerometry/...`).
- `archive/test.R` sources `code/2011_2014/prepare_data_functional.R`, which now lives in `archive/`.

## Packages

tidyverse, here, survey, svrep, svylme, broom, slider, fastFMM, tidyfun, refund, haven,
nhanesA, sjlabelled, janitor, gtsummary, gt, paletteer, patchwork, DescTools, curl.
