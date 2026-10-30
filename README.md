# nhanes_pa

Sex differences in accelerometer-measured physical activity (PA) across age in NHANES,
using 2003-2006 (ActiGraph counts) and 2011-2014 (ActiGraph counts and MIMS) data.
The pipelines download and process the data into day- and subject-level PA summaries
joined with covariates. Analyses are in the reports (`docs/`); earlier regression and
functional (FUI) analyses are in `code/archive/`.

All paths use `here::here()`, so open `nhanes_pa.Rproj` (or run from the repo root).
`data/` is gitignored; every file in it is downloaded or created by the scripts below.

## Layout

```
code/
  2003_2006/     pipeline for NHANES 2003-2006 (waves C/D)
  2011_2014/     pipeline for NHANES 2011-2014 (waves G/H)
  utils.R        NHANES download helpers (used by 2003_2006/03)
  rewt_accel.R   reweight_accel(): survey weights adjusted for the accelerometry subsample
  archive/       older/exploratory scripts, not maintained (see archive/README.md)
docs/            Quarto reports (.qmd) and their rendered output
data/            downloaded and generated data (not tracked in git)
figures/         output folder for figures
```

## 2003-2006 pipeline (`code/2003_2006/`)

Run in order. Outputs are under `data/2003_2006/`.

| Script | Does | Writes |
|---|---|---|
| `01_download_raw_pa_data_2003_2006.R` | Downloads and unzips PAXRAW_C/D from CDC | `accelerometry/raw/paxraw_{c,d}.*` |
| `02_process_raw_pa_data_2003_2006.R` | Reshapes to one row per subject-day, 1440 minute columns; AC set to NA when not calibrated | `accelerometry/minute_level/nhanes_1440_{C,D}_{PAXINTEN,PAXCAL,PAXSTAT}.rds` (+ `D_PAXSTEP`) |
| `03_download_survey_data_2003_2006.R` | Downloads and combines questionnaire/exam XPTs | `demographics/raw/`, `demographics/processed/*.csv.gz` |
| `04_create_analytic_survey_data_2003_2006.R` | Cleans and derives covariates | `demographics/processed/subset_C_D_tidy.rds` |
| `05_summarize_pa_data.R` | Day-level AC summaries (TAC, TLAC, mean log AC, sedentary < 100, LiPA 101-2019, MVPA >= 2020), overall and in 2-hr windows; derives valid days; then subject-level means over valid days for subjects with >= 3 valid days | `accelerometry/summarized/pa_summary_AC_C_D.rds`, `pa_summary_subj_AC_C_D.rds` |
| `06_join_with_covars.R` | Joins subject-level PA with covariates; derives categorical covariates | `covariates_pa_{all,valid}.rds` |

Valid day (2003-2006): >= 600 min wear between minutes 360 and 1320 (6am-10pm). Nonwear is
60+ consecutive minutes of zero counts, allowing 1-2 min of counts between 1 and 99. Stored as
`wear_6a_10p`, `include_day` and `n_good_days` in `pa_summary_AC_C_D.rds`.

## 2011-2014 pipeline (`code/2011_2014/`)

Run in order. Outputs are under `data/2011_2014/`.

| Script | Does | Writes |
|---|---|---|
| `00_download_data.R` | Downloads PhysioNet minute-level AC, MIMS, steps, wear and flags, plus covariates; derives valid days; saves minute-level files with nonwear/flagged minutes set to NA (AC, LAC, MIMS, steps) | `accelerometry/raw/`, `accelerometry/minute_level/nhanes_1440_{AC,LAC,PAXMTSM,scsslsteps,PAXPREDM,PAXFLGSM}.rds`, `accelerometry/inclusion_g_h.rds`, `demographics/covariates_accel_mortality_df.rds` |
| `01_get_pa_summaries.R` | Day-level AC summaries (wear, TAC, TLAC, Montoye sedentary/light/moderate/vigorous minutes) and MIMS totals, overall and in 2-hr windows; then subject-level means over valid days | `accelerometry/summarized/pa_summary_AC_MIMS_G_H.rds`, `pa_summary_subj_AC_MIMS_G_H.rds` |
| `02_join_with_covars.R` | Joins subject-level PA with covariates; derives categorical covariates | `covariates_pa_{all,valid}.rds` |

Valid day (2011-2014): >= 1368 min non-flagged wear, >= 420 min wake wear and >= 420 min with MIMS > 0.

## Output names: 2003-2006 vs 2011-2014

Paths are relative to `data/2003_2006/` or `data/2011_2014/`.

| | 2003-2006 (waves C/D) | 2011-2014 (waves G/H) |
|---|---|---|
| Raw downloads | `accelerometry/raw/paxraw_{c,d}.*` | `accelerometry/raw/nhanes_1440_*.csv.xz` |
| Minute level | `accelerometry/minute_level/nhanes_1440_{C,D}_PAXINTEN.rds`, ... | `accelerometry/minute_level/nhanes_1440_AC.rds`, `_LAC`, `_PAXMTSM`, ... |
| Valid days | `include_day` column in `accelerometry/summarized/pa_summary_AC_C_D.rds` | `accelerometry/inclusion_g_h.rds` (also joined into `pa_summary_AC_MIMS_G_H.rds`) |
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

`*_cache/` and `*_files/` folders are build output and are gitignored. The reports still read
from an older data layout (`data/accelerometry/...`).

## Packages

Pipelines: tidyverse, here, slider, haven, nhanesA, sjlabelled, janitor, curl.
Reports: survey, gtsummary, gt, tidymodels, censored, paletteer, patchwork.
