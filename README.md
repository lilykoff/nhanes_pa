# nhanes_pa

Sex differences in accelerometer-measured physical activity (PA) across age in NHANES,
using 2003-2006 (ActiGraph counts) and 2011-2014 (ActiGraph counts and MIMS) data.
Analyses include survey-weighted summaries, per-age-group regressions, and
function-on-scalar regression of minute-level activity on age and sex.

All paths use `here::here()`, so open `nhanes_pa.Rproj` (or run from the repo root).

## Layout

```
code/
  2011_2014/   main pipeline for NHANES 2011-2014 (waves G/H)
  2003_2006/   pipeline for NHANES 2003-2006 (waves C/D)
  functions/   helper functions sourced by other scripts
  utils.R      NHANES download helpers (used by 2003_2006/04)
  archive/     older/exploratory scripts, not maintained (see archive/README.md)
docs/          Quarto reports (.qmd) and their rendered output
data/          inputs and intermediate files (mostly not tracked in git)
figures/       PDF figures written by the 03_* scripts
results/       model fits and bootstrap results
```

## 2011-2014 pipeline (`code/2011_2014/`)

Run in order:

| Script | Does | Writes (under `data/2011_2014/`) |
|---|---|---|
| `00_download_data.R` | Downloads PhysioNet minute-level AC/MIMS/steps/wear/flags and covariates; saves minute-level files; derives valid days | `accelerometry/raw/`, `accelerometry/minute_level/nhanes_1440_*.rds`, `accelerometry/inclusion_g_h.rds` |
| `01_get_pa_summaries.R` | Log AC; day- and subject-level AC/MIMS summaries (Montoye thresholds, bouts, 2-hr windows, wake-only) | `accelerometry/summarized/pa_summary_AC_MIMS_G_H.rds`, `pa_summary_subj_AC_MIMS_G_H.rds` |
| `02_join_with_covars.R` | Joins PA with covariates; derives categorical covariates | `covariates_pa_{all,valid}.rds` |
| `03_regressions.R` | Survey-weighted regressions of PA on sex by age group (models M1-M4) | `figures/linear_regressions*.pdf` |
| `03_distributions.R` | Distributions by sex/age; time-of-day regressions | `figures/distributions*.pdf` |

`covariates_mortality_G_H_tidy.rds` (used by 02) is included in `data/2011_2014/demographics/`.

### Output names: 2003-2006 vs 2011-2014

| | 2003-2006 (waves C/D) | 2011-2014 (waves G/H) |
|---|---|---|
| Raw downloads | `accelerometry/raw/paxraw_{c,d}.*` | `accelerometry/raw/nhanes_1440_*.csv.xz` |
| Minute level | `accelerometry/minute_level/nhanes_1440_{C,D}_PAXINTEN.rds`, ... | `accelerometry/minute_level/nhanes_1440_AC.rds`, `_PAXMTSM`, `_scsslsteps`, `_PAXPREDM`, `_PAXFLGSM` |
| Valid days | `accelerometry/inclusion_c_d.rds` (`include_day`) | `accelerometry/inclusion_g_h.rds` (`include_day`, also `include`) |
| Day-level PA | `accelerometry/summarized/pa_summary_AC_C_D.rds` | `accelerometry/summarized/pa_summary_AC_MIMS_G_H.rds` |
| Covariates | `demographics/processed/subset_C_D_tidy.rds` | `demographics/covariates_mortality_G_H_tidy.rds` |

2011-2014 minute-level files combine both waves (PhysioNet ships them combined), so the wave letter isn't in the file name.
Minute columns are `min_0001` to `min_1440` in both.

## 2003-2006 pipeline (`code/2003_2006/`)

Run `01` to `06` in order: download raw PAXRAW files, reshape to 1440-minute rows, derive
wear/valid days, download and clean survey covariates, then summarize PA.
`06_summarize_pa_data_rnhanesdata.R` is an alternative step 6 using the `rnhanesdata` package.

## Reports (`docs/`)

- `sex_differences.qmd`: survey-weighted PA by sex and age, NHANES 2003-2006.
- `sex_differences2.qmd`: extended version covering 2003-2006, 2011-2014 (AC and MIMS) and NNYFS.

Rendered `.html` files and `*_cache/` / `*_files/` folders are build output and are gitignored.

## Known issues

These were left as-is to keep changes minimal:

- `03_distributions.R` calls `run_models()`, defined in `03_regressions.R`. Run that first in the same session.
- The reports in `docs/` and `2003_2006/06_summarize_pa_data_rnhanesdata.R` still read from older data
  layouts (`data/accelerometry/...`, `data/2003_2006/rnhanes_data/`).
- `archive/test.R` sources `code/2011_2014/prepare_data_functional.R`, which now lives in `archive/`.

## Packages

tidyverse, here, survey, svrep, svylme, broom, slider, fastFMM, tidyfun, refund, haven,
nhanesA, sjlabelled, janitor, gtsummary, gt, paletteer, patchwork, DescTools, curl.
