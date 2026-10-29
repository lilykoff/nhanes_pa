# Archive

Older or exploratory scripts kept for reference. They are not part of the current
pipeline and most read from an older data layout (`data/accelerometry/...`,
`data/lily/data/...`), so they will not run as-is.

| File | What it was |
|---|---|
| `06_summarize_pa_data_11_14.R` | Earlier 2011-2014 day-level AC/MIMS summaries (also NNYFS "paxy" data). Superseded by `2011_2014/01_get_pa_summaries.R`. |
| `summarize_pa.R` | Earlier 2011-2014 AC summaries in 2-hr windows; writes `ac_summary.rds` used by `plot_2011.R`. |
| `06_summarize_pa_data_2003_2006.R` | Earlier 2003-2006 day-level AC summaries (MVPA/vigorous minutes, bouts, AC ranges). Superseded by `2003_2006/06_summarize_pa_data.R`. |
| `lac.R` | Creates log-AC file. Same step is now in `2011_2014/00_download_data.R`. |
| `plot_2011.R`, `plot_2003.R` | Exploratory plots of PA vs. age by sex, per 2-hr window. |
| `distribution_plots.R` | Histograms/densities of minute-level AC and MIMS. |
| `functional.R` | First functional regression attempt (`refund::pffr`) on 2003-2006 AC. |
| `sex_diff_ac.R` | Survey-weighted MVPA tables/plots by sex and age (precursor to the reports). |
| `00_download_data_original.R` | Longer version of `2011_2014/00_download_data.R`. Also writes valid-day-only MIMS/steps files, subject-level MIMS + covariates, and valid days per subject (from nhanes_steps_mortality). |
| `04_fui.R`, `scripts.sh` | `fastFMM::fui` of minute-level log AC on age + sex, and its SLURM submit commands. |
| `prepare_data_functional.R`, `test.R` | Subject-level log-AC matrix with adjusted weights, and survey-bootstrap minute-level regressions (produced `results/fui_boot*.rds`). `test.R` sources `code/2011_2014/prepare_data_functional.R`; update that path if rerun. |
| `test_fit_uni_svy.R` | Test of `svylme::svy2lme` vs. `lme4::lmer` for one minute of the FUI model. |
