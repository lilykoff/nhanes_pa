# install.packages('sjlabelled')
# formerly create_covariate_datasets_from_raw.R
library(tidyverse)
library(sjlabelled)
source(here::here("code", "utils.R"))

# download the xpts
get_xpt = function(nh_table) {
  nh_table = nh_table_name(nh_table)
  table = normalize_table_name(nh_table)
  # outdir = table_to_outdir(nh_table)
  outdir = "demographics/raw/old"
  stopifnot(!is.na(outdir))
  data_dir = here::here("data", outdir)
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  url = nhanes_xpt_url(nh_table)
  outfile = normalize_table_name(nh_table)
  outfile = paste0(outfile, ".XPT")
  file = file.path(data_dir, outfile)
  if (!file.exists(file)) {
    curl::curl_download(url, destfile = file, quiet = FALSE)
  }
  file
}

names = c("ALQ", "DEMO", "DIQ","BMX", "HSQ", "HUQ", "HSQ", "MCQ", "PAQ", "PFQ", "SMQ")
names_g = paste(names, "C", sep = "_")
names_h = paste(names, "D", sep = "_")

purrr::map(.x = c(names_g, names_h), .f = get_xpt)

# read in the XPTs - do a bunch of manipulations
demo_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "DEMO_C.XPT"))
demo_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "DEMO_D.XPT"))

# translate columns; need to manually fix levels for household size waves in waves G and Y to match wave H
demo_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'DEMO_C',
                           colnames = colnames(demo_c),
                           data = demo_c)
demo_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'DEMO_D',
                           colnames = colnames(demo_d),
                           data = demo_d)

demo = bind_rows(demo_c, demo_d)

# get subset of variables for analysis
demo_analytic =
  demo_c_trans %>%
  select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1, RIDEXMON,
        DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
          INDFMPIR) %>%
  bind_rows(
    demo_d_trans %>%
      select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1,  RIDEXMON,
              DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
             INDFMPIR)
  )

# get same subset but make labels the column names and add column names as labels
demo_analytic_labeled =
  demo_c_trans %>%
  select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1, RIDEXMON,
          DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
         INDFMPIR) %>%
  label_to_colnames() %>%
  bind_rows(
    demo_d_trans %>%
      select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1, RIDEXMON,
              DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
              INDFMPIR) %>%
      label_to_colnames()
  ) %>%
  set_label(., label = colnames(demo_analytic)) # this adds nhanes col names as labels



# repeat process for smoking questionnaire
smq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "SMQ_C.XPT"))
smq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "SMQ_D.XPT"))

smq_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'SMQ_C',
                           colnames = colnames(smq_c),
                           data = smq_c)
smq_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'SMQ_D',
                           colnames = colnames(smq_d),
                           data = smq_d)
smq = bind_rows(smq_c, smq_d)

smq_analytic =
  smq_c_trans %>%
  select(SEQN, SMQ020, SMD030, SMQ040, SMD055) %>%
  bind_rows(
    smq_d_trans %>%
      select(SEQN, SMQ020, SMD030, SMQ040, SMD055))



smq_analytic_labeled =
  smq_c_trans %>%
  select(SEQN, SMQ020, SMD030, SMQ040, SMD055) %>%
  label_to_colnames() %>%
  bind_rows(smq_d_trans %>%
              select(SEQN, SMQ020, SMD030, SMQ040, SMD055) %>%
              label_to_colnames()) %>%
  set_label(., label = colnames(smq_analytic))


# alcohol
alq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "ALQ_C.XPT"))
alq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "ALQ_D.XPT"))

alq_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'ALQ_C',
                           colnames = colnames(alq_c),
                           data = alq_c)
alq_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'ALQ_D',
                           colnames = colnames(alq_d),
                           data = alq_d)

alq = bind_rows(alq_c, alq_d)


alq_analytic = bind_rows(alq_c_trans, alq_d_trans)

alq_analytic_labeled =
  bind_rows(alq_c_trans %>% label_to_colnames(),
            alq_d_trans %>% label_to_colnames()) %>%
  set_label(., label = colnames(alq_analytic))

# height weight bmi
bmx_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "BMX_C.XPT"))
bmx_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "BMX_D.XPT"))


bmx_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'BMX_C',
                           colnames = colnames(bmx_c),
                           data = bmx_c)
bmx_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'BMX_D',
                           colnames = colnames(bmx_d),
                           data = bmx_d)
bmx = bind_rows(bmx_c, bmx_d)

bmx_analytic =
  bmx_c_trans %>% select(SEQN, BMXWT, BMXHT, BMXBMI) %>%
  bind_rows(bmx_d_trans  %>% select(SEQN, BMXWT, BMXHT, BMXBMI))


bmx_analytic_labeled =
  bmx_c_trans %>%
  select(SEQN, BMXWT, BMXHT, BMXBMI) %>% label_to_colnames() %>%
  bind_rows(bmx_d_trans  %>%
              select(SEQN, BMXWT, BMXHT, BMXBMI) %>%
              label_to_colnames()) %>%
  set_label(., label = colnames(bmx_analytic))


# diabetes
diq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "DIQ_C.XPT"))
diq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw",  "DIQ_D.XPT"))


diq_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'DIQ_C',
                           colnames = colnames(diq_c),
                           data = diq_c)
diq_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'DIQ_D',
                           colnames = colnames(diq_d),
                           data = diq_d)
diq = bind_rows(diq_c, diq_d)

diq_analytic =
  diq_c_trans %>%
  select(SEQN, DIQ010) %>%
  bind_rows(diq_d_trans %>%
              select(SEQN, DIQ010))




diq_analytic_labeled =
  diq_c_trans %>%
  select(SEQN, DIQ010) %>%
  label_to_colnames() %>%
  bind_rows(diq_d_trans %>%
              select(SEQN, DIQ010) %>%
              label_to_colnames()) %>%
  set_label(., label = colnames(diq_analytic))


# MCQ
mcq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "MCQ_C.XPT"))
mcq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "MCQ_D.XPT"))

cols = c("SEQN","MCQ080","MCQ160A","MCQ180A","MCQ160B","MCQ180B","MCQ160C",
         "MCQ180C","MCQ160D","MCQ180D","MCQ160E","MCQ180E","MCQ160F","MCQ180F",
         "MCQ160G","MCQ180G","MCQ220")


mcq_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'MCQ_D',
                           colnames = cols,
                           data = mcq_d)

mcq_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'MCQ_C',
                           colnames = cols,
                           data = mcq_c)
mcq = bind_rows(mcq_c, mcq_d)

mcq_analytic =
  mcq_c_trans %>%
  select(
    SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
    MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
    MCQ160G,MCQ180G,MCQ220
  ) %>%
  bind_rows(
    mcq_d_trans %>% select(
      SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
      MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
      MCQ160G,MCQ180G,MCQ220
    )
  )



mcq_analytic_labeled =
  mcq_c_trans %>%
  select(SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
         MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
         MCQ160G,MCQ180G,MCQ220) %>%
  label_to_colnames() %>%
  bind_rows(
    mcq_d_trans %>%
      select(
        SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
        MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
        MCQ160G,MCQ180G,MCQ220) %>%
      label_to_colnames()
  ) %>%
  set_label(., label = colnames(mcq_analytic))


# PAQ
paq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "PAQ_C.XPT"))
paq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "PAQ_D.XPT"))


paq_d_trans = nhanesA::nhanesTranslate(
  nh_table = 'PAQ_D',
  data = paq_d
)

paq_c_trans = nhanesA::nhanesTranslate(nh_table = 'PAQ_C',
                                       data = paq_c)

paq = bind_rows(paq_c, paq_d)

paq_analytic =
  paq_c_trans %>%
  bind_rows(
    paq_d_trans
  )


paq_analytic_labeled =
  paq_c_trans %>%
  label_to_colnames() %>%
  bind_rows(
    paq_d_trans %>%
      label_to_colnames()
  )


# PFQ
pfq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "PFQ_C.XPT"))
pfq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "PFQ_D.XPT"))

pfq_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'PFQ_D',
                           colnames = colnames(pfq_d),
                           data = pfq_d)

pfq_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'PFQ_C',
                           colnames = colnames(pfq_c),
                           data = pfq_c)

pfq = bind_rows(pfq_c, pfq_d)

pfq_analytic =
  pfq_c_trans %>%
  select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ059, PFQ057) %>%
  bind_rows(pfq_d_trans %>%
              select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ057,PFQ059))


pfq_analytic_labeled =
  pfq_c_trans %>%
  select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ059, PFQ057) %>%
  label_to_colnames() %>%
  bind_rows(
    pfq_d_trans %>%
      select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ059, PFQ057) %>%
      label_to_colnames()) %>%
  set_label(., label = colnames(pfq_analytic))


# HSQ
hsq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "HSQ_C.XPT"))
hsq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "HSQ_D.XPT"))

hsq_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'HSQ_D',
                           colnames = colnames(hsq_d),
                           data = hsq_d)

hsq_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'HSQ_C',
                           colnames = colnames(hsq_c),
                           data = hsq_c)


hsq = bind_rows(hsq_c, hsq_d)

hsq_analytic =
  hsq_c_trans %>%
  select(SEQN, HSD010) %>%
  bind_rows(hsq_d_trans %>%
              select(SEQN, HSD010))


hsq_analytic_labeled =
  hsq_c_trans %>%
  select(SEQN, HSD010) %>%
  label_to_colnames() %>%
  bind_rows(
    hsq_d_trans %>%
      select(SEQN, HSD010) %>%
      label_to_colnames()) %>%
  set_label(., label = colnames(hsq_analytic))

# HUQ
huq_c = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "HUQ_C.XPT"))
huq_d = haven::read_xpt(here::here("data", "2003_2006", "demographics", "raw", "HUQ_D.XPT"))

huq_d_trans =
  nhanesA::nhanesTranslate(nh_table = 'HUQ_D',
                           colnames = colnames(huq_d),
                           data = huq_d)

huq_c_trans =
  nhanesA::nhanesTranslate(nh_table = 'HUQ_C',
                           colnames = colnames(huq_c),
                           data = huq_c)


huq = bind_rows(huq_c, huq_d)

huq_analytic =
  huq_c_trans %>%
  select(SEQN, HUQ010) %>%
  bind_rows(huq_d_trans %>%
              select(SEQN, HUQ010))


huq_analytic_labeled =
  huq_c_trans %>%
  select(SEQN, HUQ010) %>%
  label_to_colnames() %>%
  bind_rows(
    huq_d_trans %>%
      select(SEQN, HUQ010) %>%
      label_to_colnames()) %>%
  set_label(., label = colnames(huq_analytic))



# bind all together
all =
  demo %>%
  left_join(smq, by = "SEQN") %>%
  left_join(alq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(diq, by = "SEQN") %>%
  left_join(mcq, by = "SEQN") %>%
  left_join(paq, by = "SEQN") %>%
  left_join(pfq, by = "SEQN") %>%
  left_join(hsq, by = "SEQN") %>%
  left_join(huq, by = "SEQN")

if(!dir.exists(here::here("data", "2003_2006", "demographics", "processed"))){
  dir.create(here::here("data", "2003_2006", "demographics", "processed"), showWarnings = FALSE, recursive = TRUE)
}
readr::write_csv(all, here::here("data", "2003_2006", "demographics", "processed", "all_C_D_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_DSQ_DUQ_raw.csv.gz"))

all_analytic =
  demo_analytic %>%
  left_join(smq_analytic, by = "SEQN") %>%
  left_join(alq_analytic, by = "SEQN") %>%
  left_join(bmx_analytic, by = "SEQN") %>%
  left_join(diq_analytic, by = "SEQN") %>%
  left_join(mcq_analytic, by = "SEQN") %>%
  left_join(paq_analytic, by = "SEQN") %>%
  left_join(pfq_analytic, by = "SEQN") %>%
  left_join(hsq_analytic, by = "SEQN") %>%
  left_join(huq_analytic, by = "SEQN")

readr::write_csv(all_analytic, here::here("data", "2003_2006", "demographics", "processed", "subset_C_D_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_DSQ_DUQ_raw.csv.gz"))


all_analytic_labeled =
  demo_analytic_labeled %>%
  left_join(smq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(alq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(bmx_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(diq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(mcq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(paq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(pfq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(hsq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(huq_analytic_labeled, by = "Respondent sequence number")


readr::write_csv(all_analytic_labeled, here::here("data", "2003_2006", "demographics", "processed", "subset_C_D_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_DSQ_DUQ_translated.csv.gz"))

