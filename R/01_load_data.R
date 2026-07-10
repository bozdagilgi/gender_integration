# =========================================================
# 01_load_data.R
# Purpose: Load raw .rds files for all countries and create
#          survey design objects used throughout the workflow.
# =========================================================

# ----------------------------------------------------------
# USER ADJUSTMENT: country data file paths
# Edit the here() paths below to match your folder structure.
# ----------------------------------------------------------

# Note: clear your own environment before sourcing if needed: rm(list = ls())

source(here::here("R", "00_packages.R"))

# --- Cameroon -------------------------------------------------
CMR_HHroster    <- read_rds(here("Cameroon", "data", "HHroster.rds"))
CMR_main        <- read_rds(here("Cameroon", "data", "main.rds"))
CMR_RA_adult    <- read_rds(here("Cameroon", "data", "RA_adult.rds"))
CMR_RA_woman    <- read_rds(here("Cameroon", "data", "RA_woman.rds"))
CMR_RA_caregiver <- read_rds(here("Cameroon", "data", "RA_caregiver.rds"))

# Survey objects – Cameroon
FDS_CMR_2024_main <- CMR_main %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr, nest = TRUE)

FDS_CMR_2024_HHroster <- CMR_HHroster %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr, nest = TRUE)

FDS_CMR_2024_RA_adult <- CMR_RA_adult %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

FDS_CMR_2024_RA_caregiver <- CMR_RA_caregiver %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_u5, nest = TRUE)

FDS_CMR_2024_RA_woman <- CMR_RA_woman %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_w, nest = TRUE)

# --- Zambia ---------------------------------------------------
# Zambia survey objects are stored pre-built as .rds
FDS_ZAM_2025_main        <- read_rds(here("Zambia", "data", "Survey objects", "FDS_ZAM_2025_main.rds"))
FDS_ZAM_2025_RA_adult    <- read_rds(here("Zambia", "data", "Survey objects", "FDS_ZAM_2025_RA_adult.rds"))
FDS_ZAM_2025_RA_woman    <- read_rds(here("Zambia", "data", "Survey objects", "FDS_ZAM_2025_RA_woman.rds"))
FDS_ZAM_2025_RA_caregiver <- read_rds(here("Zambia", "data", "Survey objects", "FDS_ZAM_2025_RA_caregiver.rds"))
FDS_ZAM_2025_HHroster    <- read_rds(here("Zambia", "data", "Survey objects", "FDS_ZAM_2025_HHroster_strat.rds"))

# --- Pakistan -------------------------------------------------
PAK_HHroster    <- read_rds(here("Pakistan", "data", "HHroster.rds"))
PAK_main        <- read_rds(here("Pakistan", "data", "main.rds"))
PAK_RA_adult    <- read_rds(here("Pakistan", "data", "RA_adult.rds"))
PAK_RA_woman    <- read_rds(here("Pakistan", "data", "RA_woman.rds"))
PAK_RA_caregiver <- read_rds(here("Pakistan", "data", "RA_caregiver.rds"))

# Survey objects – Pakistan
FDS_PAK_2024_main <- PAK_main %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr, nest = TRUE)

FDS_PAK_2024_HHroster <- PAK_HHroster %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr, nest = TRUE)

FDS_PAK_2024_RA_adult <- PAK_RA_adult %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

FDS_PAK_2024_RA_caregiver <- PAK_RA_caregiver %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_u5, nest = TRUE)

FDS_PAK_2024_RA_woman <- PAK_RA_woman %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_w, nest = TRUE)

message("01_load_data.R complete: all country data and survey objects loaded.")
