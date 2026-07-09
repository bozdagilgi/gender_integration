###Skills related indicators on individual level

#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven, here,
  extrafont,here)


##Clean environment if need be
rm(list = ls())
###Load Cameroon data

CMR_HHroster <- read_rds(here("Cameroon","data","HHroster.rds"))
CMR_main <- read_rds(here("Cameroon","data","main.rds"))
CMR_RA_adult <- read_rds(here("Cameroon","data","RA_adult.rds"))
CMR_RA_woman <- read_rds(here("Cameroon","data","RA_woman.rds"))
CMR_RA_caregiver <- read_rds(here("Cameroon","data","RA_caregiver.rds"))

# Survey objects
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


##Load Zambia data

FDS_ZAM_2025_main <- read_rds(here("Zambia","data","Survey objects","FDS_ZAM_2025_main.rds"))
FDS_ZAM_2025_RA_adult <- read_rds(here("Zambia","data","Survey objects","FDS_ZAM_2025_RA_adult.rds"))
FDS_ZAM_2025_RA_woman <- read_rds(here("Zambia","data","Survey objects","FDS_ZAM_2025_RA_woman.rds"))
FDS_ZAM_2025_RA_caregiver <- read_rds(here("Zambia","data","Survey objects","FDS_ZAM_2025_RA_caregiver.rds"))
FDS_ZAM_2025_HHroster <- read_rds(here("Zambia","data","Survey objects","FDS_ZAM_2025_HHroster_strat.rds"))

##Load Pakistan data

# Load data
PAK_HHroster <- read_rds(here("Pakistan","data","HHroster.rds"))
PAK_main <- read_rds(here("Pakistan","data","main.rds"))
PAK_RA_adult <- read_rds(here("Pakistan","data","RA_adult.rds"))
PAK_RA_woman <- read_rds(here("Pakistan","data","RA_woman.rds"))
PAK_RA_caregiver <- read_rds(here("Pakistan","data","RA_caregiver.rds"))

# Survey objects
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
