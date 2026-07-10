# =========================================================
# 00_packages.R
# Purpose: Install and load all packages used in the workflow.
#          Source this script first from 08_run_all.R or any
#          individual script before loading other scripts.
# =========================================================

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  # Core tidyverse
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr,

  # Data I/O
  readr, haven, here, writexl,

  # Survey analysis
  srvyr, survey,

  # Variable labels
  labelled, sjlabelled, expss,

  # Tables & output
  gt, webshot2, gtsummary,

  # Modelling helpers
  broom,

  # Descriptive stats
  pastecs, psych, tableone,

  # Visualisation
  ggplot2, unhcrthemes, scales, waffle,

  # Fonts / misc
  extrafont, remotes
)
