# =========================================================
# 00_packages.R
# Purpose: Install and load all packages required for the
#          gender integration labour market analysis pipeline.
# Expected output: All packages available in the session.
# =========================================================

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  # Core tidyverse + data wrangling
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, stringr, forcats,

  # Survey design and weighted statistics
  srvyr, survey,

  # Data import / labels
  readr, haven, labelled, sjlabelled,

  # Descriptive statistics
  pastecs, psych, tableone, expss,

  # Regression and marginal effects
  modelsummary, marginaleffects,

  # Tables
  gt, webshot2, writexl, gtsummary,

  # Figures
  ggplot2, unhcrthemes, scales, waffle,

  # Utilities
  here, remotes, extrafont
)

message("00_packages.R complete: all packages loaded.")
