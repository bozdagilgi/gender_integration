# =========================================================
# 04_descriptive_tables.R
# Purpose: Weighted descriptive statistics by gender and
#          population group for each country.
#          Run after 03_build_analysis_data.R.
# =========================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------
# USER ADJUSTMENT: survey design variables
# Confirm strata and weight column names for each country.
# ----------------------------------------------------------

# Re-build survey design objects from analysis data so they
# include the harmonised variables created in script 03.
des_pak <- pak_analysis %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

des_cmr <- cmr_analysis %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

des_zam <- zam_analysis %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

# Helper: produce a weighted summary table for one survey design object
make_desc_table <- function(des, country_label) {
  des %>%
    filter(!is.na(female), !is.na(Intro_07)) %>%
    group_by(country = country_label, pop_group = Intro_07, female) %>%
    summarise(
      n_unweighted    = unweighted(n()),
      outside_lf_rate = survey_mean(outside_lf,      na.rm = TRUE, vartype = "ci"),
      unpaid_care_rate = survey_mean(unpaid_care,     na.rm = TRUE, vartype = "ci"),
      no_legal_rate   = survey_mean(no_legal_work_right, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    ) %>%
    mutate(female = labelled::to_factor(female))
}

tab_pak <- make_desc_table(des_pak, "Pakistan")
tab_cmr <- make_desc_table(des_cmr, "Cameroon")
tab_zam <- make_desc_table(des_zam, "Zambia")

tab_desc <- bind_rows(tab_pak, tab_cmr, tab_zam)

# Excel export
write_xlsx(tab_desc, path = here("output", "tables", "table1_weighted_descriptives.xlsx"))

# HTML export via gt
tab_desc %>%
  gt(groupname_col = "country") %>%
  tab_header(title = "Weighted descriptive statistics by gender and country") %>%
  fmt_percent(
    columns = c(outside_lf_rate, unpaid_care_rate, no_legal_rate),
    decimals = 1
  ) %>%
  cols_label(
    pop_group        = "Population group",
    female           = "Gender",
    n_unweighted     = "N (unweighted)",
    outside_lf_rate  = "Outside LF (%)",
    unpaid_care_rate = "Unpaid care (%)",
    no_legal_rate    = "No legal work right (%)"
  ) %>%
  gtsave(filename = here("output", "tables", "table1_weighted_descriptives.html"))

message("04_descriptive_tables.R complete.")
message("Saved: output/tables/table1_weighted_descriptives.{xlsx,html}")
