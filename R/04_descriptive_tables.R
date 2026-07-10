# =========================================================
# 04_descriptive_tables.R
# Purpose: Produce Table 1 – descriptive statistics by
#          gender and country for the analysis sample.
# Expected output:
#   output/tables/table1_descriptives_by_gender.html
#   output/tables/table1_descriptives_by_gender.xlsx
#   output/tables/table1_descriptives_by_country.html
# =========================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Weighted descriptives using srvyr
# Build a pooled survey design object from analysis_data
# ---------------------------
svy_analysis <- analysis_data %>%
  as_survey_design(strata = strata, weights = weight, nest = TRUE)

# ---------------------------
# Table 1a: Key indicators by gender (weighted proportions / means)
# ---------------------------

# Variables to summarise
bin_vars  <- c("excluded_lf", "care_barrier", "docs_barrier",
               "digital_barrier", "mobility_barrier")
cont_vars <- c("age", "barrier_count")

# Weighted means for binary / continuous vars by gender × country
desc_by_gender <- svy_analysis %>%
  group_by(country, female) %>%
  summarise(
    n_uw             = unweighted(n()),
    excl_lf_pct      = survey_mean(excluded_lf,      vartype = "ci", na.rm = TRUE),
    care_pct         = survey_mean(care_barrier,      vartype = "ci", na.rm = TRUE),
    docs_pct         = survey_mean(docs_barrier,      vartype = "ci", na.rm = TRUE),
    digital_pct      = survey_mean(digital_barrier,   vartype = "ci", na.rm = TRUE),
    mobility_pct     = survey_mean(mobility_barrier,  vartype = "ci", na.rm = TRUE),
    mean_age         = survey_mean(age,               vartype = "ci", na.rm = TRUE),
    mean_barriers    = survey_mean(barrier_count,     vartype = "ci", na.rm = TRUE)
  ) %>%
  mutate(
    gender_label = case_when(
      female == 1 ~ "Female",
      female == 0 ~ "Male",
      TRUE        ~ "Unknown"
    )
  )

write_csv(desc_by_gender, here("output", "tables", "table1_descriptives_raw.csv"))

# ---------------------------
# Table 1b: Formatted gt table – exclusion rate and barrier rates by gender
# ---------------------------
tab1_gt <- desc_by_gender %>%
  select(country, gender_label, n_uw,
         excl_lf_pct, care_pct, docs_pct, digital_pct, mobility_pct,
         mean_age, mean_barriers) %>%
  # Convert proportions (0-1) to percentages only for the pct columns;
  # mean_age and mean_barriers are already on their natural scale.
  mutate(across(c(excl_lf_pct, care_pct, docs_pct, digital_pct, mobility_pct),
                ~ round(.x * 100, 1))) %>%
  mutate(across(c(mean_age, mean_barriers), ~ round(.x, 1))) %>%
  gt(groupname_col = "country") %>%
  tab_header(
    title    = "Table 1. Key Labour Market and Barrier Indicators by Gender and Country",
    subtitle = "Weighted percentages (survey-design-adjusted)"
  ) %>%
  cols_label(
    gender_label    = "Gender",
    n_uw            = "N (unweighted)",
    excl_lf_pct     = "Excluded from LF (%)",
    care_pct        = "Care barrier (%)",
    docs_pct        = "Documentation barrier (%)",
    digital_pct     = "Digital barrier (%)",
    mobility_pct    = "Mobility barrier (%)",
    mean_age        = "Mean age",
    mean_barriers   = "Mean barrier count"
  ) %>%
  tab_spanner(
    label   = "Intersectional barriers",
    columns = c(care_pct, docs_pct, digital_pct, mobility_pct)
  ) %>%
  fmt_number(columns = c(mean_age, mean_barriers), decimals = 1) %>%
  tab_source_note("Source: UNHCR FDS – Cameroon 2024, Pakistan 2024, Zambia 2025.") %>%
  opt_row_striping()

gtsave(tab1_gt, here("output", "tables", "table1_descriptives_by_gender.html"))

# ---------------------------
# Excel export for sharing
# ---------------------------
write_xlsx(
  list(Table1 = as.data.frame(desc_by_gender)),
  here("output", "tables", "table1_descriptives_by_gender.xlsx")
)

message("04_descriptive_tables.R complete.")
message("Outputs written to output/tables/")
