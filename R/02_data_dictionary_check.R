# =========================================================
# 02_data_dictionary_check_full.R
# Purpose: Audit availability, class, missingness, and
#          observed values for all labour market model
#          variables across Cameroon, Pakistan, and Zambia.
# 
# Run after:
#   00_packages.R
#   01_load_data.R
#
# Output:
#   output/checks_full/variable_availability.csv
#   output/checks_full/variable_summary.csv
#   output/checks_full/categorical_unique_values.csv
#   output/checks_full/missing_by_country.csv
# =========================================================

dir.create(here("output", "checks_full"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1. Full variable inventory from QMD + pooled model plan
# ---------------------------------------------------------
analysis_vars <- c(
  # identifiers / survey design
  "uuid", "rosterposition", "samp_strat", "wgh_samp_pop_restr_resp",
  
  # disaggregation / demographics
  "HH_02_RA", "HH_03_RA", "Intro_07", "disability_RA",
  
  # education / roster merge targets
  "HH_Educ18", "HH_Educ23", "HH_Educ07", "HH_27",
  
  # labour force inputs
  "labour_force", "employed", "unemployed",
  "EMP30", "EMP27", "EMP25a", "EMP25aa", "EMP29",
  
  # skills / experience
  "Exp01a", "Exp01c", "Exp01d", "Exp01e", "Exp01f", "Exp01g", "Exp01h",
  "Exp01i", "Exp01j", "Exp01k", "Exp01l", "Exp01m", "Exp01n", "Exp01o",
  "Exp03",
  
  # literacy / numeracy / digital
  "Skill01a", "Skills06b", "Skills06c", "Skills06d",
  "Skills38a", "Skills38b", "Skills38c", "Skills39",
  "Skills41a", "Skills41b", "Skills41c", "Skills41d",
  "Skills42a", "Skills42b",
  
  # legal / finance / assets
  "JobLegal1", "RBM21301", "C050b01",
  
  # mental health
  paste0("MH01_", 1:9),
  
  # decision-making
  "MD01_1", "MD01_2", "MD01_3", "MD01_4", "MD01_5",
  "MD01_6", "MD01_7", "MD01_8", "MD01_9",
  
  # job barriers
  "JobSearch11", "JobSearch12", "JobSearch13", "JobSearch14",
  
  # discrimination
  "DI01_01", "DI01_02", "DI01_03", "DI01_04", "DI01_05",
  "DI01_06", "DI01_07", "DI01_08", "DI01_09",
  
  # safety / mobility
  "FS01", "C160105",
  
  # domestic work
  "DW01_1", "DW01_2", "DW01_3", "DW01_4", "DW01_5",
  "DW01_6", "DW01_7", "DW01_8", "DW01_9", "DW01_10",
  
  # training / job information
  "EducR13", "INFO00_3"
)

# Variables expected in HH roster rather than RA_adult
roster_vars <- c("HH_Educ18", "HH_Educ23", "HH_Educ07", "HH_27")

# Derived variables from QMD (not expected to exist yet in raw data)
derived_vars <- c(
  "unpaid_work", "no_job_want", "no_job_search", "no_job_availability",
  "outside_lab_force_potential", "outside_lab_force_no_potential",
  "labour_force_status", "in_labour_force",
  "hhwork", "farming", "volunteering",
  paste0("new_MH01_", 1:9),
  "PHQ9", "depression",
  "country"
)

# ---------------------------------------------------------
# 2. Data sources
# ---------------------------------------------------------
country_data <- list(
  Cameroon = list(
    ra_adult = CMR_RA_adult,
    hhroster = CMR_HHroster
  ),
  Pakistan = list(
    ra_adult = PAK_RA_adult,
    hhroster = PAK_HHroster
  ),
  Zambia = list(
    ra_adult = ZAM_RA_adult,
    hhroster = ZAM_HHroster
  )
)

# ---------------------------------------------------------
# 3. Helper functions
# ---------------------------------------------------------
safe_class <- function(x) {
  paste(class(x), collapse = ", ")
}

safe_missing_n <- function(x) {
  sum(is.na(x))
}

safe_missing_pct <- function(x) {
  round(100 * mean(is.na(x)), 2)
}

safe_n_unique <- function(x) {
  dplyr::n_distinct(x, na.rm = FALSE)
}

get_source_expected <- function(var) {
  if (var %in% roster_vars) {
    return("HHroster")
  } else if (var %in% derived_vars) {
    return("Derived")
  } else {
    return("RA_adult")
  }
}

audit_one_variable <- function(var, country_name, ra_df, roster_df) {
  expected_source <- get_source_expected(var)
  
  in_ra <- var %in% names(ra_df)
  in_roster <- var %in% names(roster_df)
  
  actual_source <- dplyr::case_when(
    in_ra & in_roster ~ "Both",
    in_ra ~ "RA_adult",
    in_roster ~ "HHroster",
    TRUE ~ "Missing"
  )
  
  if (in_ra) {
    x <- ra_df[[var]]
  } else if (in_roster) {
    x <- roster_df[[var]]
  } else {
    x <- NULL
  }
  
  tibble(
    country = country_name,
    variable = var,
    expected_source = expected_source,
    actual_source = actual_source,
    exists = actual_source != "Missing",
    in_ra_adult = in_ra,
    in_hhroster = in_roster,
    class = if (is.null(x)) NA_character_ else safe_class(x),
    n_missing = if (is.null(x)) NA_integer_ else safe_missing_n(x),
    pct_missing = if (is.null(x)) NA_real_ else safe_missing_pct(x),
    n_unique = if (is.null(x)) NA_integer_ else safe_n_unique(x)
  )
}

extract_unique_values <- function(var, country_name, ra_df, roster_df, max_values = 50) {
  in_ra <- var %in% names(ra_df)
  in_roster <- var %in% names(roster_df)
  
  if (in_ra) {
    x <- ra_df[[var]]
    source <- "RA_adult"
  } else if (in_roster) {
    x <- roster_df[[var]]
    source <- "HHroster"
  } else {
    return(
      tibble(
        country = country_name,
        variable = var,
        source = "Missing",
        value = NA_character_,
        n = NA_integer_,
        pct = NA_real_,
        note = "COLUMN_NOT_FOUND"
      )
    )
  }
  
  tibble(value = as.character(x)) %>%
    count(value, sort = TRUE) %>%
    mutate(
      country = country_name,
      variable = var,
      source = source,
      pct = round(100 * n / sum(n), 2),
      note = ""
    ) %>%
    select(country, variable, source, value, n, pct, note) %>%
    slice_head(n = max_values)
}

# ---------------------------------------------------------
# 4. Variable availability and summary
# ---------------------------------------------------------
all_vars_to_check <- c(analysis_vars, derived_vars)

variable_summary <- purrr::map_dfr(names(country_data), function(ctry) {
  audit_one_variable(
    var = all_vars_to_check[1],
    country_name = ctry,
    ra_df = country_data[[ctry]]$ra_adult,
    roster_df = country_data[[ctry]]$hhroster
  )
})

variable_summary <- purrr::map_dfr(all_vars_to_check, function(vv) {
  purrr::map_dfr(names(country_data), function(ctry) {
    audit_one_variable(
      var = vv,
      country_name = ctry,
      ra_df = country_data[[ctry]]$ra_adult,
      roster_df = country_data[[ctry]]$hhroster
    )
  })
})

variable_availability <- variable_summary %>%
  select(country, variable, expected_source, actual_source, exists, in_ra_adult, in_hhroster)

missing_by_country <- variable_summary %>%
  filter(!exists | expected_source != actual_source) %>%
  arrange(variable, country)

# ---------------------------------------------------------
# 5. Unique values for categorical / coded variables
# ---------------------------------------------------------
categorical_vars <- c(
  "HH_02_RA", "Intro_07", "disability_RA",
  "labour_force", "employed", "unemployed",
  "EMP30", "EMP27", "EMP25a", "EMP25aa", "EMP29",
  "HH_Educ18", "HH_Educ23", "HH_Educ07", "HH_27",
  "Exp01a", "Exp01c", "Exp01d", "Exp01e", "Exp01f", "Exp01g", "Exp01h",
  "Exp01i", "Exp01j", "Exp01k", "Exp01l", "Exp01m", "Exp01n", "Exp01o",
  "Exp03",
  "Skill01a", "Skills06b", "Skills06c", "Skills06d",
  "Skills38a", "Skills38b", "Skills38c", "Skills39",
  "Skills41a", "Skills41b", "Skills41c", "Skills41d",
  "Skills42a", "Skills42b",
  "JobLegal1", "RBM21301", "C050b01",
  paste0("MH01_", 1:9),
  "MD01_1", "MD01_2", "MD01_3", "MD01_4", "MD01_5",
  "MD01_6", "MD01_7", "MD01_8", "MD01_9",
  "JobSearch11", "JobSearch12", "JobSearch13", "JobSearch14",
  "DI01_01", "DI01_02", "DI01_03", "DI01_04", "DI01_05",
  "DI01_06", "DI01_07", "DI01_08", "DI01_09",
  "FS01", "C160105",
  "DW01_1", "DW01_2", "DW01_3", "DW01_4", "DW01_5",
  "DW01_6", "DW01_7", "DW01_8", "DW01_9", "DW01_10",
  "EducR13", "INFO00_3"
)

categorical_unique_values <- purrr::map_dfr(categorical_vars, function(vv) {
  purrr::map_dfr(names(country_data), function(ctry) {
    extract_unique_values(
      var = vv,
      country_name = ctry,
      ra_df = country_data[[ctry]]$ra_adult,
      roster_df = country_data[[ctry]]$hhroster,
      max_values = 50
    )
  })
})

# ---------------------------------------------------------
# 6. Save outputs
# ---------------------------------------------------------
readr::write_csv(
  variable_availability,
  here("output", "checks_full", "variable_availability.csv")
)

readr::write_csv(
  variable_summary,
  here("output", "checks_full", "variable_summary.csv")
)

readr::write_csv(
  categorical_unique_values,
  here("output", "checks_full", "categorical_unique_values.csv")
)

readr::write_csv(
  missing_by_country,
  here("output", "checks_full", "missing_by_country.csv")
)

# ---------------------------------------------------------
# 7. Console messages
# ---------------------------------------------------------
message("02_data_dictionary_check_full.R complete.")
message("Files written to output/checks_full/")

message("Summary:")
message("- variable_availability.csv: whether each variable exists and where")
message("- variable_summary.csv: class, missingness, unique counts")
message("- categorical_unique_values.csv: observed values for coded/categorical variables")
message("- missing_by_country.csv: variables missing or not in expected source")

if (nrow(missing_by_country) > 0) {
  message("Some variables are missing or found in an unexpected source. Review missing_by_country.csv")
} else {
  message("All checked variables were found in expected sources.")
}