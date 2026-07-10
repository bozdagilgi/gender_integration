# =========================================================
# 02_data_dictionary_check.R
# Purpose: Audit column names, classes, missingness, and
#          unique values for variables used in the analysis.
#          Run this before 03_build_analysis_data.R to verify
#          variable mapping for each country's dataset.
# Expected output: CSV and TXT check files in output/checks/
# =========================================================

dir.create(here("output", "checks"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# USER ADJUSTMENT SECTION
# Map standardised analysis names to raw column names.
# Verify these match your actual dataset columns.
# ---------------------------
var_map <- list(
  # Identifiers
  uuid            = "uuid",
  rosterposition  = "rosterposition",
  country         = "country",           # added in 03_build_analysis_data.R
  strata          = "samp_strat",
  weight_adult    = "wgh_samp_pop_restr_resp",

  # Demographics
  sex             = "HH_02_RA",          # 1=Male, 2=Female (or labelled)
  age             = "HH_03_RA",          # age in years
  pop_group       = "Intro_07",          # refugee / asylum seeker / host
  disability      = "disability_RA",

  # Education (merged from HHroster)
  educ_level      = "HH_Educ18",         # highest completed level
  educ_current    = "HH_Educ07",         # currently attending school

  # Labour force (constructed in skills_indicators.R style)
  employed        = "employed",
  unemployed      = "unemployed",
  emp_type        = "EMP30",             # type of unpaid work
  emp_want        = "EMP27",             # want a job
  emp_search_a    = "EMP25a",            # searched last 4 weeks
  emp_search_aa   = "EMP25aa",           # searched last 4 weeks (alt)
  emp_available   = "EMP29",             # available to start

  # Legal right to work
  right_to_work   = "JobLegal1",

  # Care barriers
  care_skill      = "Exp01a",            # experience caring for children
  care_sick       = "Exp01c",            # experience caring for sick/disabled

  # Digital barriers
  has_phone       = "Skills38a",         # can use mobile phone
  has_smartphone  = "Skills38b",         # can use smartphone
  used_computer   = "Skills39",          # used computer last 3 months
  digital_email   = "Skills41a",         # send/receive email
  digital_calls   = "Skills41b",         # video/internet calls
  digital_info    = "Skills41c",         # find info online

  # Driving / mobility proxy
  drive_car       = "Skills42a"          # driving licence - car
)

# ---------------------------
# Helper: audit one raw data frame
# ---------------------------
audit_df <- function(df, country_label) {
  map_check <- tibble(
    country  = country_label,
    std_name = names(var_map),
    raw_name = unlist(var_map),
    exists   = unlist(var_map) %in% names(df)
  )

  # Column-level summary
  col_summary <- tibble(
    country     = country_label,
    column      = names(df),
    class       = map_chr(df, ~ paste(class(.x), collapse = ", ")),
    n_missing   = map_int(df, ~ sum(is.na(.x))),
    pct_missing = round(100 * n_missing / nrow(df), 2)
  )

  list(map_check = map_check, col_summary = col_summary)
}

# ---------------------------
# Run audit for each country RA_adult dataset
# ---------------------------
audits <- list(
  audit_df(CMR_RA_adult, "Cameroon"),
  audit_df(PAK_RA_adult, "Pakistan"),
  audit_df(ZAM_RA_adult, "Zambia")
)

map_checks   <- bind_rows(map(audits, "map_check"))
col_summaries <- bind_rows(map(audits, "col_summary"))

write_csv(map_checks,    here("output", "checks", "var_map_check.csv"))
write_csv(col_summaries, here("output", "checks", "columns_missingness.csv"))

# Flag missing mappings
missing_vars <- map_checks %>% filter(!exists)
if (nrow(missing_vars) > 0) {
  message("Some mapped variables are missing from one or more datasets:")
  print(missing_vars)
  message("Please update var_map in 02_data_dictionary_check.R and 03_build_analysis_data.R.")
} else {
  message("All mapped variables found in all three country datasets.")
}

# ---------------------------
# Unique values for key categorical variables
# ---------------------------
key_vars <- c("sex", "employed", "unemployed", "emp_type", "emp_want",
              "right_to_work", "has_phone", "used_computer",
              "disability", "educ_level", "pop_group")

safe_unique <- function(df, country_label, std_nm) {
  raw_nm <- var_map[[std_nm]]
  if (!raw_nm %in% names(df)) {
    return(tibble(country = country_label, std_name = std_nm,
                  raw_name = raw_nm, value = NA_character_, n = NA_integer_,
                  pct = NA_real_, note = "COLUMN_NOT_FOUND"))
  }
  df %>%
    mutate(.val = as.character(.data[[raw_nm]])) %>%
    count(.val) %>%
    mutate(
      country  = country_label,
      std_name = std_nm,
      raw_name = raw_nm,
      value    = .val,
      pct      = round(100 * n / sum(n), 2),
      note     = ""
    ) %>%
    select(country, std_name, raw_name, value, n, pct, note) %>%
    slice_head(n = 50)
}

unique_tables <- bind_rows(
  map_dfr(key_vars, ~ safe_unique(CMR_RA_adult, "Cameroon", .x)),
  map_dfr(key_vars, ~ safe_unique(PAK_RA_adult, "Pakistan", .x)),
  map_dfr(key_vars, ~ safe_unique(ZAM_RA_adult, "Zambia",   .x))
)

write_csv(unique_tables, here("output", "checks", "mapped_variables_unique_values.csv"))

message("02_data_dictionary_check.R complete.")
message("Check files written to output/checks/")
