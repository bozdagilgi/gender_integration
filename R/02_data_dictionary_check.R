# =========================================================
# 02_data_dictionary_check.R
# Purpose: Quick audit of loaded data columns, types,
#          missingness, and unique values for mapped variables.
#          Run after 01_load_data.R.
# =========================================================

# ----------------------------------------------------------
# USER ADJUSTMENT: variable mapping
# Update raw_name values to match your actual column names.
# ----------------------------------------------------------

var_map <- list(
  gender          = "HH_02_RA",
  pop_group       = "Intro_07",
  labour_status   = "labour_force_status",
  employed        = "employed",
  unemployed      = "unemployed",
  unpaid_work     = "unpaid_work",
  education       = "HH_Educ18",
  disability      = "disability_RA",
  unpaid_type     = "EMP30",
  legal_work_right = "JobLegal1"
)

dir.create(here("output", "checks"), recursive = TRUE, showWarnings = FALSE)

# Helper: check one data frame against the map
check_one_country <- function(df, country_label) {
  col_df <- tibble(
    country  = country_label,
    column   = names(df),
    class    = purrr::map_chr(df, ~ paste(class(.x), collapse = ", ")),
    n_missing = purrr::map_int(df, ~ sum(is.na(.x))),
    pct_missing = round(100 * n_missing / nrow(df), 2)
  )

  map_check <- tibble(
    country    = country_label,
    std_name   = names(var_map),
    raw_name   = unlist(var_map),
    exists_in_data = unlist(var_map) %in% names(df)
  )

  list(columns = col_df, mapping = map_check)
}

# Run check across RA_adult datasets for each country
checks_pak <- check_one_country(PAK_RA_adult, "Pakistan")
checks_cmr <- check_one_country(CMR_RA_adult, "Cameroon")
checks_zam <- check_one_country(FDS_ZAM_2025_RA_adult$variables, "Zambia")

# Write column / missingness reports
all_cols <- bind_rows(checks_pak$columns, checks_cmr$columns, checks_zam$columns)
write_csv(all_cols, here("output", "checks", "columns_missingness.csv"))

# Write variable mapping check
all_mapping <- bind_rows(checks_pak$mapping, checks_cmr$mapping, checks_zam$mapping)
write_csv(all_mapping, here("output", "checks", "var_map_check.csv"))

# Flag any missing mapped variables
missing_mapped <- all_mapping %>% filter(!exists_in_data)
if (nrow(missing_mapped) > 0) {
  message("Some mapped variables are missing – see output/checks/var_map_check.csv")
  print(missing_mapped)
}

# Unique values (top 30) for each mapped variable – Pakistan as reference
safe_unique <- function(df, col_name, top_n = 30) {
  if (!col_name %in% names(df)) {
    return(tibble(value = NA_character_, n = NA_integer_, pct = NA_real_,
                  note = "COLUMN_NOT_FOUND"))
  }
  tibble(value = as.character(df[[col_name]])) %>%
    count(value, sort = TRUE) %>%
    mutate(pct = round(100 * n / sum(n), 2)) %>%
    slice_head(n = top_n)
}

unique_tables <- purrr::map_dfr(names(var_map), function(std_nm) {
  raw_nm <- var_map[[std_nm]]
  safe_unique(PAK_RA_adult, raw_nm) %>%
    mutate(std_name = std_nm, raw_name = raw_nm, country = "Pakistan", .before = 1)
})

write_csv(unique_tables, here("output", "checks", "mapped_variables_unique_values_top30.csv"))

message("02_data_dictionary_check.R complete.")
message("Check output/checks/ for: columns_missingness.csv, var_map_check.csv,")
message("  mapped_variables_unique_values_top30.csv")
