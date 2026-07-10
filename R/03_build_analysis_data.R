# =========================================================
# 03_build_analysis_data.R
# Purpose: Merge per-country RA_adult datasets with HHroster
#          education variables, construct labour force status
#          and intersectional barrier indicators, and pool
#          into a single analysis-ready data frame.
# Expected output:
#   analysis_data  – pooled data frame (in session)
#   output/data/analysis_model_ready.rds
# =========================================================

dir.create(here("output", "data"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# USER ADJUSTMENT SECTION
# Verify these column names match your raw data.
# Run 02_data_dictionary_check.R first to inspect values.
# ---------------------------
sex_col        <- "HH_02_RA"       # 1 = Male, 2 = Female
age_col        <- "HH_03_RA"       # numeric age
pop_group_col  <- "Intro_07"       # population group (refugee / host / etc.)
disability_col <- "disability_RA"
educ_col       <- "HH_Educ18"      # merged from HHroster
right_work_col <- "JobLegal1"      # 1 = Yes, 2 = No (right to work)
phone_col      <- "Skills38a"      # 1 = Yes (can use mobile phone)
computer_col   <- "Skills39"       # 1 = Yes (used computer last 3 months)
care_col       <- "Exp01a"         # 1 = Yes (experience caring for children)
drive_col      <- "Skills42a"      # driving licence as mobility proxy

# Unpaid work / outside labour force type
emp_type_col   <- "EMP30"          # 2=hh work, 5=farming, 6=volunteering
emp_want_col   <- "EMP27"          # 1=want, 2=don't want
emp_search_col <- "EMP25a"         # 1=searched, 2=not searched
emp_srch2_col  <- "EMP25aa"        # alternate search variable
emp_avail_col  <- "EMP29"          # 1=available, 2=not available

# ---------------------------
# Helper: prepare one country's RA_adult data
# ---------------------------
prep_country <- function(ra_adult, hhroster, country_label,
                         wgt_var = "wgh_samp_pop_restr_resp",
                         strata_var = "samp_strat") {

  # 'employed' and 'unemployed' are binary columns pre-built in the raw RA_adult
  # .rds files (see skills_indicators.R which uses them directly). Confirm they
  # exist before proceeding.
  required_cols <- c("employed", "unemployed", emp_want_col, emp_avail_col,
                     emp_search_col, emp_srch2_col, emp_type_col)
  missing_req <- required_cols[!required_cols %in% names(ra_adult)]
  if (length(missing_req) > 0) {
    stop("prep_country (", country_label, "): required columns missing: ",
         paste(missing_req, collapse = ", "),
         "\nCheck RA_adult data or update the variable name constants above.")
  }

  # Merge education from HHroster
  ra_adult <- ra_adult %>%
    mutate(rosterposition = as.numeric(rosterposition)) %>%
    left_join(
      hhroster %>%
        mutate(rosterposition = as.numeric(rosterposition)) %>%
        select(uuid, rosterposition,
               any_of(c(educ_col, "HH_Educ23", "HH_Educ07"))),
      by = c("uuid", "rosterposition")
    )

  ra_adult %>%
    mutate(
      country = country_label,
      weight  = .data[[wgt_var]],
      strata  = .data[[strata_var]],

      # --- Demographics ---
      female = case_when(
        as.numeric(.data[[sex_col]]) == 2 ~ 1L,
        as.numeric(.data[[sex_col]]) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),
      age       = suppressWarnings(as.numeric(.data[[age_col]])),
      education = as.factor(suppressWarnings(as.numeric(.data[[educ_col]]))),
      disability = as.factor(suppressWarnings(as.numeric(.data[[disability_col]]))),
      pop_group  = as.factor(suppressWarnings(as.numeric(.data[[pop_group_col]]))),

      # --- Labour force status step 1: potential LF (mirrors skills_indicators.R) ---
      outside_lf_potential = case_when(
        .data[[emp_want_col]] == 1 &
          .data[[emp_avail_col]] == 1 &
          .data[[emp_search_col]] == 2 &
          .data[[emp_srch2_col]] == 2 ~ 1L,
        TRUE ~ 0L
      ),

      # --- Barrier indicators ---

      # Care barrier: does unpaid care work (household work).
      # 'employed' is a pre-existing binary column in the raw RA_adult data.
      care_barrier = case_when(
        as.numeric(.data[[emp_type_col]]) == 2 ~ 1L,  # household/care work
        !is.na(employed)                       ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Documentation barrier: no legal right to work
      docs_barrier = case_when(
        as.numeric(.data[[right_work_col]]) == 2 ~ 1L,  # No = 2
        as.numeric(.data[[right_work_col]]) == 1 ~ 0L,  # Yes = 1
        TRUE ~ NA_integer_
      ),

      # Digital barrier: cannot use a mobile phone OR no computer use
      digital_barrier = case_when(
        as.numeric(.data[[phone_col]])    == 2 ~ 1L,   # cannot use phone
        as.numeric(.data[[computer_col]]) == 2 ~ 1L,   # no computer use
        as.numeric(.data[[phone_col]])    == 1 &
          as.numeric(.data[[computer_col]]) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Mobility barrier: no driving licence as accessible proxy
      # USER ADJUSTMENT: replace with a direct safety/mobility question if available
      mobility_barrier = case_when(
        as.numeric(.data[[drive_col]]) == 2 ~ 1L,   # no driving licence
        as.numeric(.data[[drive_col]]) == 1 ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    # Second mutate: derived columns that depend on outside_lf_potential or age
    mutate(
      age2 = age^2,

      # Labour force status step 2: requires outside_lf_potential from above
      outside_lf_no_potential = case_when(
        employed == 0 & unemployed == 0 & outside_lf_potential == 0 ~ 1L,
        TRUE ~ 0L
      ),
      labour_force_status = case_when(
        employed == 1                  ~ 1L,
        unemployed == 1                ~ 2L,
        outside_lf_potential == 1      ~ 3L,
        outside_lf_no_potential == 1   ~ 4L,
        TRUE ~ NA_integer_
      ),

      # Outcome: excluded from labour force (outside LF, any reason)
      excluded_lf = case_when(
        labour_force_status %in% c(3L, 4L) ~ 1L,
        labour_force_status %in% c(1L, 2L) ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    mutate(
      # Cumulative barrier count (0-4)
      barrier_count = rowSums(
        cbind(care_barrier, docs_barrier, digital_barrier, mobility_barrier),
        na.rm = FALSE
      ),
      barrier_cat = cut(
        barrier_count,
        breaks = c(-Inf, 0, 1, 2, Inf),
        labels = c("0", "1", "2", "3+"),
        right  = TRUE
      )
    )
}

# ---------------------------
# Prepare each country
# ---------------------------
CMR_analysis <- prep_country(CMR_RA_adult, CMR_HHroster, "Cameroon")
PAK_analysis <- prep_country(PAK_RA_adult, PAK_HHroster, "Pakistan")
ZAM_analysis <- prep_country(ZAM_RA_adult, ZAM_HHroster,  "Zambia")

# ---------------------------
# Pool into a single data frame
# ---------------------------
analysis_data <- bind_rows(CMR_analysis, PAK_analysis, ZAM_analysis) %>%
  mutate(country = as.factor(country)) %>%
  filter(
    !is.na(excluded_lf),
    !is.na(female),
    !is.na(age),
    !is.na(country)
  )

saveRDS(analysis_data, here("output", "data", "analysis_model_ready.rds"))

message("03_build_analysis_data.R complete.")
message(paste("Pooled analysis dataset:", nrow(analysis_data), "observations across",
              n_distinct(analysis_data$country), "countries."))
