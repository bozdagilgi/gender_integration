# =========================================================
# 03_build_analysis_data.R
# Purpose: Harmonise variables across countries, build model-
#          ready analysis datasets, and save to output/data/.
#          Run after 01_load_data.R.
# =========================================================

dir.create(here("output", "data"), recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------
# USER ADJUSTMENT: recode mappings
# Edit the case_when conditions to match actual survey values.
# Gender coding: 1 = Male, 2 = Female (HH_02_RA)
# Population group: Intro_07 (labels vary by country)
# ----------------------------------------------------------

# Helper: build analysis variables on a raw RA_adult data frame
build_analysis_vars <- function(df, country_label) {
  df %>%
    mutate(
      country = country_label,

      # Binary gender flag
      female = case_when(
        HH_02_RA == 2 ~ 1L,
        HH_02_RA == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Labour force status categories (already built in existing scripts)
      # Keep labour_force_status as-is; create a simpler binary outcome
      outside_lf = case_when(
        employed == 1 | unemployed == 1 ~ 0L,
        TRUE ~ 1L          # outside labour force (inactive / potential)
      ),

      # Unpaid work (EMP30: 2 = household/care, 5 = farming, 6 = volunteering)
      unpaid_care = case_when(
        EMP30 == 2 ~ 1L,
        !is.na(EMP30) ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Legal right to work
      no_legal_work_right = case_when(
        JobLegal1 == 2 ~ 1L,          # No
        JobLegal1 == 1 ~ 0L,          # Yes
        TRUE ~ NA_integer_
      )
    )
}

# --- Pakistan -------------------------------------------------
# Merge HHroster education into RA_adult (as per existing pattern)
PAK_RA_adult <- PAK_RA_adult %>%
  mutate(rosterposition = as.numeric(rosterposition)) %>%
  left_join(
    PAK_HHroster %>%
      mutate(rosterposition = as.numeric(rosterposition)) %>%
      select(uuid, rosterposition, HH_Educ18, HH_Educ23, HH_Educ07),
    by = c("uuid", "rosterposition")
  )

# Build labour_force_status (mirrors existing skills_indicators.R logic)
PAK_RA_adult <- PAK_RA_adult %>%
  mutate(
    unpaid_work = case_when(
      unemployed == 0 & employed == 0 & EMP30 %in% c(2, 5, 6) ~ 1,
      TRUE ~ 0
    ),
    no_job_want = case_when(
      unemployed == 0 & employed == 0 & EMP27 == 2 ~ 1,
      TRUE ~ 0
    ),
    no_job_search = case_when(
      unemployed == 0 & employed == 0 & EMP27 == 1 & EMP25a == 2 & EMP25aa == 2 ~ 1,
      TRUE ~ 0
    ),
    no_job_availability = case_when(
      unemployed == 0 & employed == 0 & EMP27 == 1 & EMP25a == 1 & EMP25aa == 1 & EMP29 == 2 ~ 1,
      TRUE ~ 0
    ),
    outside_lab_force_potential = case_when(
      EMP27 == 1 & EMP29 == 1 & EMP25a == 2 & EMP25aa == 2 ~ 1,
      TRUE ~ 0
    ),
    outside_lab_force_no_potential = case_when(
      unemployed == 0 & employed == 0 & outside_lab_force_potential == 0 ~ 1,
      TRUE ~ 0
    ),
    labour_force_status = case_when(
      employed == 1 ~ 1,
      unemployed == 1 ~ 2,
      outside_lab_force_potential == 1 ~ 3,
      outside_lab_force_no_potential == 1 ~ 4,
      TRUE ~ NA_real_
    ),
    labour_force_status = labelled(
      labour_force_status,
      labels = c(
        "Employed" = 1,
        "Unemployed" = 2,
        "Outside labour force - potential" = 3,
        "Outside labour force - unavailable" = 4
      )
    )
  )

pak_analysis <- build_analysis_vars(PAK_RA_adult, "Pakistan")

# --- Cameroon -------------------------------------------------
# ----------------------------------------------------------
# USER ADJUSTMENT: add Cameroon-specific recodes if variable
# names differ from Pakistan (e.g., education, EMP variables).
# ----------------------------------------------------------
cmr_analysis <- build_analysis_vars(CMR_RA_adult, "Cameroon")

# --- Zambia ---------------------------------------------------
# Zambia RA_adult is stored inside the survey object
zam_analysis <- build_analysis_vars(FDS_ZAM_2025_RA_adult$variables, "Zambia")

# --- Combined analysis dataset --------------------------------
analysis_combined <- bind_rows(pak_analysis, cmr_analysis, zam_analysis)

write_rds(analysis_combined, here("output", "data", "analysis_model_ready.rds"))

message("03_build_analysis_data.R complete.")
message("Saved: output/data/analysis_model_ready.rds (", nrow(analysis_combined), " rows)")
