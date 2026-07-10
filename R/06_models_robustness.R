# =========================================================
# 06_models_robustness.R
# Purpose: Alternative model specifications to test
#          sensitivity of main results.
#          Run after 05_models_main.R.
# =========================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "data"),   recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------
# USER ADJUSTMENT: robustness specifications
# Add or remove model variants below as needed.
# ----------------------------------------------------------

des_all <- analysis_combined %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

# R1 – exclude observations with missing education
des_r1 <- analysis_combined %>%
  filter(!is.na(HH_Educ18)) %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

r1 <- svyglm(
  outside_lf ~ female + unpaid_care + no_legal_work_right + HH_Educ18,
  design = des_r1,
  family = quasibinomial()
)

# R2 – placeholder for working-age restriction (18-64)
# USER ADJUSTMENT: replace the filter below with the actual age variable once confirmed,
#   e.g. filter(age >= 18, age <= 64)
des_r2 <- analysis_combined %>%
  filter(!is.na(female)) %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

r2 <- svyglm(
  outside_lf ~ female + unpaid_care + no_legal_work_right + HH_Educ18,
  design = des_r2,
  family = quasibinomial()
)

# R3 – linear probability model (gaussian) as alternative
r3 <- svyglm(
  outside_lf ~ female + unpaid_care + no_legal_work_right + HH_Educ18,
  design = des_all,
  family = gaussian()
)

# Save robustness model objects
save(r1, r2, r3, file = here("output", "data", "robustness_models.RData"))

# Regression table
tidy_coef <- function(model, model_name) {
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(model = model_name) %>%
    select(model, term, estimate, std.error, conf.low, conf.high, p.value)
}

rob_table <- bind_rows(
  tidy_coef(r1, "R1 – complete education cases"),
  tidy_coef(r2, "R2 – non-missing gender subset"),
  tidy_coef(r3, "R3 – linear probability model")
)

write_xlsx(rob_table, path = here("output", "tables", "table3_robustness_models.xlsx"))

rob_table %>%
  gt(groupname_col = "model") %>%
  tab_header(title = "Robustness checks: outside labour force") %>%
  fmt_number(columns = c(estimate, std.error, conf.low, conf.high, p.value), decimals = 3) %>%
  gtsave(filename = here("output", "tables", "table3_robustness_models.html"))

message("06_models_robustness.R complete.")
message("Saved: output/tables/table3_robustness_models.{xlsx,html}")
message("Saved: output/data/robustness_models.RData")
