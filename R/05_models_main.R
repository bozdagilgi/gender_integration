# =========================================================
# 05_models_main.R
# Purpose: Survey-weighted logistic regression models (main
#          specifications).  Uses svyglm with quasibinomial
#          family on the combined analysis dataset.
#          Run after 03_build_analysis_data.R.
# =========================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "data"),   recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------
# USER ADJUSTMENT: model formula variables
# Edit predictors to match harmonised column names.
# ----------------------------------------------------------

# Combined survey design (all countries, using combined analysis data)
des_all <- analysis_combined %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

# M1 – gender + demographics only
m1 <- svyglm(
  outside_lf ~ female + HH_Educ18,
  design = des_all,
  family = quasibinomial()
)

# M2 – add structural barriers
m2 <- svyglm(
  outside_lf ~ female + unpaid_care + no_legal_work_right + HH_Educ18,
  design = des_all,
  family = quasibinomial()
)

# M3 – gender interactions with barriers
m3 <- svyglm(
  outside_lf ~ female * unpaid_care + female * no_legal_work_right + HH_Educ18,
  design = des_all,
  family = quasibinomial()
)

# Country-specific models (Pakistan)
des_pak_m <- pak_analysis %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

m4_pak <- svyglm(
  outside_lf ~ female + unpaid_care + no_legal_work_right + HH_Educ18,
  design = des_pak_m,
  family = quasibinomial()
)

# Country-specific models (Cameroon)
des_cmr_m <- cmr_analysis %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

m5_cmr <- svyglm(
  outside_lf ~ female + unpaid_care + no_legal_work_right + HH_Educ18,
  design = des_cmr_m,
  family = quasibinomial()
)

# Country-specific models (Zambia)
des_zam_m <- zam_analysis %>%
  as_survey_design(strata = samp_strat, weights = wgh_samp_pop_restr_resp, nest = TRUE)

m6_zam <- svyglm(
  outside_lf ~ female + unpaid_care + no_legal_work_right + HH_Educ18,
  design = des_zam_m,
  family = quasibinomial()
)

# --- Save model objects for use in robustness / export scripts
save(m1, m2, m3, m4_pak, m5_cmr, m6_zam,
     file = here("output", "data", "main_models.RData"))

# --- Regression table (odds ratios) --------------------------
tidy_or <- function(model, model_name) {
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      OR      = exp(estimate),
      OR_low  = exp(conf.low),
      OR_high = exp(conf.high),
      model   = model_name
    ) %>%
    select(model, term, estimate, std.error, p.value, OR, OR_low, OR_high)
}

reg_table <- bind_rows(
  tidy_or(m1,      "M1 – gender + education"),
  tidy_or(m2,      "M2 – barriers (pooled)"),
  tidy_or(m3,      "M3 – interactions (pooled)"),
  tidy_or(m4_pak,  "M4 – Pakistan"),
  tidy_or(m5_cmr,  "M5 – Cameroon"),
  tidy_or(m6_zam,  "M6 – Zambia")
)

write_xlsx(reg_table, path = here("output", "tables", "table2_main_models_OR.xlsx"))

reg_table %>%
  gt(groupname_col = "model") %>%
  tab_header(title = "Survey-weighted logit models: outside labour force (Odds Ratios)") %>%
  fmt_number(columns = c(OR, OR_low, OR_high, p.value), decimals = 3) %>%
  gtsave(filename = here("output", "tables", "table2_main_models_OR.html"))

message("05_models_main.R complete.")
message("Saved: output/tables/table2_main_models_OR.{xlsx,html}")
message("Saved: output/data/main_models.RData")
