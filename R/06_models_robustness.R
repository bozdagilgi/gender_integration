# =========================================================
# 06_models_robustness.R
# Purpose: Robustness checks for the main regression results:
#          (a) country-specific models,
#          (b) cumulative barrier count model,
#          (c) alternative outcome (unemployment).
# Expected output:
#   output/tables/table4_country_models.html
#   output/tables/table4_country_models.xlsx
#   output/tables/tableA1_barrier_count_model.html
#   output/tables/tableA2_unemployment_model.html
# =========================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

coef_labels_rob <- c(
  "female"                  = "Female",
  "care_barrier"            = "Care barrier",
  "mobility_barrier"        = "Mobility/safety barrier",
  "docs_barrier"            = "Documentation barrier",
  "digital_barrier"         = "Digital access barrier",
  "barrier_count"           = "Barrier count (0–4)",
  "female:care_barrier"     = "Female × Care barrier",
  "female:mobility_barrier" = "Female × Mobility/safety",
  "female:docs_barrier"     = "Female × Documentation",
  "female:digital_barrier"  = "Female × Digital access"
)

base_controls_ctry <- "age + age2 + education + disability + pop_group"

# ---------------------------
# (a) Country-specific models (Table 4)
# ---------------------------
countries <- levels(analysis_data$country)

country_models <- map(countries, function(ctry) {
  d_ctry <- analysis_data %>% filter(country == ctry)
  tryCatch(
    svyglm(
      as.formula(paste(
        "excluded_lf ~ female + care_barrier + mobility_barrier +",
        "docs_barrier + digital_barrier +",
        base_controls_ctry
      )),
      family = quasibinomial(link = "logit"),
      design = svydesign(
        ids     = ~1,
        strata  = ~strata,
        weights = ~weight,
        nest    = TRUE,
        data    = d_ctry
      )
    ),
    error = function(e) {
      message("Model failed for ", ctry, ": ", e$message)
      NULL
    }
  )
})

names(country_models) <- countries
country_models        <- country_models[!map_lgl(country_models, is.null)]

if (length(country_models) > 0) {
  modelsummary(
    country_models,
    exponentiate = TRUE,
    coef_map     = coef_labels_rob,
    coef_omit    = "age|education|disability|pop_group|Intercept",
    stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_omit     = "AIC|BIC|Log.Lik|RMSE|Deviance",
    notes        = "Survey-weighted logistic regression (svyglm). Odds ratios. Controlled for age, age², education, disability, and population group.",
    output       = here("output", "tables", "table4_country_models.html")
  )

  ms_ctry <- modelsummary(
    country_models,
    exponentiate = TRUE,
    coef_map     = coef_labels_rob,
    coef_omit    = "age|education|disability|pop_group|Intercept",
    stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_omit     = "AIC|BIC|Log.Lik|RMSE|Deviance",
    output       = "dataframe"
  )
  write_xlsx(list(Table4 = ms_ctry),
             here("output", "tables", "table4_country_models.xlsx"))
}

# ---------------------------
# (b) Cumulative barrier count model (Appendix A1)
# ---------------------------
svy_svydesign <- svydesign(
  ids     = ~1,
  strata  = ~strata,
  weights = ~weight,
  nest    = TRUE,
  data    = analysis_data
)

m_count <- svyglm(
  as.formula(paste(
    "excluded_lf ~ female * barrier_count +",
    base_controls_ctry, "+ country"
  )),
  family = quasibinomial(link = "logit"),
  design = svy_svydesign
)

modelsummary(
  list("Barrier count model" = m_count),
  exponentiate = TRUE,
  coef_map     = coef_labels_rob,
  coef_omit    = "age|education|disability|pop_group|country|Intercept",
  stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit     = "AIC|BIC|Log.Lik|RMSE|Deviance",
  notes        = "Survey-weighted logistic regression. Cumulative barrier count (0–4) replaces individual barrier dummies.",
  output       = here("output", "tables", "tableA1_barrier_count_model.html")
)

# ---------------------------
# (c) Alternative outcome: unemployed (among labour force participants)
# USER ADJUSTMENT: verify 'unemployed' column is present in analysis_data
# ---------------------------
if ("unemployed" %in% names(analysis_data)) {
  d_lf <- analysis_data %>%
    filter(!is.na(unemployed), labour_force_status %in% c(1L, 2L))

  svy_lf <- svydesign(
    ids     = ~1,
    strata  = ~strata,
    weights = ~weight,
    nest    = TRUE,
    data    = d_lf
  )

  m_unemp <- svyglm(
    as.formula(paste(
      "unemployed ~ female + care_barrier + mobility_barrier +",
      "docs_barrier + digital_barrier +",
      base_controls_ctry, "+ country"
    )),
    family = quasibinomial(link = "logit"),
    design = svy_lf
  )

  modelsummary(
    list("Unemployment model" = m_unemp),
    exponentiate = TRUE,
    coef_map     = coef_labels_rob,
    coef_omit    = "age|education|disability|pop_group|country|Intercept",
    stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_omit     = "AIC|BIC|Log.Lik|RMSE|Deviance",
    notes        = "Alternative outcome: unemployed (among labour force participants). Survey-weighted logistic regression.",
    output       = here("output", "tables", "tableA2_unemployment_model.html")
  )
} else {
  message("'unemployed' column not found; skipping alternative outcome model.")
}

message("06_models_robustness.R complete.")
message("Robustness tables written to output/tables/")
