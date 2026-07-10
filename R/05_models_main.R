# =========================================================
# 05_models_main.R
# Purpose: Run the main nested logistic regression models
#          (M1–M6) on labour force exclusion, using survey-
#          weighted logit via survey::svyglm().
#          Produces Table 2 (odds ratios) and Table 3 (AMEs).
# Expected output:
#   output/tables/table2_main_logit_oddsratios.html
#   output/tables/table2_main_logit_oddsratios.xlsx
#   output/tables/table3_marginal_effects.html
#   output/tables/table3_marginal_effects.xlsx
# =========================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Build survey design object from analysis_data
# ---------------------------
svy_analysis <- analysis_data %>%
  as_survey_design(strata = strata, weights = weight, nest = TRUE)

# survey package design (required by svyglm)
svy_svydesign <- svydesign(
  ids     = ~1,
  strata  = ~strata,
  weights = ~weight,
  nest    = TRUE,
  data    = analysis_data
)

# ---------------------------
# Base control formula (no outcome, no key exposure)
# ---------------------------
base_controls <- "age + age2 + education + disability + pop_group + country"

# ---------------------------
# Nested models (M1–M6)
# ---------------------------

# M1: Gender + controls
m1 <- svyglm(
  as.formula(paste("excluded_lf ~ female +", base_controls)),
  family  = quasibinomial(link = "logit"),
  design  = svy_svydesign
)

# M2: + care barrier
m2 <- svyglm(
  as.formula(paste("excluded_lf ~ female + care_barrier +", base_controls)),
  family  = quasibinomial(link = "logit"),
  design  = svy_svydesign
)

# M3: + mobility barrier
m3 <- svyglm(
  as.formula(paste("excluded_lf ~ female + care_barrier + mobility_barrier +",
                   base_controls)),
  family  = quasibinomial(link = "logit"),
  design  = svy_svydesign
)

# M4: + documentation barrier
m4 <- svyglm(
  as.formula(paste("excluded_lf ~ female + care_barrier + mobility_barrier +",
                   "docs_barrier +", base_controls)),
  family  = quasibinomial(link = "logit"),
  design  = svy_svydesign
)

# M5: + digital barrier
m5 <- svyglm(
  as.formula(paste("excluded_lf ~ female + care_barrier + mobility_barrier +",
                   "docs_barrier + digital_barrier +", base_controls)),
  family  = quasibinomial(link = "logit"),
  design  = svy_svydesign
)

# M6: All barriers + gender interactions (fully intersectional model)
m6 <- svyglm(
  as.formula(paste(
    "excluded_lf ~ female * care_barrier +",
    "female * mobility_barrier +",
    "female * docs_barrier +",
    "female * digital_barrier +",
    base_controls
  )),
  family  = quasibinomial(link = "logit"),
  design  = svy_svydesign
)

models_main <- list(
  "M1: Gender"         = m1,
  "M2: + Care"         = m2,
  "M3: + Mobility"     = m3,
  "M4: + Documentation" = m4,
  "M5: + Digital"      = m5,
  "M6: Interactions"   = m6
)

# ---------------------------
# Table 2: Odds ratios via modelsummary
# ---------------------------
coef_labels <- c(
  "female"                  = "Female",
  "care_barrier"            = "Care barrier",
  "mobility_barrier"        = "Mobility/safety barrier",
  "docs_barrier"            = "Documentation barrier",
  "digital_barrier"         = "Digital access barrier",
  "female:care_barrier"     = "Female × Care barrier",
  "female:mobility_barrier" = "Female × Mobility/safety",
  "female:docs_barrier"     = "Female × Documentation",
  "female:digital_barrier"  = "Female × Digital access"
)

modelsummary(
  models_main,
  exponentiate = TRUE,          # report odds ratios
  coef_map     = coef_labels,
  coef_omit    = "age|education|disability|pop_group|country|Intercept",
  stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit     = "AIC|BIC|Log.Lik|RMSE|Deviance",
  notes        = "Survey-weighted logistic regression (svyglm). Odds ratios reported. Controlled for age, age², education, disability, population group, and country fixed effects.",
  output       = here("output", "tables", "table2_main_logit_oddsratios.html")
)

# Excel version
ms_tbl <- modelsummary(
  models_main,
  exponentiate = TRUE,
  coef_map     = coef_labels,
  coef_omit    = "age|education|disability|pop_group|country|Intercept",
  stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit     = "AIC|BIC|Log.Lik|RMSE|Deviance",
  output       = "dataframe"
)

write_xlsx(list(Table2 = ms_tbl),
           here("output", "tables", "table2_main_logit_oddsratios.xlsx"))

# ---------------------------
# Table 3: Average Marginal Effects for M6
# ---------------------------
ame_m6 <- avg_slopes(
  m6,
  variables = c("female", "care_barrier", "mobility_barrier",
                "docs_barrier", "digital_barrier")
)

ame_df <- as.data.frame(ame_m6) %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

ame_gt <- ame_df %>%
  gt() %>%
  tab_header(
    title    = "Table 3. Average Marginal Effects on Labour Force Exclusion",
    subtitle = "Survey-weighted logit Model M6 (fully intersectional)"
  ) %>%
  cols_label(
    term      = "Variable",
    estimate  = "AME",
    std.error = "SE",
    statistic = "z",
    p.value   = "p",
    conf.low  = "95% CI low",
    conf.high = "95% CI high"
  ) %>%
  tab_source_note("Source: UNHCR FDS – Cameroon 2024, Pakistan 2024, Zambia 2025.") %>%
  fmt_number(columns = where(is.numeric), decimals = 4)

gtsave(ame_gt, here("output", "tables", "table3_marginal_effects.html"))
write_xlsx(list(Table3 = ame_df),
           here("output", "tables", "table3_marginal_effects.xlsx"))

message("05_models_main.R complete.")
message("Tables 2 and 3 written to output/tables/")
