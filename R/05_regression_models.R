

# -----------------------------
# 1) Build analytic dataset
# -----------------------------
df_reg <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    !is.na(labour_force),
    !is.na(HH_02_RA),
    !is.na(country),
    !is.na(age_cat_lf),
    !is.na(disability_RA),
    !is.na(wgh_samp_pop_restr_resp),
    wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    # make sure outcome is 0/1 numeric
    labour_force = as.numeric(labour_force),
    HH_02_RA = as_factor(HH_02_RA),
    country = as_factor(country),
    disability_RA = as_factor(disability_RA)
  )

# Quick check outcome coding
table(df_reg$labour_force, useNA = "ifany")
levels(df_reg$HH_02_RA)
levels(df_reg$country)
levels(df_reg$disability_RA)

# -----------------------------
# 2) Survey design
# -----------------------------
des_reg <- df_reg %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# -----------------------------
# 3) Models (clean, interpretable)
# -----------------------------
# Model 1: Gender only
m1 <- svyglm(labour_force ~ HH_02_RA, design = des_reg, family = quasibinomial())

# Model 2: Gender + Country
m2 <- svyglm(labour_force ~ HH_02_RA + country, design = des_reg, family = quasibinomial())

# Model 3: Gender + Country + Age + Disability
m3 <- svyglm(labour_force ~ HH_02_RA + country + age_cat_lf + disability_RA,
             design = des_reg, family = quasibinomial())

# -----------------------------
# 4) Odds Ratios tables
# -----------------------------
or_m1 <- tidy(m1, conf.int = TRUE, exponentiate = TRUE)
or_m2 <- tidy(m2, conf.int = TRUE, exponentiate = TRUE)
or_m3 <- tidy(m3, conf.int = TRUE, exponentiate = TRUE)

or_m1
or_m2
or_m3


##Now add skills

