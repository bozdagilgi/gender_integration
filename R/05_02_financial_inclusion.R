

# 1) Female-only sample (since HH_27 is female-specific)
df_lf_fin <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),
    !is.na(labour_force), labour_force %in% c(0,1),
    !is.na(RBM21301),
    !is.na(age_cat_lf), !is.na(country), !is.na(HH_08),
    !is.na(HH_27), !is.na(HH_Educ07_RA),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    labour_force = as.numeric(labour_force),
    
    # Financial inclusion coding (edit if your coding differs)
    fin_inclusion = if_else(RBM21301 == 1, 1,
                            if_else(RBM21301 == 2, 0, NA_real_)),
    
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    country    = droplevels(as_factor(country)),
    HH_08      = droplevels(as_factor(HH_08)),
    HH_27_bin  = if_else(HH_27 == 1, 1, if_else(HH_27 == 2, 0, NA_real_)),
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0),
    
    HH_08_grp = case_when(
      HH_08 %in% c("Married", "Non-formal union") ~ "1_union",
      HH_08 %in% c("Separated", "Divorced", "Widow or Widower") ~ "2_prev_union",
      HH_08 %in% c("Never married") ~ "3_never_married",
      TRUE ~ NA_character_
    ),
    HH_08_grp = factor(HH_08_grp, levels = c("1_union","2_prev_union","3_never_married"))
  ) %>%
  filter(!is.na(fin_inclusion), !is.na(HH_27_bin), !is.na(HH_08_grp))

# check coding
table(df_lf_fin$labour_force, useNA="ifany")
table(df_lf_fin$fin_inclusion, useNA="ifany")

# 2) Survey design
des_lf_fin <- df_lf_fin %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# 3) Model: financial inclusion as key predictor
m_lf_fin <- svyglm(
  labour_force ~ fin_inclusion + HH_27_bin + age_cat_lf + country + HH_08_grp + HH_Educ07_RA_bin,
  design = des_lf_fin,
  family = quasibinomial()
)


summary(m_lf_fin)
tidy(m_lf_fin, conf.int = TRUE, exponentiate = TRUE)
# 4) OR table
or_lf_fin <- tidy(m_lf_fin, conf.int = TRUE, exponentiate = TRUE)
or_lf_fin

# 5) Global test for financial inclusion
g_fin <- regTermTest(m_lf_fin, ~ fin_inclusion)
g_fin



# =========================================================
# Barriers to labour-force access among refugee women
# Primary model: survey-weighted logistic regression
# Outcome: out_of_lf (1 = out of labour force, 0 = in labour force)
# =========================================================


# -----------------------------
# 1) Build analysis dataset
# -----------------------------
df_w <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),                 # adjust if your female code differs
    !is.na(labour_force), labour_force %in% c(0,1),
    !is.na(RBM21301),                              # financial inclusion
    !is.na(age_cat_lf),
    !is.na(country),
    !is.na(HH_08),
    !is.na(HH_27),
    !is.na(HH_Educ07_RA),
    !is.na(wgh_samp_pop_restr_resp),
    wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    # Outcome flipped to "barrier framing"
    out_of_lf = if_else(labour_force == 0, 1, 0),
    
    # Key barrier: financial inclusion (edit coding if needed)
    fin_inclusion = case_when(
      RBM21301 == 1 ~ 1,   # included
      RBM21301 == 2 ~ 0,   # not included
      TRUE ~ NA_real_
    ),
    
    # Child under 2
    HH_27_bin = case_when(
      HH_27 == 1 ~ 1,      # has child under 2
      HH_27 == 2 ~ 0,      # no child under 2
      TRUE ~ NA_real_
    ),
    
    # Education
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0),
    
    # Factors
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    country    = droplevels(as_factor(country)),
    HH_08      = droplevels(as_factor(HH_08)),
    
    # Grouped marital status
    HH_08_grp = case_when(
      HH_08 %in% c("Married", "Non-formal union") ~ "1_union",
      HH_08 %in% c("Separated", "Divorced", "Widow or Widower") ~ "2_prev_union",
      HH_08 %in% c("Never married") ~ "3_never_married",
      TRUE ~ NA_character_
    ),
    HH_08_grp = factor(HH_08_grp, levels = c("1_union","2_prev_union","3_never_married"))
  ) %>%
  filter(!is.na(fin_inclusion), !is.na(HH_27_bin), !is.na(HH_08_grp))

# -----------------------------
# 2) Basic diagnostics
# -----------------------------
cat("\n--- CHECKS ---\n")
print(table(df_w$out_of_lf, useNA = "ifany"))
print(table(df_w$fin_inclusion, useNA = "ifany"))
print(table(df_w$HH_27_bin, useNA = "ifany"))
print(table(df_w$HH_08_grp, useNA = "ifany"))
print(table(df_w$country, useNA = "ifany"))

# Cross-tabs to detect separation risk
cat("\n--- POSSIBLE SEPARATION CHECKS ---\n")
print(table(df_w$country, df_w$out_of_lf))
print(table(df_w$country, df_w$fin_inclusion, df_w$out_of_lf))

# Optional: collapse sparse country levels if needed
# df_w <- df_w %>%
#   mutate(country = fct_lump_min(country, min = 100, other_level = "Other"))

# -----------------------------
# 3) Survey design
# -----------------------------
des_w <- df_w %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# -----------------------------
# 4) Primary adjusted model
# -----------------------------
m_barrier <- svyglm(
  out_of_lf ~ fin_inclusion + HH_27_bin + age_cat_lf + country + HH_08_grp + HH_Educ07_RA_bin,
  design = des_w,
  family = quasibinomial()
)

cat("\n--- PRIMARY MODEL SUMMARY ---\n")
print(summary(m_barrier))

# OR table
or_barrier <- tidy(m_barrier, conf.int = TRUE, exponentiate = TRUE)
cat("\n--- ADJUSTED ORs ---\n")
print(or_barrier)

# If singularity drops a term, inspect:
cat("\n--- ALIAS / ESTIMABLE TERMS ---\n")
print(alias(m_barrier))
print(names(coef(m_barrier)))

# -----------------------------
# 5) Robust fallback model (if country causes separation)
# -----------------------------
m_barrier_nocountry <- svyglm(
  out_of_lf ~ fin_inclusion + HH_27_bin + age_cat_lf + HH_08_grp + HH_Educ07_RA_bin,
  design = des_w,
  family = quasibinomial()
)

cat("\n--- FALLBACK MODEL (NO COUNTRY) ---\n")
print(summary(m_barrier_nocountry))
or_barrier_nocountry <- tidy(m_barrier_nocountry, conf.int = TRUE, exponentiate = TRUE)
print(or_barrier_nocountry)

# -----------------------------
# 6) Extract key effect (financial inclusion)
# -----------------------------
fin_main <- or_barrier %>% filter(term == "fin_inclusion")
fin_fallback <- or_barrier_nocountry %>% filter(term == "fin_inclusion")

cat("\n--- KEY EFFECT: FINANCIAL INCLUSION ---\n")
print(fin_main)
cat("\n--- KEY EFFECT (FALLBACK) ---\n")
print(fin_fallback)


# 1) Unadjusted fin model
m_fin_only <- svyglm(
  out_of_lf ~ fin_inclusion,
  design = des_w,
  family = quasibinomial()
)
tidy(m_fin_only, conf.int=TRUE, exponentiate=TRUE)

# 2) Minimal adjusted model
m_fin_min <- svyglm(
  out_of_lf ~ fin_inclusion + age_cat_lf + HH_27_bin,
  design = des_w,
  family = quasibinomial()
)
tidy(m_fin_min, conf.int=TRUE, exponentiate=TRUE)