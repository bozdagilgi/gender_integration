###recode education variables


##recode HH_08

table(combined_RA_adult_ind$HH_08)

combined_RA_adult_ind <- combined_RA_adult_ind %>%   # or use your main dataframe before model
  mutate(
    HH_08 = as_factor(HH_08),
    
    HH_08_grp = case_when(
      HH_08 %in% c("Married", "Non-formal union") ~ "1_union",
      HH_08 %in% c("Separated", "Divorced", "Widow or Widower") ~ "2_prev_union",
      HH_08 %in% c("Never married") ~ "3_never_married",
      TRUE ~ NA_character_
    ),
    HH_08_grp = factor(HH_08_grp,
                       levels = c("1_union","2_prev_union","3_never_married"))
  )

table(combined_RA_adult_ind$HH_08, combined_RA_adult_ind$HH_08_grp, useNA = "ifany")
table(combined_RA_adult_ind$HH_08_grp, useNA = "ifany")


combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    # 1 stays 1, everything else (including NA/99/2/3...) becomes 0
    HH_Educ07_RA_bin        = if_else(HH_Educ07_RA == 1, 1, 0),
    primary_complete_RA_bin = if_else(primary_complete_RA == 1, 1, 0),
    lowersec_complete_RA_bin= if_else(lowersec_complete_RA == 1, 1, 0)
  )

# check
table(combined_RA_adult_ind$HH_Educ07_RA_bin, useNA = "ifany")
table(combined_RA_adult_ind$primary_complete_RA_bin, useNA = "ifany")
table(combined_RA_adult_ind$lowersec_complete_RA_bin, useNA = "ifany")


combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    HH_Educ07_RA_bin = case_when(
      HH_Educ07_RA == 1 ~ 1,
      is.na(HH_Educ07_RA) ~ NA_real_,
      TRUE ~ 0
    ),
    primary_complete_RA_bin = case_when(
      primary_complete_RA == 1 ~ 1,
      is.na(primary_complete_RA) ~ NA_real_,
      TRUE ~ 0
    ),
    lowersec_complete_RA_bin = case_when(
      lowersec_complete_RA == 1 ~ 1,
      is.na(lowersec_complete_RA) ~ NA_real_,
      TRUE ~ 0
    )
  )


##Do with demographics


# =========================
# 1) FULL REFUGEE MODEL
# outcome: labour_force
# predictors: HH_08, country, gender, age, education
# =========================
df_full <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    !is.na(labour_force), labour_force %in% c(0, 1),
    !is.na(HH_08), !is.na(country), !is.na(HH_02_RA), !is.na(age_cat_lf),
    !is.na(HH_Educ07_RA), !is.na(primary_complete_RA), !is.na(lowersec_complete_RA),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    labour_force = as.numeric(labour_force),
    HH_08      = droplevels(as_factor(HH_08)),
    country    = droplevels(as_factor(country)),
    HH_02_RA   = droplevels(as_factor(HH_02_RA)),
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    HH_Educ07_RA_bin         = if_else(HH_Educ07_RA == 1, 1, 0),
    primary_complete_RA_bin  = if_else(primary_complete_RA == 1, 1, 0),
    lowersec_complete_RA_bin = if_else(lowersec_complete_RA == 1, 1, 0)
  )

des_full <- df_full %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

m_full <- svyglm(
  labour_force ~ HH_08 + country + HH_02_RA + age_cat_lf +
    HH_Educ07_RA_bin + primary_complete_RA_bin + lowersec_complete_RA_bin,
  design = des_full,
  family = quasibinomial()
)

or_full <- tidy(m_full, conf.int = TRUE, exponentiate = TRUE)

g_hh08    <- regTermTest(m_full, ~ HH_08)
g_country <- regTermTest(m_full, ~ country)
g_gender  <- regTermTest(m_full, ~ HH_02_RA)
g_age     <- regTermTest(m_full, ~ age_cat_lf)
g_edu1    <- regTermTest(m_full, ~ HH_Educ07_RA_bin)

# print
or_full
g_hh08; g_country; g_gender; g_age; g_edu1


# =========================
# 2) FEMALE-ONLY MODEL WITH HH_27
# =========================
df_f <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),  # adapt if your coding differs
    !is.na(labour_force), labour_force %in% c(0,1),
    !is.na(age_cat_lf), !is.na(country), !is.na(HH_08),
    !is.na(HH_27), !is.na(HH_Educ07_RA),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    labour_force = as.numeric(labour_force),
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    country    = droplevels(as_factor(country)),
    HH_08      = droplevels(as_factor(HH_08)),
    HH_27_bin  = if_else(HH_27 == 1, 1, if_else(HH_27 == 2, 0, NA_real_)),
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0)
  ) %>%
  filter(!is.na(HH_27_bin)) %>%
  mutate(
    # grouped marital status
    HH_08_grp = case_when(
      HH_08 %in% c("Married", "Non-formal union") ~ "1_union",
      HH_08 %in% c("Separated", "Divorced", "Widow or Widower") ~ "2_prev_union",
      HH_08 %in% c("Never married") ~ "3_never_married",
      TRUE ~ NA_character_
    ),
    HH_08_grp = factor(HH_08_grp, levels = c("1_union", "2_prev_union", "3_never_married"))
  ) %>%
  filter(!is.na(HH_08_grp))

# checks
table(df_f$HH_27_bin, useNA = "ifany")
table(df_f$HH_08_grp, useNA = "ifany")
table(df_f$labour_force, useNA = "ifany")

des_f <- df_f %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# original marital categories
m_f <- svyglm(
  labour_force ~ HH_27_bin + age_cat_lf + country + HH_08 + HH_Educ07_RA_bin,
  design = des_f,
  family = quasibinomial()
)

# grouped marital categories
m_f2 <- svyglm(
  labour_force ~ HH_27_bin + age_cat_lf + country + HH_08_grp + HH_Educ07_RA_bin,
  design = des_f,
  family = quasibinomial()
)

or_f  <- tidy(m_f,  conf.int = TRUE, exponentiate = TRUE)
or_f2 <- tidy(m_f2, conf.int = TRUE, exponentiate = TRUE)

g_hh27_f   <- regTermTest(m_f2, ~ HH_27_bin)
g_hh08grp  <- regTermTest(m_f2, ~ HH_08_grp)

# print
or_f
or_f2
g_hh27_f
g_hh08grp