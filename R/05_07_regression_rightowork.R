
# 1) Check coding first
table(FDS_PAK_2024_RA_adult$variables$JobLegal1, useNA = "ifany")
##to your knowledge d you have a right to work? - it's more about perception


# 2) Build Pakistan female refugee sample
df_legal <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    country %in% c("Pakistan", 1, "PAK"),   # adjust to your country coding
    HH_02_RA %in% c(2, "Female"),
    labour_force %in% c(0,1),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0,
    !is.na(age_cat_lf), !is.na(HH_Educ07_RA), !is.na(HH_27)
  ) %>%
  mutate(
    out_of_lf = if_else(labour_force == 0, 1, 0),
    
    # JobLegal1 recode
    legal_right_work = case_when(
      JobLegal1 == 1 ~ 1,   # yes, has legal right
      JobLegal1 == 2 ~ 0,   # no
      TRUE ~ NA_real_       # DK/refused/etc
    ),
    
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0),
    HH_27_bin = case_when(HH_27 == 1 ~ 1, HH_27 == 2 ~ 0, TRUE ~ NA_real_)
  ) %>%
  filter(!is.na(legal_right_work), !is.na(HH_27_bin))

# 3) Diagnostics
table(df_legal$legal_right_work, useNA="ifany")
table(df_legal$out_of_lf, useNA="ifany")

# 4) Survey design
des_legal <- df_legal %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# 5) Model: out of LF
m_legal <- svyglm(
  out_of_lf ~ legal_right_work + age_cat_lf + HH_Educ07_RA_bin + HH_27_bin,
  design = des_legal,
  family = quasibinomial()
)

tidy(m_legal, conf.int = TRUE, exponentiate = TRUE)