

# 1) Female refugee sample
df_dep <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),   # adjust coding if needed
    !is.na(depression),
    !is.na(labour_force),
    labour_force %in% c(0,1),
    !is.na(age_cat_lf),
    !is.na(country),
    !is.na(HH_08),
    !is.na(HH_Educ07_RA),
    !is.na(HH_27),
    !is.na(wgh_samp_pop_restr_resp),
    wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    # outcome (edit coding after table check)
    depression_bin = if_else(depression == 1, 1,
                             if_else(depression == 0, 0, NA_real_)),
    
    # key exposure
    out_of_lf = if_else(labour_force == 0, 1, 0),
    
    # controls
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
  filter(!is.na(depression_bin), !is.na(HH_27_bin), !is.na(HH_08_grp))

# IMPORTANT: verify depression coding first
table(combined_RA_adult_ind$depression, useNA="ifany")
table(df_dep$depression_bin, useNA="ifany")
table(df_dep$out_of_lf, useNA="ifany")

# 2) Survey design
des_dep <- df_dep %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# 3) Main model: depression ~ out_of_lf + confounders
m_dep <- svyglm(
  depression_bin ~ out_of_lf + age_cat_lf + country + HH_08_grp + HH_Educ07_RA_bin + HH_27_bin,
  design = des_dep,
  family = quasibinomial()
)

summary(m_dep)
or_dep <- tidy(m_dep, conf.int = TRUE, exponentiate = TRUE)
or_dep

# key effect
or_dep %>% filter(term == "out_of_lf")