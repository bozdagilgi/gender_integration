
#Job search barriers + lack of reading, computer literacy + legal literacy +
#1 yes 2 no

table(PAK_RA_adult$JobSearch11) #lack of reading/ writing skills
table(PAK_RA_adult$JobSearch12) #lack of computer/digital skills
table(PAK_RA_adult$JobSearch13) #lack of legal documents
table(PAK_RA_adult$JobSearch14) #discrimination in the labour market



# 1) Female refugee sample
df_js <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),
    !is.na(labour_force), labour_force %in% c(0,1),
    !is.na(JobSearch11), !is.na(JobSearch12), !is.na(JobSearch13), !is.na(JobSearch14),
    !is.na(age_cat_lf), !is.na(country),
    !is.na(HH_Educ07_RA), !is.na(HH_27),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    out_of_lf = if_else(labour_force == 0, 1, 0),
    
    # 1=yes, 2=no  --> binary barrier indicators
    js11_reading_barrier = if_else(JobSearch11 == 1, 1, if_else(JobSearch11 == 2, 0, NA_real_)),
    js12_digital_barrier = if_else(JobSearch12 == 1, 1, if_else(JobSearch12 == 2, 0, NA_real_)),
    js13_legaldoc_barrier= if_else(JobSearch13 == 1, 1, if_else(JobSearch13 == 2, 0, NA_real_)),
    js14_discrim_barrier = if_else(JobSearch14 == 1, 1, if_else(JobSearch14 == 2, 0, NA_real_)),
    
    # controls
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    country    = droplevels(as_factor(country)),
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0),
    HH_27_bin  = if_else(HH_27 == 1, 1, if_else(HH_27 == 2, 0, NA_real_))
  ) %>%
  filter(
    !is.na(js11_reading_barrier), !is.na(js12_digital_barrier),
    !is.na(js13_legaldoc_barrier), !is.na(js14_discrim_barrier),
    !is.na(HH_27_bin)
  )

# quick checks
table(df_js$js11_reading_barrier, useNA="ifany")
table(df_js$js12_digital_barrier, useNA="ifany")
table(df_js$js13_legaldoc_barrier, useNA="ifany")
table(df_js$js14_discrim_barrier, useNA="ifany")

# 2) Survey design
des_js <- df_js %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# 3) Joint model (all barriers together)
m_js_all <- svyglm(
  out_of_lf ~ js11_reading_barrier + js12_digital_barrier + js13_legaldoc_barrier + js14_discrim_barrier +
    age_cat_lf + country + HH_Educ07_RA_bin + HH_27_bin,
  design = des_js,
  family = quasibinomial()
)

or_js_all <- tidy(m_js_all, conf.int = TRUE, exponentiate = TRUE)
or_js_all

# 4) Item-specific adjusted models (more stable, one barrier at a time)
run_js <- function(v){
  f <- as.formula(paste(
    "out_of_lf ~", v, "+ age_cat_lf + country + HH_Educ07_RA_bin + HH_27_bin"
  ))
  m <- svyglm(f, design = des_js, family = quasibinomial())
  tidy(m, conf.int = TRUE, exponentiate = TRUE) %>% filter(term == v)
}

js_vars <- c("js11_reading_barrier","js12_digital_barrier","js13_legaldoc_barrier","js14_discrim_barrier")
res_js_one <- bind_rows(lapply(js_vars, run_js))
res_js_one

#Job search is filtered for labour force - check for unemployment


df_u <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),
    labour_force == 1,
    unemployed %in% c(0,1),
    wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    js11_reading_barrier = case_when(JobSearch11 == 1 ~ 1, JobSearch11 == 2 ~ 0, TRUE ~ NA_real_),
    js12_digital_barrier = case_when(JobSearch12 == 1 ~ 1, JobSearch12 == 2 ~ 0, TRUE ~ NA_real_),
    js13_legaldoc_barrier = case_when(JobSearch13 == 1 ~ 1, JobSearch13 == 2 ~ 0, TRUE ~ NA_real_),
    js14_discrim_barrier = case_when(JobSearch14 == 1 ~ 1, JobSearch14 == 2 ~ 0, TRUE ~ NA_real_),
    
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    country = droplevels(as_factor(country)),
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0),
    HH_27_bin = case_when(HH_27 == 1 ~ 1, HH_27 == 2 ~ 0, TRUE ~ NA_real_)
  ) %>%
  filter(
    !is.na(js11_reading_barrier), !is.na(js12_digital_barrier),
    !is.na(js13_legaldoc_barrier), !is.na(js14_discrim_barrier),
    !is.na(age_cat_lf), !is.na(country), !is.na(HH_Educ07_RA_bin), !is.na(HH_27_bin)
  )

des_u <- df_u %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

m_u_all <- svyglm(
  unemployed ~ js11_reading_barrier + js12_digital_barrier + js13_legaldoc_barrier + js14_discrim_barrier +
    age_cat_lf + country + HH_Educ07_RA_bin + HH_27_bin,
  design = des_u,
  family = quasibinomial()
)

or_u_all <- tidy(m_u_all, conf.int = TRUE, exponentiate = TRUE)
or_u_all %>% filter(grepl("^js1[1-4]_", term))

##Check for Pakistan

df_pk <- df_u %>% filter(country %in% c("Pakistan", 1, "PAK"))
des_pk <- df_pk %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

m_pk <- svyglm(
  unemployed ~ js11_reading_barrier + js12_digital_barrier + js13_legaldoc_barrier + js14_discrim_barrier +
    age_cat_lf + HH_Educ07_RA_bin + HH_27_bin,
  design = des_pk,
  family = quasibinomial()
)

tidy(m_pk, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("^js1[1-4]_", term))

##any barrier


df_u2 <- df_u %>%
  mutate(any_barrier = if_else(
    js11_reading_barrier==1 | js12_digital_barrier==1 | js13_legaldoc_barrier==1 | js14_discrim_barrier==1, 1, 0
  ))

des_u2 <- df_u2 %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

m_any <- svyglm(
  unemployed ~ any_barrier + age_cat_lf + country + HH_Educ07_RA_bin + HH_27_bin,
  design = des_u2,
  family = quasibinomial()
)

tidy(m_any, conf.int = TRUE, exponentiate = TRUE)
