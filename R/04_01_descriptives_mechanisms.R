##Barriers and mechanisms


###Financial inclusion



svyby(
  ~labour_force,
  ~RBM21301,
  design_women,
  svymean,
  na.rm = TRUE
)

svychisq(
  ~labour_force + RBM21301,
  design_women
)

women_ref <- women %>%
  filter(Intro_07 == "Refugees")

design_ref <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = women_ref
)

svyby(
  ~labour_force,
  ~RBM21301,
  design_ref,
  svymean,
  na.rm = TRUE
)


m_ref_fi <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    marital_grp +
    child_u2 +
    educ_level +
    disability_RA +
    RBM21301,
  design = design_ref,
  family = quasibinomial()
)

summary(m_ref_fi)


##Legal right to work----



svyby(
  ~labour_force,
  ~JobLegal1,
  design_women,
  svymean,
  na.rm = TRUE
)


women <- women %>%
  mutate(
    legal_work = case_when(
      JobLegal1 == 1 ~ 1,
      JobLegal1 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

women_ref <- women %>%
  filter(Intro_07 == "Refugees")

design_ref <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = women_ref
)

svyby(
  ~labour_force,
  ~legal_work,
  design_ref,
  svymean,
  na.rm = TRUE
)

svychisq(
  ~labour_force + legal_work,
  design_ref
)


m_ref_legal <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    marital_grp +
    child_u2 +
    educ_level +
    disability_RA +
    legal_work,
  design = design_ref,
  family = quasibinomial()
)

summary(m_ref_legal)


###depression----

table(women_ref$depression, useNA = "ifany")

svyby(
  ~labour_force,
  ~depression,
  design_ref,
  svymean,
  na.rm = TRUE
)

svyby(
  ~labour_force,
  ~depression + marital_grp,
  design_ref,
  svymean,
  na.rm = TRUE
)


svyby(
  ~depression,
  ~marital_grp,
  design_ref,
  svymean,
  na.rm = TRUE
)


m_dep <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    marital_grp +
    legal_work +
    depression,
  design = design_ref,
  family = quasibinomial()
)

summary(m_dep)

m_dep_int <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    marital_grp * depression +
    legal_work,
  design = design_ref,
  family = quasibinomial()
)

summary(m_dep_int)


##decision making mechanisms----

##Decision-making section - check if we can add

#Indicator as here:

#1 always respondent
#2 usually respondent
#3 respondent and spouse equally
#4 usually spouse
#5 always spouse
#6 always or usually other person in the household
#7 always or usually someone else not living in the household
#97 not applicable

table(PAK_RA_adult$MD01_1) #Routine purchase for the HH
table(PAK_RA_adult$MD01_2) #Occasional expensive purchase
table(PAK_RA_adult$MD01_3) #time you spend in paid work
table(PAK_RA_adult$MD01_4) #time spouse spend in paid work
table(PAK_RA_adult$MD01_5) #the way children raised
table(PAK_RA_adult$MD01_6) #your family social life and leisure
table(PAK_RA_adult$MD01_7) #who you spend time with
table(PAK_RA_adult$MD01_8) #who your spouse spend time with
table(PAK_RA_adult$MD01_9) #having children


##if 1 woman has the power on decision-making 



md_vars <- paste0("MD01_", 1:9)

women <- women %>%
  mutate(
    across(
      all_of(md_vars),
      ~ case_when(
        . %in% c(1, 2, 3) ~ 1,
        . %in% c(4, 5, 6, 7) ~ 0,
        TRUE ~ NA_real_
      )
    )
  )

table(women$MD01_1)

table(women$marital_grp)

women_ref_married <- women %>%
  filter(Intro_07 == "Refugees" & marital_grp=="Union")


design_ref <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = women_ref_married
)


#base model ( only for married refugee women)

#Routine purchase for the HH

m_dm1 <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    disability_RA +
    educ_level +
    MD01_1,
  design = design_ref,
  family = quasibinomial()
)

summary(m_dm1)

#Occasional expensive purchase

m_dm2 <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    educ_level +
    MD01_2,
  design = design_ref,
  family = quasibinomial()
)

summary(m_dm2)

#time you spend in paid work

m_dm3 <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    educ_level +
    MD01_3,
  design = design_ref,
  family = quasibinomial()
)

summary(m_dm3)

#who you spend time with

m_dm7 <- svyglm(
  labour_force ~
    country +
    age_cat_lf +
    educ_level +
    MD01_7,
  design = design_ref,
  family = quasibinomial()
)

summary(m_dm7)

#table(PAK_RA_adult$MD01_9) #having children

m_dm9 <- svyglm(
  labour_force ~
    country +
    educ_level +
    MD01_9,
  design = design_ref,
  family = quasibinomial()
)

summary(m_dm9)



##Job search barriers -only asked to those who are in labour force 

table(women_ref$JobSearch11, women_ref$employed) #lack of reading/ writing skills
table(women_ref$JobSearch12) #lack of computer/digital skills
table(women_ref$JobSearch13) #lack of legal documents
table(women_ref$JobSearch14) #discrimination in the labour market


svychisq(~ labour_force + JobSearch11, design_ref)
svychisq(~ labour_force + JobSearch12, design_ref)
svychisq(~ labour_force + JobSearch13, design_ref)
svychisq(~ labour_force + JobSearch14, design_ref)


women_ref <- women_ref %>%
  mutate(
    js_literacy = case_when(
      JobSearch11 == 1 ~ 1,
      JobSearch11 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    js_digital = case_when(
      JobSearch12 == 1 ~ 1,
      JobSearch12 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    js_documents = case_when(
      JobSearch13 == 1 ~ 1,
      JobSearch13 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    js_discrimination = case_when(
      JobSearch14 == 1 ~ 1,
      JobSearch14 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table()


design_ref <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = women_ref
)

#lack of reading and writing skills

m_job1 <- svyglm(
  employed ~
    country +
    age_cat_lf +
    educ_level +
    js_literacy,
  design = design_ref,
  family = quasibinomial()
)

summary(m_job1)


#lack of computer/digital skills

m_job2 <- svyglm(
  employed ~
    country +
    age_cat_lf +
    educ_level +
    js_digital,
  design = design_ref,
  family = quasibinomial()
)

summary(m_job2)

#lack of legal documents



m_job3 <- svyglm(
  employed ~
    country +
    age_cat_lf +
    educ_level +
    js_documents,
  design = design_ref,
  family = quasibinomial()
)

summary(m_job3)

#discrimination in the labour market



m_job4 <- svyglm(
  employed ~
    country +
    age_cat_lf +
    educ_level +
    js_discrimination,
  design = design_ref,
  family = quasibinomial()
)

summary(m_job4)


##Check by gender



combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    js_literacy = case_when(
      JobSearch11 == 1 ~ 1,
      JobSearch11 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    js_digital = case_when(
      JobSearch12 == 1 ~ 1,
      JobSearch12 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    js_documents = case_when(
      JobSearch13 == 1 ~ 1,
      JobSearch13 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    js_discrimination = case_when(
      JobSearch14 == 1 ~ 1,
      JobSearch14 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )


combined_RA_adult_ind <- combined_RA_adult_ind %>%
  filter(Intro_07 == "Refugees" )


design_all <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = combined_RA_adult_ind
)

svyby(
  ~js_discrimination,
  ~HH_02_RA,
  design_all,
  svymean,
  na.rm = TRUE
)


m_barriers <- svyglm(
  employed ~
    country +
    age_cat_lf +
    educ_level +
    HH_02_RA +
    js_literacy +
    js_digital +
    js_documents +
    js_discrimination,
  design = design_all,
  family = quasibinomial()
)

summary(m_barriers)


m_barriers_gender <- svyglm(
  employed ~
    country +
    HH_02_RA +
    js_literacy +
    js_digital +
    js_documents +
    js_discrimination,
  design = design_all,
  family = quasibinomial()
)

summary(m_barriers_gender)


##compare stats

svyby(
  ~js_literacy,
  ~HH_02_RA,
  design_all,
  svymean,
  na.rm = TRUE
)

svyby(
  ~js_digital,
  ~HH_02_RA,
  design_all,
  svymean,
  na.rm = TRUE
)

svyby(
  ~js_documents,
  ~HH_02_RA,
  design_all,
  svymean,
  na.rm = TRUE
)

svyby(
  ~js_discrimination,
  ~HH_02_RA,
  design_all,
  svymean,
  na.rm = TRUE
)


##phone ownership


table(combined_RA_adult_ind$C050b01)

women_ref <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA == "Female"
  )

design_ref <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = women_ref
)


svyby(
  ~labour_force,
  ~C050b01,
  design_ref,
  svymean,
  na.rm = TRUE
)


##now check with overall model



m_phone_gender <- svyglm(
  labour_force ~
    country +
    HH_02_RA +
    C050b01,
  design = design_all,
  family = quasibinomial()
)

summary(m_phone_gender)


##now safety


table(combined_RA_adult_ind$C160105)



m_safety_gender <- svyglm(
  labour_force ~
    country +
    HH_02_RA +
    C160105,
  design = design_all,
  family = quasibinomial()
)

summary(m_safety_gender)




m_safety_gender_v2 <- svyglm(
  labour_force ~
    country +
    Intro_07 +
    C160105,
  design = design_women,
  family = quasibinomial()
)

summary(m_safety_gender_v2)

##Information and training

#Trainings -> check indicator EducR16

##ANY TRAINING in the country of origin or host country

#ANY training that is not part of educational system

table(combined_RA_adult_ind$EducR13)


combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    training = case_when(
      EducR13 == 1 ~ 1,
      EducR13 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )


design_ref <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = combined_RA_adult_ind
)


svyby(
  ~labour_force,
  ~training,
  design_ref,
  svymean,
  na.rm = TRUE
)

m_training <- svyglm(
  labour_force ~
    country +
    HH_02_RA +
    educ_level +
    training,
  design = design_ref,
  family = quasibinomial()
)

summary(m_training)