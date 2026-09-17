combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    Intro_07 = case_when(
      Intro_07 %in% c("Host Community", "Host community") ~ "Host Community",
      TRUE ~ Intro_07
    )
  )

table(combined_RA_adult_ind$Intro_07, useNA = "ifany")

##create other variables 

combined_RA_adult_ind <- combined_RA_adult_ind %>%
  
  mutate(
    
    # Refugee / Host
    refugee_status = factor(
      Intro_07,
      levels = c("Host Community", "Refugees")
    ),
    
    # Gender
    sex = case_when(
      HH_02_RA == 1 ~ "Male",
      HH_02_RA == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    # Disability
    disability = factor(disability_RA),
    
    # Marital status grouped
    marital_grp = case_when(
      HH_08 %in% c("Married", "Non-formal union") ~ "Union",
      HH_08 %in% c("Separated",
                   "Divorced",
                   "Widow or Widower") ~ "Previous_union",
      HH_08 %in% c("Never married") ~ "Never_married",
      TRUE ~ NA_character_
    ),
    
    # Child under 2
    child_u2 = case_when(
      HH_27 == 1 ~ 1,
      HH_27 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Ever attended school
    ever_school = case_when(
      HH_Educ07_RA == 1 ~ 1,
      HH_Educ07_RA == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )


combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    age_cat_lf = cut(
      age_selected,
      breaks = c(15,17, 24, 34, 44, 54, 64, Inf),
      labels = c(
        "15-17",
        "18-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65+"
      )
    )
  )

table(combined_RA_adult_ind$age_cat_lf,
      useNA = "ifany")


combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    
    educ_level = case_when(
      
      ever_school == 0
      ~ "No schooling",
      
      lowersec_complete_RA == 1
      ~ "Lower secondary+",
      
      primary_complete_RA == 1
      ~ "Primary complete",
      
      ever_school == 1
      ~ "Some schooling",
      
      TRUE ~ NA_character_
    )
    
  )


i 

class(combined_RA_adult_ind$HH_02_RA)

table(combined_RA_adult_ind$HH_02_RA, useNA = "ifany")

