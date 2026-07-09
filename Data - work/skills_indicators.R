###Skills Indicators

###We will use the random adult dataset for this part of the analysis

#Start with Pakistan

#FDS_PAK_2024_RA_adult
#FDS_CMR_2024_RA_adult
#FDS_ZAM_2025_RA_adult

table(FDS_PAK_2024_RA_adult$variables$HH_02_RA)
table(FDS_PAK_2024_RA_adult$variables$disability_RA)
table(FDS_PAK_2024_RA_adult$variables$Intro_07)

#Potential sections to be include

#Education from HHroster

table(FDS_PAK_2024_HHroster$variables$HH_Educ18) #Highest level of education

#merge with rosterposition and uuid



table(PAK_HHroster$rosterposition)
table(PAK_RA_adult$rosterposition)


PAK_RA_adult <- PAK_RA_adult %>%
  mutate(rosterposition = as.numeric(rosterposition)) %>%
  left_join(
    PAK_HHroster %>%
      mutate(rosterposition = as.numeric(rosterposition)) %>%
      select(uuid, rosterposition, HH_Educ18, HH_Educ23, HH_Educ07),
    by = c("uuid", "rosterposition")
  )

##Employment Indicators


table(FDS_PAK_2024_RA_adult$variables$labour_force)


## Labour force participation rate

PAK_RA_adult <- PAK_RA_adult %>%
  mutate(unpaid_work = case_when(
    unemployed == 0 & employed == 0 & EMP30 %in% c(2,5,6) ~ 1,  
    TRUE ~ 0
  )) |>
  mutate(no_job_want = case_when(
    unemployed == 0 & employed == 0 & EMP27 == 2 ~ 1,  
    TRUE ~ 0
  )) |>
  mutate(no_job_search = case_when(
    unemployed == 0 & employed == 0 &  EMP27 == 1 & EMP25a == 2 & EMP25aa == 2 ~ 1,  
    TRUE ~ 0
  )) |>
  mutate(no_job_availability = case_when(
    unemployed == 0 & employed == 0 &  EMP27 == 1 & EMP25a == 1 & EMP25aa == 1 & EMP29 == 2 ~ 1,  
    TRUE ~ 0
  )) |>
  mutate(outside_lab_force_potential = case_when(EMP27 == 1 & EMP29 == 1 & EMP25a == 2 & EMP25aa == 2 ~ 1, # Available for a job but not searching
                                                 TRUE ~ 0)) |>
  mutate(outside_lab_force_no_potential = case_when(unemployed == 0 & employed == 0 & outside_lab_force_potential == 0 ~ 1,
                                                    TRUE ~ 0)) |>
  mutate(labour_force_status = case_when(
    employed == 1 ~ 1,
    unemployed == 1 ~ 2,
    outside_lab_force_potential == 1 ~ 3,
    outside_lab_force_no_potential == 1 ~ 4,
    TRUE ~ NA_real_
  )) |>
  mutate(labour_force_status = labelled(labour_force_status,
                                        labels = c("Employed" = 1,
                                                   "Unemployed" = 2,
                                                   "Outside labour force - unavailable - potential labour force (available but not looking for a job)" = 3,
                                                   "Outside labour force - unavailable" = 4)))

FDS_PAK_2024_RA_adult <- PAK_RA_adult %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_resp, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

table(FDS_PAK_2024_RA_adult$variables$labour_force_status) # Use as binary variable 


##Now check with the main activity for those in the labour force


# Type of unpaid work

hhwork_G <- FDS_PAK_2024_RA_adult %>%
  filter(EMP30 %in% c(2,5,6), !is.na(EMP30)) %>%
  mutate(hhwork = case_when(EMP30 == 2 ~ 1,
                            EMP30 != 2 ~ 0)) %>%
  filter(!is.na(HH_02_RA), !is.na(Intro_07)) %>%     # Exclude if pop groups is NA
  group_by(Intro_07, HH_02_RA) %>%      # Show results disaggregated by pop groups
  summarise(
    var_name = "Household/care work",
    num_obs_uw = n(),
    denominator = survey_total(),
    mean_value = survey_mean(hhwork, vartype = c("ci", "se"), na.rm = TRUE)
  )

farming_G <- FDS_PAK_2024_RA_adult %>%
  filter(EMP30 %in% c(2,5,6), !is.na(EMP30)) %>%
  mutate(farming = case_when(EMP30 == 5 ~ 1,
                             EMP30 != 5 ~ 0)) %>%
  filter(!is.na(HH_02_RA), !is.na(Intro_07)) %>%     # Exclude if pop groups is NA
  group_by(Intro_07, HH_02_RA) %>%      # Show results disaggregated by pop groups
  summarise(
    var_name = "Farming",
    num_obs_uw = n(),
    denominator = survey_total(),
    mean_value = survey_mean(farming, vartype = c("ci", "se"), na.rm = TRUE)
  )

volunteering_G <- FDS_PAK_2024_RA_adult %>%
  filter(EMP30 %in% c(2,5,6), !is.na(EMP30)) %>%
  mutate(volunteering = case_when(EMP30 == 6 ~ 1,
                                  EMP30 != 6 ~ 0)) %>%
  filter(!is.na(HH_02_RA), !is.na(Intro_07)) %>%     # Exclude if pop groups is NA
  group_by(Intro_07, HH_02_RA) %>%      # Show results disaggregated by pop groups
  summarise(
    var_name = "Volunteering",
    num_obs_uw = n(),
    denominator = survey_total(),
    mean_value = survey_mean(volunteering, vartype = c("ci", "se"), na.rm = TRUE)
  )

# Bind the dataframes together
unpaid_combined_G <- bind_rows(hhwork_G, farming_G, volunteering_G)

unpaid_combined_G <- unpaid_combined_G  %>%
  group_by(Intro_07, HH_02_RA) %>%
  arrange(desc(var_name)) %>%
  mutate(cumulative_sum = cumsum(mean_value) - (mean_value / 2))  |>
  mutate(HH_02_RA = labelled::to_factor(HH_02_RA))


###Skills


#Check tables

##Skills -1 Yes 2 No

table(FDS_PAK_2024_RA_adult$variables$Exp01a) #Caring for children
table(FDS_PAK_2024_RA_adult$variables$Exp01c) #Caring for sick or disabled people
table(FDS_PAK_2024_RA_adult$variables$Exp01d) #Making/mending clothing
table(FDS_PAK_2024_RA_adult$variables$Exp01e) #Preparing meals / baking
table(FDS_PAK_2024_RA_adult$variables$Exp01f) #Cultivating crops
table(FDS_PAK_2024_RA_adult$variables$Exp01g) #Taking care of livestock
table(FDS_PAK_2024_RA_adult$variables$Exp01h) #Fishing
table(FDS_PAK_2024_RA_adult$variables$Exp01i) #Handicraft activities
table(FDS_PAK_2024_RA_adult$variables$Exp01j) #Selling or trading products
table(FDS_PAK_2024_RA_adult$variables$Exp01k) #Making furniture
table(FDS_PAK_2024_RA_adult$variables$Exp01l) #House construction
table(FDS_PAK_2024_RA_adult$variables$Exp01m) #Coaching/teaching
table(FDS_PAK_2024_RA_adult$variables$Exp01n) #Photography/film-making
table(FDS_PAK_2024_RA_adult$variables$Exp01o) #Hairdressing/aesthetic care




##HAVE YOU EVER RUN A BUSINESS?

table(FDS_PAK_2024_RA_adult$variables$Exp03) #Ever run a business



###Reading/writing skills

table(FDS_PAK_2024_RA_adult$variables$Skill01a) #Can you read following items easily 
#Phone messages on platforms like WhatsApp,Messenger,Instagram
#1. Easily
#2. Withsomedifficulty
#3. Withlotsofdifficulty
#4. Notatall


table(FDS_PAK_2024_RA_adult$variables$Skills06b) #Calculateprices 
table(FDS_PAK_2024_RA_adult$variables$Skills06c) #Performanyothermultiplicationordivision 
table(FDS_PAK_2024_RA_adult$variables$Skills06d) #useorcalculatefractions,decimalsorpercentages


table(FDS_PAK_2024_RA_adult$variables$Skills38a) #use mobile phone
table(FDS_PAK_2024_RA_adult$variables$Skills38b) #use smart phone
table(FDS_PAK_2024_RA_adult$variables$Skills38c) #a tablet
table(FDS_PAK_2024_RA_adult$variables$Skills39) #used computer in the past 3 months
table(FDS_PAK_2024_RA_adult$variables$Skills41a) #send receive emails
table(FDS_PAK_2024_RA_adult$variables$Skills41b) #making internet/video calls
table(FDS_PAK_2024_RA_adult$variables$Skills41c) #finding information on internet
table(FDS_PAK_2024_RA_adult$variables$Skills41d) #learning/studying online


table(FDS_PAK_2024_RA_adult$variables$Skills42a) #driving license to drive a car
table(FDS_PAK_2024_RA_adult$variables$Skills42b) #driving license to drive a truck

#add job search sction
table(FDS_ZAM_2025_RA_adult$variables$PL01_10) #driving license to drive a truck


##To your knowledge, do you have the right to work legally in Pakistan?

table(FDS_PAK_2024_RA_adult$variables$JobLegal1) #to your knowledge d you have a right to work?



##Regression by skills as above ? by Gender ? Regression also based on legal framework
###Add below indicators to the regression

#Women with children under 2
#Women with children under 5
#Education level
#Literacy
#Trainings -> check indicator EducR16
#Ownership of a bank account or any financial account
#Depression rates
#Decision-making section - check if we can add
#If any of the languages in the host country is spoken 
#Job search barriers + lack of reading, computer literacy + legal literacy +
#Discrimination
#Documentation linked to gender
#Ownership of a mobile phone 
#Safety feeling + if you cannot move in the evening.
#Domestic work
#Safety at work ( from protection section)
#Documentation -that is any valid ID document? Also, work permits 
