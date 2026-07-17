###Data Manipulation for indicators


##Disaggregation variables 


table(FDS_ZAM_2025_RA_adult$variables$HH_02_RA)
table(FDS_ZAM_2025_RA_adult$variables$disability_RA)
table(FDS_ZAM_2025_RA_adult$variables$Intro_07)

##Create a new variable called country for the survey


FDS_ZAM_2025_RA_adult <- FDS_ZAM_2025_RA_adult %>% 
  mutate(country = "Zambia")


### Bring marital status variable to RA_adult dataset



ZAM_RA_adult <- ZAM_RA_adult %>%
  left_join(
    ZAM_HHroster %>% select(`_uuid`, HH_08, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "selected_adultap" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 


table(ZAM_RA_adult$HH_08)


ZAM_RA_adult <- ZAM_RA_adult %>%
  mutate(
    HH_08 = factor(
      HH_08,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married",
        "Non-formal union",
        "Separated",
        "Divorced",
        "Widow or Widower",
        "Never married"
      )
    )
  )



table(ZAM_RA_adult$age_selected)

##Labour force status indicators

##Employment Indicators


## Labour force participation rate

ZAM_RA_adult <- ZAM_RA_adult %>%
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

table(ZAM_RA_adult$labour_force)
table(ZAM_RA_adult$labour_force_status)



table(FDS_ZAM_2025_RA_adult$variables$labour_force_status) # Use as binary variable 


##Now check with the main activity for those in the labour force

# Type of unpaid work


##Recode 


table(ZAM_RA_adult$EMP30)


ZAM_RA_adult <- ZAM_RA_adult %>%
  mutate(
    unpaid_work_housecare = if_else(EMP30 == 2, 1, 0, 
                                    missing = NA_real_)
  )

table(ZAM_RA_adult$unpaid_work_housecare)
table(ZAM_RA_adult$unpaid_work)


##Education indicators from HH roster

#merge with rosterposition and uuid


ZAM_HHroster <- ZAM_HHroster |>
  mutate(
    highest_level_edu = case_when(
      HH_Educ07 == 2 ~ 1,
      HH_Educ18 %in% c(1:6) ~ 2,
      HH_Educ18 %in% c(7:8) ~ 3,
      HH_Educ18 %in% c(9:11, 13) ~ 4,
      HH_Educ18 %in% c(12, 14:19) ~ 5
    ),
    highest_level_edu = labelled(
      highest_level_edu,
      labels = c(
        "None" = 1,
        "Primary" = 2,
        "Lower secondary" = 3,
        "Upper secondary" = 4,
        "Tertiary" = 5
      )
      ))

ZAM_RA_adult <- ZAM_RA_adult %>%
  left_join(
    ZAM_HHroster %>% select(`_uuid`, highest_level_edu, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "selected_adultap" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 


ZAM_RA_adult <- ZAM_RA_adult %>%
  mutate(
    # Primary complete if reached at least Primary (2+)
    primary_complete_RA = case_when(
      is.na(highest_level_edu) ~ NA_real_,
      highest_level_edu >= 2 ~ 1,
      highest_level_edu < 2 ~ 0
    ),
    
    # Lower secondary complete if reached at least Lower secondary (3+)
    lowersec_complete_RA = case_when(
      is.na(highest_level_edu) ~ NA_real_,
      highest_level_edu >= 3 ~ 1,
      highest_level_edu < 3 ~ 0
    )
  )

table(ZAM_RA_adult$HH_Educ07_RA) #have you ever been to school? 
table(ZAM_RA_adult$highest_level_edu)
table(ZAM_RA_adult$primary_complete_RA)
table(ZAM_RA_adult$lowersec_complete_RA)


#Skills


#Check tables

##Skills -1 Yes 2 No

table(FDS_ZAM_2025_RA_adult$variables$Exp01a) #Caring for children
table(FDS_ZAM_2025_RA_adult$variables$Exp01c) #Caring for sick or disabled people
table(FDS_ZAM_2025_RA_adult$variables$Exp01d) #Making/mending clothing
table(FDS_ZAM_2025_RA_adult$variables$Exp01e) #Preparing meals / baking
table(FDS_ZAM_2025_RA_adult$variables$Exp01f) #Cultivating crops
table(FDS_ZAM_2025_RA_adult$variables$Exp01g) #Taking care of livestock
table(FDS_ZAM_2025_RA_adult$variables$Exp01h) #Fishing
table(FDS_ZAM_2025_RA_adult$variables$Exp01i) #Handicraft activities
table(FDS_ZAM_2025_RA_adult$variables$Exp01j) #Selling or trading products
table(FDS_ZAM_2025_RA_adult$variables$Exp01k) #Making furniture
table(FDS_ZAM_2025_RA_adult$variables$Exp01l) #House construction
table(FDS_ZAM_2025_RA_adult$variables$Exp01m) #Coaching/teaching
table(FDS_ZAM_2025_RA_adult$variables$Exp01n) #Photography/film-making
table(FDS_ZAM_2025_RA_adult$variables$Exp01o) #Hairdressing/aesthetic care


##HAVE YOU EVER RUN A BUSINESS?

table(FDS_ZAM_2025_RA_adult$variables$Exp03) #Ever run a business

###Reading/writing skills

table(FDS_ZAM_2025_RA_adult$variables$Skill01a) #Can you read following items easily 
#Phone messages on platforms like WhatsApp,Messenger,Instagram
#1. Easily
#2. Withsomedifficulty
#3. Withlotsofdifficulty
#4. Notatall


table(FDS_ZAM_2025_RA_adult$variables$Skills06b) #Calculateprices 
table(FDS_ZAM_2025_RA_adult$variables$Skills06c) #Performanyothermultiplicationordivision 
table(FDS_ZAM_2025_RA_adult$variables$Skills06d) #useorcalculatefractions,decimalsorpercentages


table(FDS_ZAM_2025_RA_adult$variables$Skills38a) #use mobile phone
table(FDS_ZAM_2025_RA_adult$variables$Skills38b) #use smart phone
table(FDS_ZAM_2025_RA_adult$variables$Skills38c) #a tablet
table(FDS_ZAM_2025_RA_adult$variables$Skills39) #used computer in the past 3 months
table(FDS_ZAM_2025_RA_adult$variables$Skills41a) #send receive emails
table(FDS_ZAM_2025_RA_adult$variables$Skills41b) #making internet/video calls
table(FDS_ZAM_2025_RA_adult$variables$Skills41c) #finding information on internet
table(FDS_ZAM_2025_RA_adult$variables$Skills41d) #learning/studying online


table(FDS_ZAM_2025_RA_adult$variables$Skills42a) #driving license to drive a car
table(FDS_ZAM_2025_RA_adult$variables$Skills42b) #driving license to drive a truck



##To your knowledge, do you have the right to work legally in Zambia?

table(FDS_ZAM_2025_RA_adult$variables$JobLegal1) #to your knowledge d you have a right to work?


##If they have a children under 2 years old - or any live birth in the last 2 years


# Not possible to calculate as randomly selected adult is not always the spouse -

#Women with children under 2
#Women with children under 5

##We can instead use the question ->Did you/${membName} give birth to a child who was bornaliveinthepast2years?

table(ZAM_HHroster$HH_27)


table(FDS_ZAM_2025_HHroster$variables$HH_27) #Giver birth to a live child in the past 2 years

#merge with rosterposition and uuid
ZAM_RA_adult <- ZAM_RA_adult %>%
  left_join(
    ZAM_HHroster %>% select(`_uuid`, HH_27, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "selected_adultap" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 

table(ZAM_RA_adult$HH_27) # Only available for women - 


#Ownership of a bank account or any financial account

#We can use the RBM indicator which is Proportion of people with an account at a bank or other financial institution or with a mobile-money provider
table(ZAM_RA_adult$RBM21301)


##depression rates


ZAM_RA_adult <- ZAM_RA_adult %>%
  mutate(across(starts_with("MH01_"), 
                ~ ifelse(. == 99, NA_real_, as.numeric(.)), 
                .names = "new_{.col}"))

head(ZAM_RA_adult %>% select(starts_with("MH01"), starts_with("new_MH01"))) # View the output




ZAM_RA_adult <- ZAM_RA_adult %>%
  mutate(PHQ9 = new_MH01_1 + new_MH01_2 + new_MH01_3 + new_MH01_4 +
           new_MH01_5 + new_MH01_6 + new_MH01_7 + new_MH01_8 + new_MH01_9)


# Create the depression variable
ZAM_RA_adult <- ZAM_RA_adult %>%
  mutate(depression = ifelse(PHQ9 >= 10, 1, 0)) #1=depressed, 0=not depressed


table(ZAM_RA_adult$depression)


###Decision-making section - check if we can add

#Indicator as here:

#1 always respondent
#2 usually respondent
#3 respondent and spouse equally
#4 usually spouse
#5 always spouse
#6 always or usually other person in the household
#7 always or usually someone else not living in the household
#97 not applicable

table(ZAM_RA_adult$MD01_1) #Routine purchase for the HH
table(ZAM_RA_adult$MD01_2) #Occasional expensive purchase
table(ZAM_RA_adult$MD01_3) #time you spend in paid work
table(ZAM_RA_adult$MD01_4) #time spouse spend in paid work
table(ZAM_RA_adult$MD01_5) #the way children raised
table(ZAM_RA_adult$MD01_6) #your family social life and leisure
table(ZAM_RA_adult$MD01_7) #who you spend time with
table(ZAM_RA_adult$MD01_8) #who your spouse spend time with
table(ZAM_RA_adult$MD01_9) #having children

#Job search barriers + lack of reading, computer literacy + legal literacy +
#1 yes 2 no


##Does not exist in Zambia dataset
table(ZAM_RA_adult$JobSearch1.1) #lack of reading/ writing skills
table(ZAM_RA_adult$JobSearch1.2) #lack of computer/digital skills
table(ZAM_RA_adult$JobSearch1.3) #lack of legal documents
table(ZAM_RA_adult$JobSearch1.4) #discrimination in the labour market

ZAM_RA_adult <- ZAM_RA_adult %>%
  rename(
    JobSearch11 = `JobSearch1.1`,
    JobSearch12 = `JobSearch1.2`,
    JobSearch13 = `JobSearch1.3`,
    JobSearch14 = `JobSearch1.4`
  )
#Discrimination
#1 almost everyday
#2 at least once a week
#3 a few times a month
#4 a few times a year
#5 once a year
#6 never
#7 i dont know
table(ZAM_RA_adult$DI01_01) #treated with less politeness
table(ZAM_RA_adult$DI01_02) #treated with less respect
table(ZAM_RA_adult$DI01_03) #receive poorer service in restaurants
table(ZAM_RA_adult$DI01_04) #people think that you are not smart
table(ZAM_RA_adult$DI01_05) #people act as if they are afraid of you
table(ZAM_RA_adult$DI01_06) #people act as if you are dishonest
table(ZAM_RA_adult$DI01_07) #people act as if they are better than you
table(ZAM_RA_adult$DI01_08) #you are called names or insulted
table(ZAM_RA_adult$DI01_09) #threatened or harrassed

#Ownership of a mobile phone 
#Its an SDG indicator

table(ZAM_RA_adult$C050b01) #when an individual has a mobile phone with active sim card


#Safety feeling + if you cannot move in the evening.
#Proportion of population that feel safe walking alone around the area they live after dark**

table(ZAM_RA_adult$FS01) #feeling safe walking alone in the dark

#Very safe
#Fairly safe
#bit unsafe
#very unsafe
#i never walk alone
#dont know

table(ZAM_RA_adult$C160105)

#Time spent on domestic work

#1 Always respondent 
#2 usually repondent
#3respondent and spouse equally
#4 usually spouse
#5 always spouse
#6 always or usually other person in the household
#7always or usually someone else not living in the household
#97 not applicable

table(ZAM_RA_adult$DW01_6) # Make repairs to your home, furniture, vehicle and applicances
table(ZAM_RA_adult$DW01_2) # Do the laundry and wash cloth
table(ZAM_RA_adult$DW01_4) #shop for the household
table(ZAM_RA_adult$DW01_3) #cook
table(ZAM_RA_adult$DW01_7) #collect water for your household
table(ZAM_RA_adult$DW01_8) #collect firewood for the household
table(ZAM_RA_adult$DW01_10) #take care of elderly or disabled
table(ZAM_RA_adult$DW01_9) #take care of children
table(ZAM_RA_adult$DW01_1) #clean the house
table(ZAM_RA_adult$DW01_5) #manage household finances

#Information about job

#In the last 30 days did you look information about job opportunities

table(ZAM_RA_adult$INFO00_3) # not in the dataset

#Trainings -> check indicator EducR16

##ANY TRAINING in the country of origin or host country

#ANY training that is not part of educational system

table(ZAM_RA_adult$EducR13)

###All indicators are here.

FDS_ZAM_2025_RA_adult <- ZAM_RA_adult %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_resp, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )