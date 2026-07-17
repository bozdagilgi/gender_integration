

# Indicators explicitly listed in your CMR file

indicators_to_keep <- c(
  # IDs / merge keys / disaggregation
  "uuid", "country", "age_selected", "HH_08", "disability_RA", "Intro_07",
  "wgh_samp_pop_restr_resp",
  
  # Labour force indicators
  "employed", "unemployed", "labour_force", "labour_force_status",
  "unpaid_work", "unpaid_work_housecare", "no_job_want", "no_job_search",
  "no_job_availability", "outside_lab_force_potential", "outside_lab_force_no_potential",
  
  # Education
  "HH_Educ07_RA", 
  "primary_complete_RA", "lowersec_complete_RA",
  
  # Skills / experience
  "Exp01a","Exp01c","Exp01d","Exp01e","Exp01f","Exp01g","Exp01h","Exp01i",
  "Exp01j","Exp01k","Exp01l","Exp01m","Exp01n","Exp01o","Exp03",
  "Skill01a","Skills06b","Skills06c","Skills06d",
  "Skills38a","Skills38b","Skills38c",
  "Skills41a","Skills41b","Skills41c","Skills41d",
  "Skills42a","Skills42b",
  
  # Legal right to work
  "JobLegal1",
  
  # Fertility / children
  "HH_27",
  
  # Financial inclusion
  "RBM21301",
  
  # Mental health
  "MH01_1","MH01_2","MH01_3","MH01_4","MH01_5","MH01_6","MH01_7","MH01_8","MH01_9",
  "new_MH01_1","new_MH01_2","new_MH01_3","new_MH01_4","new_MH01_5",
  "new_MH01_6","new_MH01_7","new_MH01_8","new_MH01_9",
  "PHQ9","depression",
  
  # Decision-making
  "MD01_1","MD01_2","MD01_3","MD01_4","MD01_5","MD01_6","MD01_7","MD01_8","MD01_9",
  
  # Job-search barriers
  "JobSearch11","JobSearch12","JobSearch13","JobSearch14",
  
  # Discrimination
  "DI01_01","DI01_02","DI01_03","DI01_04","DI01_05","DI01_06","DI01_07","DI01_08","DI01_09",
  
  # Phone, safety, domestic work, info/training
  "C050b01","FS01","C160105",
  "DW01_1","DW01_2","DW01_3","DW01_4","DW01_5","DW01_6","DW01_7","DW01_8","DW01_9","DW01_10",
  "EducR13",
  
  # Survey design vars used at the end
  "samp_strat","wgh_samp_pop_restr_resp"
)

# Keep only those that exist for all three datasets

CMR_RA_adult_ind <- CMR_RA_adult %>%
  select(any_of(indicators_to_keep))


ZAM_RA_adult_ind <- ZAM_RA_adult %>%
  select(any_of(indicators_to_keep))


PAK_RA_adult_ind <- PAK_RA_adult %>%
  select(any_of(indicators_to_keep))



##Check all


# take label set from a labelled version (CMR here)
lbl <- attr(CMR_RA_adult_ind$C160105, "labels")

PAK_RA_adult_ind <- PAK_RA_adult_ind %>%
  mutate(
    C160105 = labelled(
      as.numeric(C160105),
      labels = lbl
    )
  )

di_vars <- paste0("DI01_0", 1:9)

ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(across(any_of(di_vars), ~ {
    v <- cur_column()
    labelled(
      parse_number(as.character(.x)),
      labels = attr(CMR_RA_adult_ind[[v]], "labels")
    )
  }))


dw_vars <- paste0("DW01_", 1:10)

ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(across(any_of(dw_vars), ~ {
    v <- cur_column()
    labelled(
      parse_number(as.character(.x)),
      labels = attr(CMR_RA_adult_ind[[v]], "labels")  # or PAK template
    )
  }))



lbl_educr13 <- attr(CMR_RA_adult_ind$EducR13, "labels")

ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(
    EducR13 = labelled(
      parse_number(as.character(EducR13)),
      labels = lbl_educr13
    )
  )

exp_vars <- c(
  "Exp01a","Exp01c","Exp01d","Exp01e","Exp01f","Exp01g","Exp01h",
  "Exp01i","Exp01j","Exp01k","Exp01l","Exp01m","Exp01n","Exp01o"
)

ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(across(any_of(exp_vars), ~ {
    v <- cur_column()
    labelled(
      parse_number(as.character(.x)),
      labels = attr(CMR_RA_adult_ind[[v]], "labels")   # or PAK template
    )
  }))



# 1) S01: character -> haven_labelled<double>
lbl_FS01 <- attr(CMR_RA_adult_ind$FS01, "labels")
ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(
    FS01 = labelled(
      parse_number(as.character(FS01)),
      labels = lbl_FS01
    )
  )

# 2) HH_08: character -> factor (same levels/labels as CMR factor)
ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(
    HH_08 = factor(
      HH_08,
      levels = levels(CMR_RA_adult_ind$HH_08),
      labels = levels(CMR_RA_adult_ind$HH_08)
    )
  )

# 3) HH_27: character -> haven_labelled<double>
lbl_HH_27 <- attr(CMR_RA_adult_ind$HH_27, "labels")
ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(
    HH_27 = labelled(
      parse_number(as.character(HH_27)),
      labels = lbl_HH_27
    )
  )

CMR_RA_adult_ind <- CMR_RA_adult_ind %>%
  mutate(HH_Educ07_RA = parse_number(as.character(HH_Educ07_RA)))

ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(HH_Educ07_RA = parse_number(as.character(HH_Educ07_RA)))

PAK_RA_adult_ind <- PAK_RA_adult_ind %>%
  mutate(HH_Educ07_RA = parse_number(as.character(HH_Educ07_RA)))


##Check if there are any differences 

# put datasets in a named list
lst <- list(
  CMR = CMR_RA_adult_ind,
  ZAM = ZAM_RA_adult_ind,
  PAK = PAK_RA_adult_ind
)

# build a table of variable types by dataset
type_table <- imap_dfr(lst, ~{
  tibble(
    dataset = .y,
    variable = names(.x),
    class = map_chr(.x, ~ paste(class(.x), collapse = "|"))
  )
}) %>%
  pivot_wider(names_from = dataset, values_from = class)

# keep only variables where at least one dataset has a different class
type_diff <- type_table %>%
  filter(
    (is.na(CMR) | is.na(ZAM) | is.na(PAK)) |
      !(coalesce(CMR, "") == coalesce(ZAM, "") &
          coalesce(ZAM, "") == coalesce(PAK, ""))
  ) %>%
  arrange(variable)

type_diff



common <- Reduce(intersect, list(
  names(CMR_RA_adult_ind), names(ZAM_RA_adult_ind), names(PAK_RA_adult_ind)
))

CMR2 <- CMR_RA_adult_ind %>% mutate(across(all_of(common), ~ as.character(.)))
ZAM2 <- ZAM_RA_adult_ind %>% mutate(across(all_of(common), ~ as.character(.)))
PAK2 <- PAK_RA_adult_ind %>% mutate(across(all_of(common), ~ as.character(.)))

combined_RA_adult_ind <- bind_rows(CMR2, ZAM2, PAK2)

# Combine all three
combined_RA_adult_ind <- bind_rows(
  CMR_RA_adult_ind,
  ZAM_RA_adult_ind,
  PAK_RA_adult_ind
)

# Checks
dim(combined_RA_adult_ind)
table(combined_RA_adult_ind$country, useNA = "ifany")
names(combined_RA_adult_ind)