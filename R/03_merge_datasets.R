

# Indicators explicitly listed in your CMR file

indicators_to_keep <- c(
  # IDs / merge keys / disaggregation
  "uuid", "country", "age_selected", "HH_08", "disability_RA", "Intro_07",
  "wgh_samp_pop_restr_resp", "HH_02_RA",
  
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
  "C200204",
  
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


# build a table of variable types by dataset

# 1) create a fresh object name (not lst)
df_list <- list(
  CMR = CMR_RA_adult_ind,
  ZAM = ZAM_RA_adult_ind,
  PAK = PAK_RA_adult_ind
)

# 2) confirm it's a list
class(df_list)
# should include "list"

# 3) build type table without imap/map2 ambiguity
type_long <- bind_rows(
  lapply(names(df_list), function(nm) {
    dat <- df_list[[nm]]
    tibble(
      dataset = nm,
      variable = names(dat),
      class = sapply(dat, function(col) paste(class(col), collapse = "|"))
    )
  })
)

type_table <- type_long %>%
  pivot_wider(names_from = dataset, values_from = class)

type_diff <- type_table %>%
  filter(
    (is.na(CMR) | is.na(ZAM) | is.na(PAK)) |
      !(coalesce(CMR, "") == coalesce(ZAM, "") &
          coalesce(ZAM, "") == coalesce(PAK, ""))
  ) %>%
  arrange(variable)

type_diff


#Corrections



lbl <- attr(CMR_RA_adult_ind$C160105, "labels")

PAK_RA_adult_ind <- PAK_RA_adult_ind %>%
  mutate(
    C160105 = labelled(
      as.numeric(C160105),
      labels = lbl
    )
  )
sapply(
  list(
    CMR = CMR_RA_adult_ind$C160105,
    ZAM = ZAM_RA_adult_ind$C160105,
    PAK = PAK_RA_adult_ind$C160105
  ),
  class
)


#library(dplyr)


vars_fix_zam <- c(
  # DI
  "DI01_03","DI01_04","DI01_05","DI01_06","DI01_07","DI01_08","DI01_09","DI01_01", "DI01_02",
  # DW
  "DW01_1","DW01_2","DW01_3","DW01_4","DW01_5","DW01_6","DW01_7","DW01_8","DW01_9","DW01_10",
  # Other listed
  "EducR13",
  "Exp01a","Exp01c","Exp01d","Exp01e","Exp01f","Exp01g","Exp01h","Exp01i",
  "Exp01j","Exp01k","Exp01l","Exp01m","Exp01n","Exp01o","Exp03",
  "FS01","HH_27","HH_Educ07_RA","JobLegal1",
  "JobSearch11","JobSearch12","JobSearch13","JobSearch14",
  "MD01_1","MD01_2","MD01_3","MD01_4","MD01_5","MD01_6","MD01_7","MD01_8","MD01_9",
  "MH01_1","MH01_2","MH01_3","MH01_4","MH01_5","MH01_6","MH01_7","MH01_8","MH01_9",
  "Skill01a","Skills06b","Skills06c","Skills06d",
  "Skills38a","Skills38b","Skills38c",
  "Skills41a","Skills41b","Skills41c","Skills41d",
  "Skills42a","Skills42b"
)

# keep only vars that exist in both ZAM and CMR (safe)
vars_fix_zam <- intersect(vars_fix_zam, names(ZAM_RA_adult_ind))
vars_fix_zam <- intersect(vars_fix_zam, names(CMR_RA_adult_ind))

ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>%
  mutate(across(all_of(vars_fix_zam), ~{
    v <- cur_column()
    haven::labelled(
      readr::parse_number(as.character(.x)),
      labels = attr(CMR_RA_adult_ind[[v]], "labels")
    )
  }))

sapply(ZAM_RA_adult_ind[vars_fix_zam], class)


all_levels <- union(
  levels(CMR_RA_adult_ind$samp_strat),
  unique(as.character(PAK_RA_adult_ind$samp_strat))
)

CMR_RA_adult_ind <- CMR_RA_adult_ind %>% mutate(samp_strat = factor(as.character(samp_strat), levels = all_levels))
ZAM_RA_adult_ind <- ZAM_RA_adult_ind %>% mutate(samp_strat = factor(as.character(samp_strat), levels = all_levels))
PAK_RA_adult_ind <- PAK_RA_adult_ind %>% mutate(samp_strat = factor(as.character(samp_strat), levels = all_levels))


##Now merge 
combined_RA_adult_ind <- bind_rows(CMR_RA_adult_ind, ZAM_RA_adult_ind, PAK_RA_adult_ind)