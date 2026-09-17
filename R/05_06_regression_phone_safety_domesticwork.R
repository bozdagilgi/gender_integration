
# -----------------------------
# A) Build female refugee dataset
# -----------------------------
df_f <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),
    labour_force %in% c(0,1),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0,
    !is.na(age_cat_lf), !is.na(country), !is.na(HH_Educ07_RA), !is.na(HH_27)
  ) %>%
  mutate(
    out_of_lf = if_else(labour_force == 0, 1, 0),
    
    phone_own = case_when(C050b01 == 1 ~ 1, C050b01 == 2 ~ 0, TRUE ~ NA_real_),
    fs01_num  = case_when(FS01 %in% c(1,2,3,4) ~ as.numeric(FS01), TRUE ~ NA_real_), # higher=less safe (if coded that way)
    never_walk_alone = case_when(C160105 == 1 ~ 1, C160105 == 2 ~ 0, TRUE ~ NA_real_),
    any_training     = case_when(EducR13 == 1 ~ 1, EducR13 == 2 ~ 0, TRUE ~ NA_real_),
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    country = droplevels(as_factor(country)),
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0),
    HH_27_bin = case_when(HH_27 == 1 ~ 1, HH_27 == 2 ~ 0, TRUE ~ NA_real_)
  )

# -----------------------------
# B) Domestic decision index
# -----------------------------
dw_vars <- paste0("DW01_", 1:10)

df_f <- df_f %>%
  mutate(across(all_of(dw_vars),
                ~ case_when(
                  .x %in% c(1,2,3) ~ 1,          # respondent empowered
                  .x %in% c(4,5,6,7) ~ 0,        # not empowered
                  .x == 97 ~ NA_real_,
                  TRUE ~ NA_real_
                ),
                .names = "{.col}_emp"
  ))

dw_emp_vars <- paste0(dw_vars, "_emp")

df_f <- df_f %>%
  rowwise() %>%
  mutate(
    dw_n_nonmiss = sum(!is.na(c_across(all_of(dw_emp_vars)))),
    dw_score = sum(c_across(all_of(dw_emp_vars)), na.rm = TRUE),
    dw_prop = if_else(dw_n_nonmiss > 0, dw_score / dw_n_nonmiss, NA_real_)
  ) %>%
  ungroup()

# -----------------------------
# C) Model 1: Core (no domestic decisions)
# -----------------------------
df_core <- df_f %>%
  filter(
    !is.na(phone_own), !is.na(fs01_num), !is.na(never_walk_alone),
    !is.na(any_training),
    !is.na(age_cat_lf), !is.na(country), !is.na(HH_Educ07_RA_bin), !is.na(HH_27_bin)
  )

des_core <- df_core %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

m_core <- svyglm(
  out_of_lf ~ phone_own + fs01_num + never_walk_alone + any_training +
    age_cat_lf + country + HH_Educ07_RA_bin + HH_27_bin,
  design = des_core,
  family = quasibinomial()
)

or_core <- tidy(m_core, conf.int = TRUE, exponentiate = TRUE)

# -----------------------------
# D) Model 2: Domestic decision index only (+ controls)
# -----------------------------
df_dw <- df_f %>%
  filter(
    !is.na(dw_prop),
    !is.na(age_cat_lf), !is.na(country), !is.na(HH_Educ07_RA_bin), !is.na(HH_27_bin)
  )

des_dw <- df_dw %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

m_dw <- svyglm(
  out_of_lf ~ dw_prop + age_cat_lf + country + HH_Educ07_RA_bin + HH_27_bin,
  design = des_dw,
  family = quasibinomial()
)

or_dw <- tidy(m_dw, conf.int = TRUE, exponentiate = TRUE)

# -----------------------------
# E) Optional Model 3: Domestic items separately
# -----------------------------
run_dw_item <- function(v){
  f <- as.formula(paste(
    "out_of_lf ~", v, "+ age_cat_lf + country + HH_Educ07_RA_bin + HH_27_bin"
  ))
  m <- svyglm(f, design = des_dw, family = quasibinomial())
  tidy(m, conf.int = TRUE, exponentiate = TRUE) %>% filter(term == v)
}

res_dw_items <- bind_rows(lapply(dw_emp_vars, run_dw_item))

# -----------------------------
# F) Print key outputs
# -----------------------------
cat("\n--- Model 1 (Core) key terms ---\n")
print(or_core %>% filter(term %in% c("phone_own","fs01_num","never_walk_alone","looked_job_info","any_training")))

cat("\n--- Model 2 (Domestic index) ---\n")
print(or_dw %>% filter(term == "dw_prop"))

cat("\n--- Model 3 (DW items one-by-one) ---\n")
print(res_dw_items)