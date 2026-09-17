
# -----------------------------
# 1) Build women sample
# -----------------------------
dm_vars <- paste0("MD01_", 1:9)

df_dm_lf <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),
    !is.na(labour_force), labour_force %in% c(0,1),
    !is.na(age_cat_lf), !is.na(country), !is.na(HH_08),
    !is.na(HH_Educ07_RA), !is.na(HH_27),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0
  ) %>%
  mutate(
    out_of_lf = if_else(labour_force == 0, 1, 0),
    
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
  )

# -----------------------------
# 2) Recode decision-making
# Empowered decision = 1,2,3 ; less empowered = 4,5,6,7 ; 97=NA
# -----------------------------
df_dm_lf <- df_dm_lf %>%
  mutate(across(all_of(dm_vars),
                ~ case_when(
                  .x %in% c(1,2,3) ~ 1,
                  .x %in% c(4,5,6,7) ~ 0,
                  .x %in% c(97) ~ NA_real_,
                  TRUE ~ NA_real_
                ),
                .names = "{.col}_emp"
  ))

dm_emp_vars <- paste0(dm_vars, "_emp")

# Check distributions
lapply(df_dm_lf[dm_emp_vars], table, useNA = "ifany")

# -----------------------------
# 3) Create index
# -----------------------------
df_dm_lf <- df_dm_lf %>%
  rowwise() %>%
  mutate(
    dm_score = sum(c_across(all_of(dm_emp_vars)), na.rm = TRUE),       # 0..9
    dm_n_nonmiss = sum(!is.na(c_across(all_of(dm_emp_vars)))),
    dm_prop = if_else(dm_n_nonmiss > 0, dm_score / dm_n_nonmiss, NA_real_)  # 0..1
  ) %>%
  ungroup() %>%
  filter(!is.na(dm_prop), !is.na(HH_27_bin), !is.na(HH_08_grp))

# optional categories
df_dm_lf <- df_dm_lf %>%
  mutate(
    dm_tertile = ntile(dm_prop, 3),
    dm_tertile = factor(dm_tertile, levels = 1:3, labels = c("Low","Medium","High"))
  )

# -----------------------------
# 4) Survey design
# -----------------------------
des_dm_lf <- df_dm_lf %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# -----------------------------
# 5A) Main model (continuous index)
# -----------------------------

# 1) Ensure proper types
df_dm_lf2 <- df_dm_lf %>%
  mutate(
    out_of_lf = as.numeric(out_of_lf),
    dm_prop = as.numeric(dm_prop),
    HH_Educ07_RA_bin = as.numeric(HH_Educ07_RA_bin),
    HH_27_bin = as.numeric(HH_27_bin),
    age_cat_lf = droplevels(as.factor(age_cat_lf)),
    country = droplevels(as.factor(country)),
    HH_08_grp = droplevels(as.factor(HH_08_grp))
  )

# 2) Print levels/counts
cat("age_cat_lf levels:", nlevels(df_dm_lf2$age_cat_lf), "\n")
print(table(df_dm_lf2$age_cat_lf, useNA="ifany"))

cat("country levels:", nlevels(df_dm_lf2$country), "\n")
print(table(df_dm_lf2$country, useNA="ifany"))

cat("HH_08_grp levels:", nlevels(df_dm_lf2$HH_08_grp), "\n")
print(table(df_dm_lf2$HH_08_grp, useNA="ifany"))

cat("out_of_lf unique:", paste(sort(unique(df_dm_lf2$out_of_lf)), collapse=", "), "\n")
cat("HH_27_bin unique:", paste(sort(unique(df_dm_lf2$HH_27_bin)), collapse=", "), "\n")
cat("HH_Educ07_RA_bin unique:", paste(sort(unique(df_dm_lf2$HH_Educ07_RA_bin)), collapse=", "), "\n")

# 3) Build survey design
des_dm_lf2 <- df_dm_lf2 %>%
  filter(
    !is.na(out_of_lf), !is.na(dm_prop), !is.na(HH_27_bin), !is.na(HH_Educ07_RA_bin),
    !is.na(age_cat_lf), !is.na(HH_08_grp)
  ) %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# 4) Add terms one by one to find failing variable
m0 <- svyglm(out_of_lf ~ dm_prop, design = des_dm_lf2, family = quasibinomial())
m1 <- svyglm(out_of_lf ~ dm_prop + HH_27_bin, design = des_dm_lf2, family = quasibinomial())
m2 <- svyglm(out_of_lf ~ dm_prop + HH_27_bin + HH_Educ07_RA_bin, design = des_dm_lf2, family = quasibinomial())
m3 <- svyglm(out_of_lf ~ dm_prop + HH_27_bin + HH_Educ07_RA_bin + age_cat_lf, design = des_dm_lf2, family = quasibinomial())
m4 <- svyglm(out_of_lf ~ dm_prop + HH_27_bin + HH_Educ07_RA_bin + age_cat_lf , design = des_dm_lf2, family = quasibinomial())
m5 <- svyglm(out_of_lf ~ dm_prop + HH_27_bin + HH_Educ07_RA_bin + age_cat_lf + country, design = des_dm_lf2, family = quasibinomial())



library(dplyr)
library(broom)
library(survey)
library(srvyr)

dm_vars <- paste0("MD01_", 1:9)

# Recode each decision item: empowered=1 (1/2/3), not empowered=0 (4/5/6/7), 97=NA
df_bydec <- df_dm_lf2 %>%
  mutate(across(all_of(dm_vars),
                ~ case_when(
                  .x %in% c(1,2,3) ~ 1,
                  .x %in% c(4,5,6,7) ~ 0,
                  .x == 97 ~ NA_real_,
                  TRUE ~ NA_real_
                ),
                .names = "{.col}_emp"
  ))

dm_emp <- paste0(dm_vars, "_emp")

# Keep rows with at least, say, 6 answered decision items (optional but recommended)
df_bydec <- df_bydec %>%
  rowwise() %>%
  mutate(dm_nonmiss = sum(!is.na(c_across(all_of(dm_emp))))) %>%
  ungroup() %>%
  filter(dm_nonmiss >= 6)

# Survey design
des_bydec <- df_bydec %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

# Item-level model (keep controls that worked)
form_bydec <- as.formula(
  paste(
    "out_of_lf ~",
    paste(dm_emp, collapse = " + "),
    "+ HH_27_bin + HH_Educ07_RA_bin + age_cat_lf + country"
  )
)

m_bydec <- svyglm(form_bydec, design = des_bydec, family = quasibinomial())

or_bydec <- tidy(m_bydec, conf.int = TRUE, exponentiate = TRUE)
or_bydec %>% filter(grepl("^MD01_", term))



#In adjusted survey-weighted analyses among refugee women, most household decision-making domains were not significantly associated with labour-force status. The exception was autonomy over who the respondent spends time with (MD01_7): women who reported deciding alone/usually alone/or jointly with spouse had significantly lower odds of being out of the labour force (aOR=0.52, 95% CI: 0.32–0.83, p=0.006). This suggests that mobility/social autonomy may be an important enabling factor for women’s labour-force participation.