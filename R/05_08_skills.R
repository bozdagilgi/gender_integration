# =========================================================
# FEMALE REFUGEE (PAKISTAN): SKILLS & LABOUR FORCE STATUS
# Outcome: out_of_lf (1=out of labour force, 0=in labour force)
# Runs:
#  1) Data checks
#  2) One-by-one adjusted model for each skill
#  3) Composite skills model (numeracy, digital, licenses)
#  4) Nicely labeled output tables
# =========================================================


# -----------------------------
# 0) Variables
# -----------------------------
skill_vars <- c(
  "Skills06b","Skills06c","Skills06d",            # numeracy
  "Skills38a","Skills38b","Skills38c","Skills39", # device/basic digital
  "Skills41a","Skills41b","Skills41c","Skills41d",# digital tasks
  "Skills42a","Skills42b"                         # licenses
)

skill_labels <- c(
  Skills06b_bin = "Calculate prices",
  Skills06c_bin = "Multiplication/division",
  Skills06d_bin = "Fractions/decimals/percentages",
  Skills38a_bin = "Use mobile phone",
  Skills38b_bin = "Use smartphone",
  Skills38c_bin = "Use tablet",
  Skills39_bin  = "Used computer in past 3 months",
  Skills41a_bin = "Send/receive emails",
  Skills41b_bin = "Internet/video calls",
  Skills41c_bin = "Find information on internet",
  Skills41d_bin = "Online learning/studying",
  Skills42a_bin = "Car driving license",
  Skills42b_bin = "Truck driving license"
)

# -----------------------------
# 1) Raw checks (optional but recommended)
# -----------------------------
cat("\n================ RAW FREQUENCIES ================\n")
for (v in skill_vars) {
  cat("\n---", v, "---\n")
  print(table(combined_RA_adult_ind[[v]], useNA = "ifany"))
}
cat("\n--- labour_force ---\n")
print(table(combined_RA_adult_ind$labour_force, useNA="ifany"))

# -----------------------------
# 2) Build analysis dataset
# -----------------------------
df_sk <- combined_RA_adult_ind %>%
  filter(
    Intro_07 == "Refugees",
    HH_02_RA %in% c(2, "Female"),
    country %in% c("Pakistan", 1, "PAK"),   # adjust if needed
    labour_force %in% c(0,1),
    !is.na(wgh_samp_pop_restr_resp), wgh_samp_pop_restr_resp > 0,
    !is.na(age_cat_lf), !is.na(HH_Educ07_RA), !is.na(HH_27)
  ) %>%
  mutate(
    out_of_lf = if_else(labour_force == 0, 1, 0),
    age_cat_lf = droplevels(as_factor(age_cat_lf)),
    HH_Educ07_RA_bin = if_else(HH_Educ07_RA == 1, 1, 0),
    HH_27_bin = case_when(HH_27 == 1 ~ 1, HH_27 == 2 ~ 0, TRUE ~ NA_real_)
  )

# Recode each skill to binary: 1=yes, 2=no, else NA
for (v in skill_vars) {
  df_sk[[paste0(v, "_bin")]] <- case_when(
    df_sk[[v]] == 1 ~ 1,
    df_sk[[v]] == 2 ~ 0,
    TRUE ~ NA_real_
  )
}
skill_bin <- paste0(skill_vars, "_bin")

cat("\n================ ANALYTIC SAMPLE =================\n")
cat("N (before skill-specific filtering):", nrow(df_sk), "\n")
print(table(df_sk$out_of_lf, useNA="ifany"))

# -----------------------------
# 3) One-by-one adjusted models (each skill)
# -----------------------------
run_skill <- function(v){
  d <- df_sk %>%
    filter(!is.na(.data[[v]]), !is.na(HH_27_bin), !is.na(out_of_lf))
  
  # skip if no variation in outcome or predictor
  if (length(unique(d$out_of_lf)) < 2 || length(unique(d[[v]])) < 2) {
    return(tibble(
      skill = v, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_,
      p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_, n = nrow(d)
    ))
  }
  
  des <- d %>%
    as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
    as_survey()
  
  f <- as.formula(paste(
    "out_of_lf ~", v, "+ age_cat_lf + HH_Educ07_RA_bin + HH_27_bin"
  ))
  
  m <- svyglm(f, design = des, family = quasibinomial())
  
  tidy(m, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == v) %>%
    transmute(
      skill = v,
      estimate, std.error, statistic, p.value, conf.low, conf.high,
      n = nrow(d)
    )
}

res_skill_each <- bind_rows(lapply(skill_bin, run_skill)) %>%
  mutate(skill_label = recode(skill, !!!skill_labels, .default = skill)) %>%
  select(skill, skill_label, estimate, std.error, statistic, p.value, conf.low, conf.high, n) %>%
  arrange(p.value)

cat("\n================ ONE-BY-ONE SKILL MODELS =================\n")
print(res_skill_each, n = Inf)

# -----------------------------
# 4) Composite indices
# -----------------------------
df_comp <- df_sk %>%
  rowwise() %>%
  mutate(
    # Numeracy composite
    numeracy_n = sum(!is.na(c_across(all_of(c("Skills06b_bin","Skills06c_bin","Skills06d_bin"))))),
    numeracy_score = sum(c_across(all_of(c("Skills06b_bin","Skills06c_bin","Skills06d_bin"))), na.rm = TRUE),
    numeracy_prop = if_else(numeracy_n > 0, numeracy_score / numeracy_n, NA_real_),
    
    # Digital composite
    digital_n = sum(!is.na(c_across(all_of(c(
      "Skills38a_bin","Skills38b_bin","Skills38c_bin","Skills39_bin",
      "Skills41a_bin","Skills41b_bin","Skills41c_bin","Skills41d_bin"
    ))))),
    digital_score = sum(c_across(all_of(c(
      "Skills38a_bin","Skills38b_bin","Skills38c_bin","Skills39_bin",
      "Skills41a_bin","Skills41b_bin","Skills41c_bin","Skills41d_bin"
    ))), na.rm = TRUE),
    digital_prop = if_else(digital_n > 0, digital_score / digital_n, NA_real_),
    
    # Any driving license
    license_n = sum(!is.na(c_across(all_of(c("Skills42a_bin","Skills42b_bin"))))),
    license_score = sum(c_across(all_of(c("Skills42a_bin","Skills42b_bin"))), na.rm = TRUE),
    license_any = if_else(license_n > 0 & license_score > 0, 1,
                          if_else(license_n > 0, 0, NA_real_))
  ) %>%
  ungroup() %>%
  filter(!is.na(numeracy_prop), !is.na(digital_prop), !is.na(license_any), !is.na(HH_27_bin))

des_comp <- df_comp %>%
  as_survey_design(ids = NULL, weights = wgh_samp_pop_restr_resp, nest = TRUE) %>%
  as_survey()

m_comp <- svyglm(
  out_of_lf ~ numeracy_prop + digital_prop + license_any +
    age_cat_lf + HH_Educ07_RA_bin + HH_27_bin,
  design = des_comp,
  family = quasibinomial()
)

or_comp <- tidy(m_comp, conf.int = TRUE, exponentiate = TRUE)

cat("\n================ COMPOSITE MODEL =================\n")
print(or_comp, n = Inf)

cat("\n--- Composite skill terms only ---\n")
print(or_comp %>% filter(term %in% c("numeracy_prop","digital_prop","license_any")))

# -----------------------------
# 5) Export (optional)
# -----------------------------
# write.csv(res_skill_each, "skills_one_by_one_female_pak_out_of_lf.csv", row.names = FALSE)
# write.csv(or_comp, "skills_composite_female_pak_out_of_lf.csv", row.names = FALSE)

# -----------------------------
# 6) Quick significance flags
# -----------------------------
sig_each <- res_skill_each %>%
  mutate(sig = case_when(
    is.na(p.value) ~ "NA",
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

cat("\n================ ONE-BY-ONE (WITH SIG FLAGS) =================\n")
print(sig_each %>% select(skill_label, estimate, conf.low, conf.high, p.value, sig, n), n = Inf)