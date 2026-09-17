##descriptives


combined_RA_adult_ind <- combined_RA_adult_ind %>%
  mutate(
    Intro_07 = case_when(
      Intro_07 %in% c("Host Community", "Host community") ~ "Host Community",
      TRUE ~ Intro_07
    )
  )

combined_RA_adult_ind <- combined_RA_adult_ind %>%
  group_by(country) %>%
  mutate(
    weight_norm = wgh_samp_pop_restr_resp /
      mean(wgh_samp_pop_restr_resp, na.rm = TRUE)
  ) %>%
  ungroup()

design_all <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = combined_RA_adult_ind
)

svymean(
  ~factor(labour_force),
  design_all,
  na.rm = TRUE
) * 100


round(
  prop.table(
    svytable(~Intro_07, design_all)
  ) * 100,
  1
)


round(
  prop.table(
    svytable(~HH_02_RA, design_all)
  ) * 100,
  1
)

round(
  prop.table(
    svytable(~country, design_all)
  ) * 100,
  1
)

# Unweighted sample sizes
table(combined_RA_adult_ind$country)

# Unweighted percentages
prop.table(
  table(combined_RA_adult_ind$country)
) * 100

round(
  prop.table(
    svytable(~marital_grp, design_all)
  ) * 100,
  1
)

round(
  prop.table(
    svytable(~age_cat_lf, design_all)
  ) * 100,
  1
)

round(
  prop.table(
    svytable(~disability_RA, design_all)
  ) * 100,
  1
)

round(
  prop.table(
    svytable(~child_u2, design_all)
  ) * 100,
  1
)

round(
  prop.table(
    svytable(~educ_level, design_all)
  ) * 100,
  1
)


##Create women data

women <- combined_RA_adult_ind %>%
  filter(HH_02_RA == "Female" & Intro_07!="Former Refugees")

design_women <- svydesign(
  ids = ~1,
  weights = ~weight_norm,
  data = women
)

svymean(
  ~labour_force,
  design_women,
  na.rm = TRUE
)


svyby(
  ~labour_force,
  ~Intro_07,
  design_women,
  svymean,
  na.rm = TRUE
)

svyby(
  ~labour_force,
  ~age_cat_lf,
  design_women,
  svymean,
  na.rm = TRUE
)

svyby(
  ~labour_force,
  ~marital_grp,
  design_women,
  svymean,
  na.rm = TRUE
)


svyby(
  ~labour_force,
  ~educ_level,
  design_women,
  svymean,
  na.rm = TRUE
)

svyby(
  ~labour_force,
  ~child_u2,
  design_women,
  svymean,
  na.rm = TRUE
)


svyby(
  ~labour_force,
  ~disability_RA,
  design_women,
  svymean,
  na.rm = TRUE
)

svyby(
  ~labour_force,
  ~country,
  design_women,
  svymean,
  na.rm = TRUE
)


##regression

m1 <- svyglm(
  labour_force ~ Intro_07,
  design = design_women,
  family = quasibinomial()
)

summary(m1)


table(women$Intro_07)


m2 <- svyglm(
  labour_force ~ Intro_07 + country,
  design = design_women,
  family = quasibinomial()
)

summary(m2)

m3 <- svyglm(
  labour_force ~
    Intro_07 +
    country +
    age_cat_lf +
    marital_grp,
  design = design_women,
  family = quasibinomial()
)

summary(m3)


##Check marital status in detail

svyby(
  ~labour_force,
  ~marital_grp + Intro_07,
  design_women,
  svymean,
  na.rm = TRUE
)


m_marital <- svyglm(
  labour_force ~
    Intro_07 * marital_grp +
    country +
    age_cat_lf,
  design = design_women,
  family = quasibinomial()
)

summary(m_marital)




prop.table(
  svytable(~marital_grp + Intro_07, design_women),
  margin = 2
) * 100



m4 <- svyglm(
  labour_force ~
    Intro_07 +
    country +
    age_cat_lf +
    marital_grp +
    child_u2 +
    educ_level,
  design = design_women,
  family = quasibinomial()
)

summary(m4)


svyby(
  ~labour_force,
  ~child_u2,
  design_women,
  svymean,
  na.rm = TRUE
)

svyby(
  ~labour_force,
  ~child_u2,
  subset(design_women, Intro_07 == "Refugees"),
  svymean,
  na.rm = TRUE
)


svychisq(
  ~labour_force + disability_RA,
  design_women
)

svyby(
  ~labour_force,
  ~disability_RA,
  design_women,
  svymean,
  na.rm = TRUE
)


m5 <- svyglm(
  labour_force ~
    Intro_07 +
    country +
    age_cat_lf +
    marital_grp +
    child_u2 +
    educ_level +
    disability_RA,
  design = design_women,
  family = quasibinomial()
)

summary(m5)
  

