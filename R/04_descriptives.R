###Descriptives
##Questions from Laura

## normalized weights



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

##	Employment status by sex and age group (and host/displaced status and location)
##Employed / unemployed / looking / outside labour force-----

combined_RA_adult_ind_clean <- combined_RA_adult_ind %>%
  mutate(
    labour_force_status = as_factor(labour_force_status),
    Intro_07 = as_factor(Intro_07),
    HH_02_RA = as_factor(HH_02_RA),
    wgh_samp_pop_restr_resp = as.numeric(wgh_samp_pop_restr_resp)
  )

# 2) THEN create survey design
combined_RA_adult_ind_svy <- combined_RA_adult_ind_clean %>%
  as_survey_design(
    ids = NULL,
    weights = weight_norm,
    nest = TRUE
  )

# 3) Table
labour_force_status_tab <- combined_RA_adult_ind_svy %>%
  filter(
    !is.na(labour_force_status),
    !is.na(Intro_07),
    !is.na(HH_02_RA),
    Intro_07 == "Refugees"
  ) %>%
  group_by(Intro_07, HH_02_RA, labour_force_status) %>%
  summarise(
    var_name = "labour_force_status",
    num_obs_uw = unweighted(n()),
    denominator_w = survey_total(vartype = NULL),
    .groups = "drop"
  ) %>%
  group_by(Intro_07, HH_02_RA) %>%
  mutate(weighted_pct = 100 * denominator_w / sum(denominator_w)) %>%
  ungroup()

ggplot(
  labour_force_status_tab %>%
    filter(!is.na(labour_force_status)) %>%
    mutate(labour_force_status = factor(
      labour_force_status,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Employed",
        "Unemployed",
        "Outside of labour force - available not looking for jobs",
        "Outside of labour force - unavailable"
      )
    )),
  aes(x = HH_02_RA, y = weighted_pct, fill = labour_force_status)
) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(
    aes(label = paste0(round(weighted_pct, 1), "%")),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Labour force status by sex among refugees (weighted)",
    x = "Sex",
    y = "Percent",
    fill = "Labour force status"
  ) +
  scale_fill_unhcr_d() +
  theme_unhcr()

### Reasons for not being available----

table(combined_RA_adult_ind$unpaid_work)
table(combined_RA_adult_ind$unpaid_work_housecare)
table(combined_RA_adult_ind$no_job_want)
table(combined_RA_adult_ind$no_job_search)
table(combined_RA_adult_ind$labour_force)


# Survey design 
combined_RA_adult_ind_svy <- combined_RA_adult_ind %>%
  as_survey_design(
    ids = NULL,
    weights = weight_norm,
    nest = TRUE
  )

# Table: Refugees only, by gender and population group
ref_gender_tab <- combined_RA_adult_ind_svy %>%
  filter(Intro_07 == "Refugees", !is.na(HH_02_RA)) %>%
  group_by(Intro_07,HH_02_RA) %>%
  summarise(
    unpaid_work_pct = survey_mean(unpaid_work == 1, na.rm = TRUE, vartype = NULL) * 100,
    unpaid_work_housecare_pct = survey_mean(unpaid_work_housecare == 1, na.rm = TRUE, vartype = NULL) * 100,
    no_job_want_pct = survey_mean(no_job_want == 1, na.rm = TRUE, vartype = NULL) * 100,
    no_job_search_pct = survey_mean(no_job_search == 1, na.rm = TRUE, vartype = NULL) * 100,
    .groups = "drop"
  )

ref_gender_tab

# Table: Refugees only, by gender and population group
ref_gender_tab_pop <- combined_RA_adult_ind_svy %>%
  filter(!is.na(HH_02_RA),labour_force==0) %>%
  group_by(Intro_07,country, HH_02_RA) %>%
  summarise(
    unpaid_work_pct = survey_mean(unpaid_work == 1, na.rm = TRUE, vartype = NULL) * 100,
    unpaid_work_housecare_pct = survey_mean(unpaid_work_housecare == 1, na.rm = TRUE, vartype = NULL) * 100,
    no_job_want_pct = survey_mean(no_job_want == 1, na.rm = TRUE, vartype = NULL) * 100,
    no_job_search_pct = survey_mean(no_job_search == 1, na.rm = TRUE, vartype = NULL) * 100,
    .groups = "drop"
  )

ref_gender_tab_pop

#Chart 

plot_df <- ref_gender_tab_pop %>%
  filter(Intro_07 == "Refugees") %>%   # remove if you want all population groups
  pivot_longer(
    cols = c(unpaid_work_pct, unpaid_work_housecare_pct, no_job_want_pct, no_job_search_pct),
    names_to = "indicator",
    values_to = "percent"
  ) %>%
  mutate(
    indicator = factor(
      indicator,
      levels = c("unpaid_work_pct", "unpaid_work_housecare_pct", "no_job_want_pct", "no_job_search_pct"),
      labels = c("Does unpaid work", "Does unpaid work (house care)", "Does not want a job", "Does not search a job")
    ),
    HH_02_RA = as.factor(HH_02_RA)
  )

Figure02_out_of_labour_force <-ggplot(plot_df, aes(x = country, y = percent, fill = HH_02_RA)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_dodge(width = 1),
    vjust = -0.25,
    size = 2.8
  ) +
  facet_wrap(~ indicator, scales = "free_y") +
  scale_y_continuous(
  limits = c(0, 100)) +
  labs(
    title = "Refugees: Out of labour force",
    x = "Country",
    y = "Weighted percent",
    fill = "Gender"
  ) +
  scale_fill_unhcr_d() +
  theme_unhcr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###Try with those who are in labour force-----

# Table: Refugees only, by gender and population group
ref_gender_tab_pop <- combined_RA_adult_ind_svy %>%
  filter(!is.na(HH_02_RA),labour_force==1) %>%
  group_by(Intro_07,country, HH_02_RA) %>%
  summarise(
    unpaid_work_pct = survey_mean(unpaid_work == 1, na.rm = TRUE, vartype = NULL) * 100,
    unpaid_work_housecare_pct = survey_mean(unpaid_work_housecare == 1, na.rm = TRUE, vartype = NULL) * 100,
    no_job_want_pct = survey_mean(no_job_want == 1, na.rm = TRUE, vartype = NULL) * 100,
    no_job_search_pct = survey_mean(no_job_search == 1, na.rm = TRUE, vartype = NULL) * 100,
    .groups = "drop"
  )

ref_gender_tab_pop

#Chart 

plot_df <- ref_gender_tab_pop %>%
  filter(Intro_07 == "Refugees") %>%   # remove if you want all population groups
  pivot_longer(
    cols = c(unpaid_work_pct, unpaid_work_housecare_pct, no_job_want_pct, no_job_search_pct),
    names_to = "indicator",
    values_to = "percent"
  ) %>%
  mutate(
    indicator = factor(
      indicator,
      levels = c("unpaid_work_pct", "unpaid_work_housecare_pct", "no_job_want_pct", "no_job_search_pct"),
      labels = c("Does unpaid work", "Does unpaid work (house care)", "Does not want a job", "Does not search a job")
    ),
    HH_02_RA = as.factor(HH_02_RA)
  )

Figure03_in_labour_force <-ggplot(plot_df, aes(x = country, y = percent, fill = HH_02_RA)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_dodge(width = 1),
    vjust = -0.25,
    size = 2.8
  ) +
  facet_wrap(~ indicator, scales = "free_y") +
  scale_y_continuous(
    limits = c(0, 100)) +
  labs(
    title = "Refugees: In labour force",
    x = "Country",
    y = "Weighted percent",
    fill = "Gender"
  ) +
  scale_fill_unhcr_d() +
  theme_unhcr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###Try with those who are in labour force for all population groups-----

# Table: Refugees only, by gender and population group
ref_gender_tab_pop_all <- combined_RA_adult_ind_svy %>%
  filter(!is.na(HH_02_RA),labour_force==1) %>%
  group_by(Intro_07,country, HH_02_RA) %>%
  summarise(
    unpaid_work_housecare_pct = survey_mean(unpaid_work_housecare == 1, na.rm = TRUE, vartype = NULL) * 100,
    .groups = "drop"
  )

ref_gender_tab_pop_all

#Chart 

plot_df <- ref_gender_tab_pop_all %>%
  pivot_longer(
    cols = c(unpaid_work_housecare_pct),
    names_to = "indicator",
    values_to = "percent"
  ) %>%
  mutate(
    indicator = factor(
      indicator,
      levels = c("unpaid_work_housecare_pct"),
      labels = c("Does unpaid work (house care)")
    ),
    HH_02_RA = as.factor(HH_02_RA)
  )

Figure04_in_labour_force_allpop <-ggplot(plot_df, aes(x = Intro_07, y = percent, fill = HH_02_RA)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_dodge(width = 1),
    vjust = -0.25,
    size = 2.8
  ) +
  facet_wrap(~ indicator + country, scales = "free_y") +
  scale_y_continuous(
    limits = c(0, 100)) +
  labs(
    title = "All population groups: In labour force",
    x = "Country",
    y = "Weighted percent",
    fill = "Gender"
  ) +
  scale_fill_unhcr_d() +
  theme_unhcr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


