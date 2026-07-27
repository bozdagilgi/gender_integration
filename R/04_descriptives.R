###Descriptives
##Questions from Laura


##•	Employment status by sex and age group (and host/displaced status and location)
##Employed / unemployed / looking / outside labour force.


combined_RA_adult_ind_svy <- combined_RA_adult_ind %>%
  as_survey_design(
    ids = NULL,
    weights = wgh_samp_pop_restr_resp,
    nest = TRUE
  )

combined_RA_adult_ind_svy <- combined_RA_adult_ind_svy %>%
mutate(
  labour_force_status = as_factor(labour_force_status),
  Intro_07 = as_factor(Intro_07),
  HH_02_RA = as_factor(HH_02_RA),
  wgh_samp_pop_restr_resp = as.numeric(wgh_samp_pop_restr_resp)
) %>%
  
  
# Create table
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
  mutate(
    weighted_pct = 100 * denominator_w / sum(denominator_w)
  ) %>%
  ungroup()

labour_force_status_tab


#1 Employed
#2 Unemployed 
#3 Outside of labour force - available not looking for jobs
#4 Outside of labour force - unavailable 


library(ggplot2)
library(dplyr)
library(scales)

ggplot(
  labour_force_status_tab %>% filter(!is.na(labour_force_status)),
  aes(x = HH_02_RA, y = weighted_pct, fill = labour_force_status)
) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(
    aes(label = paste0(round(weighted_pct, 1), "%")),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Labour force status by sex among refugees (weighted)",
    x = "Sex",
    y = "Percent",
    fill = "Labour force status"
  ) +
  scale_fill_unhcr_d() +
  theme_unhcr()


### Reasons for not being available


# Refugees + unavailable only
df_ref <- combined_RA_adult_ind %>%
  mutate(
    Intro_07 = as_factor(Intro_07),
    HH_02_RA = as_factor(HH_02_RA),
    labour_force_status = as_factor(labour_force_status),
    w = as.numeric(wgh_samp_pop_restr_resp),
    no_job_want_bin = if_else(no_job_want == 1, 1, 0, missing = 0),
    no_job_search_bin = if_else(no_job_search == 1, 1, 0, missing = 0),
    unpaid_work_bin = if_else(unpaid_work == 1, 1, 0, missing = 0),
    unpaid_work_housecare_bin = if_else(unpaid_work_housecare == 1, 1, 0, missing = 0)
  ) %>%
  filter(
    Intro_07 == "Refugees",
    labour_force_status == "Outside of labour force - unavailable",
    !is.na(HH_02_RA),
    !is.na(w), w > 0
  )

# Weighted percentages by gender
tab_ref_gender <- df_ref %>%
  group_by(HH_02_RA) %>%
  summarise(
    no_job_want_pct = 100 * weighted.mean(no_job_want_bin, w, na.rm = TRUE),
    no_job_search_pct = 100 * weighted.mean(no_job_search_bin, w, na.rm = TRUE),
    unpaid_work_pct = 100 * weighted.mean(unpaid_work_bin, w, na.rm = TRUE),
    unpaid_work_housecare_pct = 100 * weighted.mean(unpaid_work_housecare_bin, w, na.rm = TRUE),
    .groups = "drop"
  )

tab_ref_gender