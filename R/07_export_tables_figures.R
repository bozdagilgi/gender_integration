# =========================================================
# 07_export_tables_figures.R
# Purpose: Produce final publication-ready figures using
#          ggplot2 + unhcrthemes. All tables are already
#          exported by scripts 04–06; this script adds the
#          visual outputs.
# Expected output:
#   output/figures/fig1_exclusion_rate_by_gender_country.png
#   output/figures/fig2_barrier_prevalence_by_gender.png
#   output/figures/fig3_predicted_prob_by_barriers.png
# =========================================================

dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Survey design object for weighted estimates
# ---------------------------
svy_analysis <- analysis_data %>%
  as_survey_design(strata = strata, weights = weight, nest = TRUE)

# ---------------------------
# Figure 1: Labour force exclusion rate by gender and country
# ---------------------------
excl_by_gender_country <- svy_analysis %>%
  filter(!is.na(female)) %>%
  group_by(country, female) %>%
  summarise(
    excl_rate     = survey_mean(excluded_lf, vartype = "ci", na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  mutate(
    gender_label = if_else(female == 1, "Female", "Male"),
    excl_pct     = excl_rate * 100,
    excl_low     = excl_rate_low * 100,
    excl_upp     = excl_rate_upp * 100
  )

fig1 <- ggplot(excl_by_gender_country,
               aes(x = country, y = excl_pct,
                   fill = gender_label, colour = gender_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = excl_low, ymax = excl_upp),
    position = position_dodge(width = 0.7),
    width = 0.25, linewidth = 0.5
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Labour Force Exclusion Rate by Gender and Country",
    subtitle = "Weighted estimates with 95% confidence intervals",
    x        = NULL,
    y        = "Exclusion rate (%)",
    fill     = NULL,
    colour   = NULL,
    caption  = "Source: UNHCR FDS – Cameroon 2024, Pakistan 2024, Zambia 2025."
  ) +
  unhcrthemes::scale_fill_unhcr_d() +
  unhcrthemes::scale_colour_unhcr_d() +
  unhcrthemes::theme_unhcr(grid = "Y")

ggsave(here("output", "figures", "fig1_exclusion_rate_by_gender_country.png"),
       fig1, width = 8, height = 5, dpi = 300)

# ---------------------------
# Figure 2: Barrier prevalence by gender (pooled)
# ---------------------------
barrier_prev <- svy_analysis %>%
  filter(!is.na(female)) %>%
  group_by(female) %>%
  summarise(
    Care          = survey_mean(care_barrier,     na.rm = TRUE),
    Documentation = survey_mean(docs_barrier,     na.rm = TRUE),
    Digital       = survey_mean(digital_barrier,  na.rm = TRUE),
    Mobility      = survey_mean(mobility_barrier, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  pivot_longer(
    cols      = c(Care, Documentation, Digital, Mobility),
    names_to  = "barrier",
    values_to = "pct"
  ) %>%
  mutate(
    gender_label = if_else(female == 1, "Female", "Male"),
    pct_plot     = pct * 100
  )

fig2 <- ggplot(barrier_prev,
               aes(x = barrier, y = pct_plot,
                   fill = gender_label, colour = gender_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title   = "Prevalence of Intersectional Barriers by Gender",
    subtitle = "Pooled sample – Cameroon, Pakistan, Zambia",
    x       = "Barrier type",
    y       = "Prevalence (%)",
    fill    = NULL,
    colour  = NULL,
    caption = "Source: UNHCR FDS – Cameroon 2024, Pakistan 2024, Zambia 2025."
  ) +
  unhcrthemes::scale_fill_unhcr_d() +
  unhcrthemes::scale_colour_unhcr_d() +
  unhcrthemes::theme_unhcr(grid = "Y")

ggsave(here("output", "figures", "fig2_barrier_prevalence_by_gender.png"),
       fig2, width = 8, height = 5, dpi = 300)

# ---------------------------
# Figure 3: Predicted probability of exclusion by barrier count and gender
# Uses the M6 model fitted in 05_models_main.R
# ---------------------------
if (exists("m6")) {
  pred_data <- expand.grid(
    female           = c(0, 1),
    barrier_count    = 0:4,
    care_barrier     = 0,
    mobility_barrier = 0,
    docs_barrier     = 0,
    digital_barrier  = 0,
    age              = median(analysis_data$age,  na.rm = TRUE),
    age2             = median(analysis_data$age2, na.rm = TRUE),
    education        = levels(analysis_data$education)[1],
    disability       = levels(analysis_data$disability)[1],
    pop_group        = levels(analysis_data$pop_group)[1],
    country          = levels(analysis_data$country)[1]
  ) %>%
    as_tibble() %>%
    mutate(
      education  = as.factor(education),
      disability = as.factor(disability),
      pop_group  = as.factor(pop_group),
      country    = as.factor(country)
    )

  pred_data$predicted <- predict(m6, newdata = pred_data, type = "response")

  pred_data <- pred_data %>%
    mutate(gender_label = if_else(female == 1, "Female", "Male"))

  fig3 <- ggplot(pred_data,
                 aes(x = barrier_count, y = predicted,
                     colour = gender_label, group = gender_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = 0:4) +
    labs(
      title    = "Predicted Probability of Labour Force Exclusion by Barriers and Gender",
      subtitle = "Based on Model M6 – at mean age, modal education, pooled sample",
      x        = "Number of barriers (0–4)",
      y        = "Predicted probability of exclusion",
      colour   = NULL,
      caption  = "Source: UNHCR FDS – Cameroon 2024, Pakistan 2024, Zambia 2025."
    ) +
    unhcrthemes::scale_colour_unhcr_d() +
    unhcrthemes::theme_unhcr(grid = "XY")

  ggsave(here("output", "figures", "fig3_predicted_prob_by_barriers.png"),
         fig3, width = 8, height = 5, dpi = 300)
} else {
  message("Model m6 not found in session. Source 05_models_main.R first.")
}

message("07_export_tables_figures.R complete.")
message("Figures written to output/figures/")
