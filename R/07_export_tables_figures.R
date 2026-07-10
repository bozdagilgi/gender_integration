# =========================================================
# 07_export_tables_figures.R
# Purpose: Produce polished publication-ready tables and
#          figures.  Run after 05_models_main.R.
# =========================================================

dir.create(here("output", "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# Load model objects if not already in environment
if (!exists("m2")) {
  load(here("output", "data", "main_models.RData"))
}

# ----------------------------------------------------------
# Figure 1: Odds ratios plot for M2 (barriers model, pooled)
# ----------------------------------------------------------
or_plot_data <- broom::tidy(m2, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high),
    term    = stringr::str_replace_all(term, "_", " ") %>% tools::toTitleCase()
  )

fig1 <- ggplot(or_plot_data, aes(x = OR, y = reorder(term, OR))) +
  geom_point(size = 3, colour = "#0072BC") +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), height = 0.2, colour = "#0072BC") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  labs(
    title   = "Odds Ratios: outside labour force (M2 – barriers, pooled)",
    x       = "Odds Ratio (95% CI)",
    y       = NULL,
    caption = "Survey-weighted quasibinomial; pooled CMR/PAK/ZAM."
  ) +
  theme_unhcr(font_size = 11)

ggsave(
  filename = here("output", "figures", "fig1_OR_barriers_pooled.png"),
  plot = fig1, width = 8, height = 5, dpi = 300
)

# ----------------------------------------------------------
# Figure 2: Outside-LF rate by gender and country
# ----------------------------------------------------------
if (!exists("tab_desc")) {
  tab_desc <- read_xlsx(here("output", "tables", "table1_weighted_descriptives.xlsx"))
}

fig2 <- tab_desc %>%
  ggplot(aes(x = female, y = outside_lf_rate, fill = female)) +
  geom_col(position = "dodge", width = 0.6) +
  facet_wrap(~country) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_unhcr_d() +
  labs(
    title = "Outside labour force rate by gender and country",
    x     = "Gender",
    y     = "% outside labour force",
    fill  = NULL,
    caption = "Weighted estimates; HH_02_RA gender variable."
  ) +
  theme_unhcr(font_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  filename = here("output", "figures", "fig2_outside_lf_by_gender_country.png"),
  plot = fig2, width = 9, height = 5, dpi = 300
)

# ----------------------------------------------------------
# Polished regression table (all main models) via gt
# ----------------------------------------------------------
reg_final <- read_xlsx(here("output", "tables", "table2_main_models_OR.xlsx"))

reg_final %>%
  gt(groupname_col = "model") %>%
  tab_header(
    title    = "Table 2: Survey-weighted logistic regression results",
    subtitle = "Outcome: outside labour force (1 = outside)"
  ) %>%
  fmt_number(columns = c(OR, OR_low, OR_high, p.value), decimals = 3) %>%
  tab_source_note("Quasibinomial family; survey weights applied.") %>%
  cols_label(
    term       = "Variable",
    estimate   = "Log-OR",
    std.error  = "SE",
    p.value    = "p",
    OR         = "OR",
    OR_low     = "95% CI low",
    OR_high    = "95% CI high"
  ) %>%
  gtsave(filename = here("output", "tables", "table2_main_models_OR_polished.html"))

message("07_export_tables_figures.R complete.")
message("Saved: output/figures/fig1_OR_barriers_pooled.png")
message("Saved: output/figures/fig2_outside_lf_by_gender_country.png")
message("Saved: output/tables/table2_main_models_OR_polished.html")
