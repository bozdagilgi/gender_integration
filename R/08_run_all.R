# =========================================================
# 08_run_all.R
# Purpose: Master script – sources all pipeline scripts in
#          sequence and stops with a clear error message if
#          any step fails.
#
# Usage:
#   source(here::here("R", "08_run_all.R"))
#   or: Rscript R/08_run_all.R
#
# Prerequisites:
#   - All raw data files in place under Cameroon/, Pakistan/,
#     Zambia/ directories (see 01_load_data.R for paths).
#   - Working directory set to the project root (RStudio
#     .Rproj handles this automatically).
# =========================================================

library(here)

scripts <- c(
  here("R", "00_packages.R"),
  here("R", "01_load_data.R"),
  here("R", "02_data_dictionary_check.R"),
  here("R", "03_build_analysis_data.R"),
  here("R", "04_descriptive_tables.R"),
  here("R", "05_models_main.R"),
  here("R", "06_models_robustness.R"),
  here("R", "07_export_tables_figures.R")
)

for (script in scripts) {
  message("\n", strrep("=", 60))
  message("Running: ", basename(script))
  message(strrep("=", 60))
  tryCatch(
    source(script, echo = FALSE),
    error = function(e) {
      stop(
        "\nPipeline stopped in: ", basename(script),
        "\nError: ", e$message,
        "\n\nPlease fix the issue in the script above and re-run 08_run_all.R.",
        call. = FALSE
      )
    }
  )
  message("Done: ", basename(script))
}

message("\n", strrep("=", 60))
message("All scripts completed successfully.")
message("Outputs are in:")
message("  output/checks/   – variable audit files")
message("  output/data/     – analysis-ready .rds")
message("  output/tables/   – HTML and Excel tables")
message("  output/figures/  – PNG figures")
message(strrep("=", 60))
