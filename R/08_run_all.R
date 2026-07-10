# =========================================================
# 08_run_all.R
# Purpose: Master runner – sources all scripts in order.
#          Run this file from the project root to reproduce
#          all outputs end-to-end.
# =========================================================

# Ensure here() resolves from project root
library(here)

message("=== Gender Integration Workflow ===")
message("Starting: ", Sys.time())

source(here("R", "00_packages.R"))              # 1. Load packages
source(here("R", "01_load_data.R"))             # 2. Load raw data + survey objects
source(here("R", "02_data_dictionary_check.R")) # 3. Audit variables
source(here("R", "03_build_analysis_data.R"))   # 4. Build harmonised analysis data
source(here("R", "04_descriptive_tables.R"))    # 5. Weighted descriptive tables
source(here("R", "05_models_main.R"))           # 6. Main regression models
source(here("R", "06_models_robustness.R"))     # 7. Robustness checks
source(here("R", "07_export_tables_figures.R")) # 8. Final outputs

message("=== All scripts complete: ", Sys.time(), " ===")
message("Outputs saved in output/data/, output/tables/, output/figures/")
