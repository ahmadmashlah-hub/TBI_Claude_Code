# scripts/08_survival_outcomes.R

source("R/00_packages.R")
source("R/01_config.R")
source("R/08_survival_outcomes.R")

dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)

# Choose matched vs unmatched (config-driven)
stopifnot(exists("Young_Clean_NoBS_After"))

if (isTRUE(cfg$do_matching)) {
  stopifnot(exists("Young_Clean_NoBS_After_matched"))
  analysis_df <- Young_Clean_NoBS_After_matched
  message("Analysis dataset: MATCHED sample (cfg$do_matching = TRUE)")
} else {
  analysis_df <- Young_Clean_NoBS_After
  message("Analysis dataset: UNMATCHED sample (cfg$do_matching = FALSE)")
}

stopifnot(exists("Indexed_Diagnoses_Combined"))

results_table <- run_survival_bundle(
  analysis_df = analysis_df,
  Indexed_Diagnoses_Combined = Indexed_Diagnoses_Combined,
  cfg = cfg,
  verbose = TRUE
)

print(results_table)
message("✅ Survival analysis complete for: ", project_prefix(cfg))
