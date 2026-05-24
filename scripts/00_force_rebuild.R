# scripts/00_force_rebuild.R
# Force a full rebuild of the pipeline by deleting cached outputs first.
# LOUD version: prints progress + errors.

options(warn = 1)  # show warnings immediately

cat("\n============================\n")
cat("FORCE REBUILD (LOUD)\n")
cat("============================\n")
cat("Working directory: ", getwd(), "\n\n")

# ---- helper to source with checks ----
source_checked <- function(path) {
  if (!file.exists(path)) {
    stop("Cannot find file to source: ", path,
         "\nWorking dir is: ", getwd(),
         "\nTip: setwd('path/to/repo') or use RStudio Project.")
  }
  message(">>> Sourcing: ", path)
  source(path, echo = TRUE, local = FALSE)  # echo prints executed lines
  message("<<< Done: ", path, "\n")
}

# ---- load packages + config ----
source_checked("R/00_packages.R")
source_checked("R/01_config.R")

# Ensure output dir exists
dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
message("Output dir: ", normalizePath(cfg$output_dir, winslash = "/", mustWork = FALSE))

# -----------------------------
# 1) Delete cached RDS files
# -----------------------------
cached_files <- c(
  #file.path(cfg$output_dir, "final_Exp_Dem.rds"),
  file.path(cfg$output_dir, "final_dataframe.rds"),
  #file.path(cfg$output_dir, "final_Control_Dem.rds"),
  file.path(cfg$output_dir, "final_Exp_Enc.rds"),
  if (isTRUE(cfg$include_controls)) file.path(cfg$output_dir, "final_Control_Enc.rds"),
  file.path(cfg$output_dir, "Index_Dates.rds"),
  if (isTRUE(cfg$include_controls)) file.path(cfg$output_dir, "Index_Dates_Control.rds"),
  file.path(cfg$output_dir, "Indexed_Diagnoses_Combined.rds"),
  file.path(cfg$output_dir, "Total_Dem.rds"),
  if (isTRUE(cfg$include_controls)) file.path(cfg$output_dir, "final_Control_dataframe.rds")
)

exists_vec <- file.exists(cached_files)
message("Cache candidates found: ", sum(exists_vec), " / ", length(cached_files))

deleted <- cached_files[exists_vec]
if (length(deleted) > 0) {
  ok <- file.remove(deleted)
  message("Deleted cached files:\n - ", paste(deleted, collapse = "\n - "))
  if (!all(ok)) warning("Some cache files could not be deleted.")
} else {
  message("No cached files found to delete in ", cfg$output_dir)
}

# -----------------------------
# 2) Run pipeline scripts (rebuild everything)
# -----------------------------
message("\n--- RUN PIPELINE ---\n")

source_checked("scripts/01_read_exp_dem_dia.R")
source_checked("scripts/02_index_dates_and_encounters.R")
source_checked("scripts/05_assign_status_severity_and_combine.R")

message("\n✅ Force rebuild complete.\n")