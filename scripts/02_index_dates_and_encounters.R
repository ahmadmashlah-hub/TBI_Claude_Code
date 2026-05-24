source("R/00_packages.R")
source("R/01_config.R")
source("R/read_index_dates.R")
source("R/read_encounters.R")

dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)

index_rds <- file.path(cfg$output_dir, "Index_Dates.rds")
enc_rds   <- file.path(cfg$output_dir, "final_Exp_Enc.rds")

# ---- Index_Dates (cached) ----
if (file.exists(index_rds)) {
  message("Found cached Index_Dates → loading...")
  Index_Dates <- readRDS(index_rds)
} else {
  message("No cached Index_Dates → building...")
  Index_Dates <- build_index_dates(
    final_dataframe   = final_dataframe,
    id_col            = cfg$id_col,
    dx_date_col       = cfg$dx_date_col,
    dx_name_col       = cfg$dx_name_col,
    index_dx_values   = cfg$index_dx_values
  )
  saveRDS(Index_Dates, index_rds)
  message("Saved Index_Dates to: ", index_rds)
}

# ---- Encounters (cached) ----
if (file.exists(enc_rds)) {
  message("Found cached Encounters summary → loading...")
  final_Exp_Enc <- readRDS(enc_rds)
} else {
  message("No cached Encounters summary → building...")
  final_Exp_Enc <- build_encounter_summary(
    enc_data_dir   = cfg$enc_data_dir,
    Index_Dates    = Index_Dates,
    id_col         = cfg$id_col,
    enc_date_col   = cfg$enc_date_col,
    enc_select_cols = cfg$enc_select_cols
  )
  saveRDS(final_Exp_Enc, enc_rds)
  message("Saved final_Exp_Enc to: ", enc_rds)
}

# Quick check
message("final_Exp_Enc rows: ", nrow(final_Exp_Enc))