# scripts/01_read_exp_dem_dia.R

source("R/00_packages.R")
source("R/01_config.R")
source("R/02_read_exposure_inputs.R")

dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)

id_col <- cfg$id_col

# -----------------------------
# RDS cache paths
# -----------------------------
dem_exp_rds  <- file.path(cfg$output_dir, "final_Exp_Dem.rds")
dia_exp_rds  <- file.path(cfg$output_dir, "final_dataframe.rds")

dem_ctl_rds  <- file.path(cfg$output_dir, "final_Control_Dem.rds")
dia_ctl_rds  <- file.path(cfg$output_dir, "final_Control_dataframe.rds")

patient_summary <- list()

# -----------------------------
# ICD codes (shared)
# -----------------------------
ICDCodesCombined <- read_icd_excel(cfg$excel_file)

# -----------------------------
# DEMOGRAPHICS (Exposure)
# -----------------------------
if (file.exists(dem_exp_rds)) {
  message("Found cached Exp Dem RDS → loading...")
  final_Exp_Dem <- readRDS(dem_exp_rds)
} else {
  message("No cached Exp Dem RDS → rebuilding...")
  final_Exp_Dem <- read_demographics_exp_cfg(cfg, Dem_select_columns)
  saveRDS(final_Exp_Dem, dem_exp_rds)
  message("Saved Exp Dem RDS to: ", dem_exp_rds)
}

message("Unique ", id_col, " in Exp Dem: ", dplyr::n_distinct(final_Exp_Dem[[id_col]]))

# -----------------------------
# DIAGNOSES (Exposure)
# -----------------------------
if (file.exists(dia_exp_rds)) {
  message("Found cached Exp Dia RDS → loading...")
  final_dataframe <- readRDS(dia_exp_rds)
} else {
  message("No cached Exp Dia RDS → rebuilding...")
  final_dataframe <- read_diagnoses_exp_cfg(cfg, Dia_select_columns, ICDCodesCombined)
  saveRDS(final_dataframe, dia_exp_rds)
  message("Saved Exp Dia RDS to: ", dia_exp_rds)
}

message("Unique ", id_col, " in Exp Dia: ", dplyr::n_distinct(final_dataframe[[id_col]]))

# -----------------------------
# DEMOGRAPHICS (Control)
# -----------------------------
if (isTRUE(cfg$include_controls)) {
  if (file.exists(dem_ctl_rds)) {
    message("Found cached Control Dem RDS → loading...")
    final_Control_Dem <- readRDS(dem_ctl_rds)
  } else {
    message("No cached Control Dem RDS → rebuilding...")
    final_Control_Dem <- read_demographics_control_cfg(cfg, Dem_select_columns)
    saveRDS(final_Control_Dem, dem_ctl_rds)
    message("Saved Control Dem RDS to: ", dem_ctl_rds)
  }
  message("Unique ", id_col, " in Control Dem: ", dplyr::n_distinct(final_Control_Dem[[id_col]]))
} else {
  message("cfg$include_controls = FALSE → skipping Control Dem.")
  final_Control_Dem <- NULL
}

# -----------------------------
# DIAGNOSES (Control)
# -----------------------------
if (isTRUE(cfg$include_controls)) {
  if (file.exists(dia_ctl_rds)) {
    message("Found cached Control Dia RDS → loading...")
    final_Control_dataframe <- readRDS(dia_ctl_rds)
  } else {
    message("No cached Control Dia RDS → rebuilding...")
    final_Control_dataframe <- read_diagnoses_control_cfg(cfg, Dia_select_columns, ICDCodesCombined)
    saveRDS(final_Control_dataframe, dia_ctl_rds)
    message("Saved Control Dia RDS to: ", dia_ctl_rds)
  }
  message("Unique ", id_col, " in Control Dia: ", dplyr::n_distinct(final_Control_dataframe[[id_col]]))
} else {
  message("cfg$include_controls = FALSE → skipping Control Dia.")
  final_Control_dataframe <- NULL
}

# -----------------------------
# PATIENT SUMMARY (your original style)
# -----------------------------
patient_summary <- c(
  patient_summary,
  paste("Total number of Exp patients was", dplyr::n_distinct(final_Exp_Dem[[id_col]])),
  if (isTRUE(cfg$include_controls))
    paste("Total number of Control patients was", dplyr::n_distinct(final_Control_Dem[[id_col]]))
)

print(patient_summary)