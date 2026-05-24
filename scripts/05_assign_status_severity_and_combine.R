source("R/00_packages.R")
source("R/01_config.R")
source("R/assign_disease_state_and_combine_control_exp.R")

# Inputs you already have from earlier steps:
# exp_dia                  = final_dataframe
# exp_dem_indexed          = merged_Index_Enc_Dem_age
# control_dia              = final_Control_dataframe
# control_dem_indexed      = merged_Control_Index_Enc_Dem_age

res_ds <- assign_tbi_status_and_build_cohort(
  cfg = cfg,
  exp_dia = final_dataframe,
  exp_dem_indexed = merged_Index_Enc_Dem_age,
  control_dia = final_Control_dataframe,
  control_dem_indexed = merged_Control_Index_Enc_Dem_age,
  verbose = TRUE
)

TBI_Status <- res_ds$TBI_Status
Indexed_Diagnoses_Combined <- res_ds$Indexed_Diagnoses_Combined
Total_Dem <- res_ds$Total_Dem

# -------------------------------------------------------
# Moderate/Severe cohort with >= 6 months follow-up
# -------------------------------------------------------
MS_Follow_Up_6_months <- Total_Dem %>%
  dplyr::filter(
    .data[[cfg$disease_col]] %in% c("Moderate", "Severe"),
    Follow_up_duration >= 180
  )

message(
  "MS_Follow_Up_6_months: ",
  nrow(MS_Follow_Up_6_months), " rows | ",
  dplyr::n_distinct(MS_Follow_Up_6_months[[cfg$id_col]]), " unique IDs"
)

ms_out_path <- file.path(
  cfg$output_dir,
  paste0(cfg$project_slug, "__MS_Follow_Up_6_months.csv")
)
dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(MS_Follow_Up_6_months, ms_out_path, row.names = FALSE)
message("Saved MS follow-up cohort to: ", ms_out_path)