# scripts/05b_assign_sub_status.R

source("R/00_packages.R")
source("R/01_config.R")
source("R/assign_sub_status.R")

stopifnot(exists("Indexed_Diagnoses_Combined"))
stopifnot(exists("Total_Dem"))


cols_to_remove <- c("SUB_Status", "SUB_Dx_Date", "SUB_Status.y", "SUB_Dx_Date.y", "SUB_Status.x", "SUB_Dx_Date.x")

existing_cols <- intersect(cols_to_remove, names(Indexed_Diagnoses_Combined))
if (length(existing_cols) > 0) {
  message("Removing existing columns from Indexed_Diagnoses_Combined: ",
      paste(existing_cols, collapse = ", "))
  
  Indexed_Diagnoses_Combined <- Indexed_Diagnoses_Combined %>%
    dplyr::select(-dplyr::all_of(existing_cols))
}
res_sub <- assign_sub_status(
  Indexed_Diagnoses_Combined = Indexed_Diagnoses_Combined,
  Total_Dem = Total_Dem,
  cfg = cfg,
  verbose = TRUE
)

SUB_Status <- res_sub$SUB_Status
Total_Dem <- res_sub$Total_Dem_with_SUB
Indexed_Diagnoses_Combined <- res_sub$Indexed_Diagnoses_Combined_with_SUB

dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(SUB_Status, file.path(cfg$output_dir, "SUB_Status.rds"))
saveRDS(Total_Dem, file.path(cfg$output_dir, "Total_Dem_with_SUB.rds"))
saveRDS(Indexed_Diagnoses_Combined, file.path(cfg$output_dir, "Indexed_Diagnoses_Combined_with_SUB.rds"))

message("Saved SUB outputs to: ", cfg$output_dir)