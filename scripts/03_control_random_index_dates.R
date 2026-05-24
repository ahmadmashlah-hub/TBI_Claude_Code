source("R/00_packages.R")
source("R/01_config.R")
source("R/read_control_encounters_random_index.R")

dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)

if (!isTRUE(cfg$include_controls)) {
  message("cfg$include_controls = FALSE → skipping control random index date assignment.")
  final_Control_Enc <- NULL
} else {

  control_enc_rds <- file.path(cfg$output_dir, "final_Control_Enc.rds")

  if (file.exists(control_enc_rds)) {
    message("Found cached Control Enc RDS → loading...")
    final_Control_Enc <- readRDS(control_enc_rds)
  } else {
    message("No cached Control Enc RDS → building...")
    final_Control_Enc <- build_control_encounters_with_random_index(cfg, verbose = TRUE)
    saveRDS(final_Control_Enc, control_enc_rds)
    message("Saved Control Enc RDS to: ", control_enc_rds)
  }

  message("Unique ", cfg$id_col, " in final_Control_Enc: ", dplyr::n_distinct(final_Control_Enc[[cfg$id_col]]))

}