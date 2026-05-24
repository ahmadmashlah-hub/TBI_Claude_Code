source("R/00_packages.R")
source("R/01_config.R")
source("R/tables_1_2.R")

stopifnot(exists("Young_Clean_NoBS_After"))

if (isTRUE(cfg$do_matching)) {
  
  stopifnot(exists("Young_Clean_NoBS_After_matched"))
  analysis_df <- Young_Clean_NoBS_After_matched
  message("Analysis dataset: MATCHED sample (cfg$do_matching = TRUE)")
  
} else {
  
  analysis_df <- Young_Clean_NoBS_After
  message("Analysis dataset: UNMATCHED sample (cfg$do_matching = FALSE)")
  
}

# Ensure group col exists (cfg$table_group_col usually cfg$status_col)
if (!(cfg$table_group_col %in% names(analysis_df))) {
  stop("Group column not found in analysis_df: ", cfg$table_group_col)
}

# Table 1
tab1 <- make_table1(
  df = analysis_df,
  cfg = cfg,
  file_name = "Table1_baseline.html",
  verbose = TRUE
)

# Table 1.5 (Baseline dx)
stopifnot(exists("Indexed_Diagnoses_Combined"))
tab1_5 <- make_table1_5_baseline_dx(
  analysis_df = analysis_df,
  Indexed_Diagnoses_Combined = Indexed_Diagnoses_Combined,
  cfg = cfg,
  file_name = "Table1_5_baseline_dx.html",
  verbose = TRUE
)

# Table 2 (UNFILTERED first)
if ("unfiltered" %in% cfg$table2_mode) {
  tab2_all <- make_table2_unfiltered(
    df = analysis_df,
    cfg = cfg,
    file_name = "Table2_unfiltered_all_incident_dx.html",
    verbose = TRUE
  )
}

# Table 2 (FILTERED, your current category version)
if ("filtered" %in% cfg$table2_mode) {
  tab2_filtered <- make_table2(
    df = analysis_df,
    cfg = cfg,
    file_name = "Table2_filtered_categories.html",
    verbose = TRUE
  )
}

# patient flow CSV (if patient_summary exists)
if (exists("patient_summary")) {
  save_patient_flow(patient_summary, cfg)
} else {
  message("patient_summary not found; skipping flowchart CSV.")
}

message("✅ Tables created in: ", cfg$tables_dir)