# R/config.R
# Central place for paths + column selections.
# We'll keep adding to this as the pipeline grows.

# ---- Project metadata ----


cfg <- list(
  data_dir   = "D:/Partners HealthCare Dropbox/Ahmad Mashlah/Concussion and TBI Data/New TBI data 11_2025/Updated TBI database/",
  excel_file = "D:/Dropbox (Partners HealthCare)/Concussion and TBI Data/ICD Codes/New folder/ICD9and10codescombinedHTNDM.xlsx"
)

cfg$project_name <- "Tumor_Imaging"  
# examples:
# "SCI_Cardiovascular"
# "TBI_PTE"
# "SCI_Endocrine"

cfg$project_slug <- gsub("[^a-zA-Z0-9]+", "_", cfg$project_name)

# ---- Control group toggle ----
# Set to FALSE to disable all control-group processing throughout the pipeline.
# When FALSE: control data is not read, filtered, or combined into the cohort.
# The exposure group is still processed in full.
cfg$include_controls <- FALSE


# ---- Identity + core column names (adjust per project) ----
cfg$id_col        <- "EPIC_PMRN"      # THIS dataset should be EMPI
cfg$dx_date_col   <- "Date"      # diagnosis date column in final_dataframe
cfg$dx_name_col   <- "Simplified_diagnosis"

# ---- Index-date diagnoses (rename as needed per project) ----
cfg$index_dx_set_name <- "TBI"  # e.g., "SCI" in other projects
cfg$index_dx_values <- c("Severe", "Moderate", "Mild", "Penetrating", "Unclassified", "PersonalHistory")

Dem_select_columns <- c(
  cfg$id_col, "Gender_Legal_Sex", "Date_of_Birth", "Age",
  "Race1", "Ethnic_Group", "Is_a_veteran", "Vital_status"
)

Dia_select_columns <- c(cfg$id_col, "Date", "Code_Type", "Code")  



# ---- Encounter files settings ----
cfg$enc_data_dir     <- cfg$data_dir   # can differ from cfg$data_dir
cfg$enc_date_col     <- "Admit_Date"
cfg$enc_select_cols  <- c(cfg$id_col, cfg$enc_date_col)

# ---- Output cache ----
cfg$output_dir <- "TBI_Tumor" ## was cfg$output_dir <- "outputs" and "TBI_Only_MS_MRNs" before for prior projects FYI

# ---- Primary exposure: Disease State labels ----
cfg$disease_col <- "TBI_Status"

cfg$tbi_dx_values <- c("Penetrating", "Severe", "Moderate", "Mild", "Unclassified", "PersonalHistory")

# for severity assignment, which code types to use (e.g., ICD9 only)
cfg$severity_code_types <- c("ICD9", "ICD10")   # or c("ICD9","ICD10") depending on project

# severity ranking (first = highest severity)
cfg$severity_rank <- c("Severe", "Moderate", "Mild", "Penetrating", "Unclassified", "PersonalHistory")

# ---- # of encounters before and after -----
cfg$min_enc_before_index <- 2
cfg$min_enc_after_index  <- 2

# ---- Seed for randomness -----
cfg$control_index_seed <- 123456
cfg$control_index_from_first_half <- TRUE

# ---- Baseline exclusion settings ----
cfg$baseline_exclusion_dx <- c("Tumor")  # e.g., c("Tumor") or names(ICDCodesCombined) (This appears to be obselete btw)
# ---- Step 06: outcome + baseline exclusion ----
cfg$outcome_dx_values <- c("Tumor")  # diagnosis of interest (baseline exclusion diagnosis)
#c("HTN", "HLD", "PCHD", "ACHD", "Obesity", "AfibFlutter") for CVS, "Dementia" For dementia,and ("MultipleSclerosis",	"NeuromyelitisOptica",	"MOGAD",	"TransverseMyelitis",	"OtherCNSDemyelination",	"PeripheralNSDemyelination") for demyelinating diseases
#c("MultipleSclerosis",	"NeuromyelitisOptica",	"MOGAD",	"TransverseMyelitis",	"OtherCNSDemyelination",	"PeripheralNSDemyelination") 
cfg$baseline_exclusion_days <- 365       # "within 1 year or before" = Date_diff < 365

#### ---- Sideline analysis ---- ####
cfg$sub_dx_values <- c("Seizure_disorder")  # adjust to your Simplified_diagnosis labels
cfg$sub_window_days <- 15  # ~1 months (use 180 if you prefer exact)
cfg$sub_require_after_index <- TRUE  # TRUE = only after index (>=0)


# ---- Step 06: which status column to use downstream (TBI vs SUB) ----
cfg$status_col <- "TBI_Status"          # set to "SUB_Status" if you want to use SUB instead

# ---- Step 06: Table 1 + cleaning definitions ----
cfg$adult_min_age <- 15

cfg$race_of_interest <- c(
  "Asian", "Black", "White",
  "American Indian or Alaska Native",
  "Hispanic", "Unknown"
)

# statuses you want to keep as-is (everything else gets lumped)
cfg$status_keep_levels <- c("None", "Mild", "Moderate", "Severe")
cfg$status_lump_to <- "Unclassified"
cfg$status_lump_values <- c("PersonalHistory", "Penetrating", "Unclassified")

# collapse moderate/severe
cfg$collapse_modsev <- TRUE
cfg$modsev_values <- c("Moderate", "Severe")
cfg$modsev_label <- "Moderate/Severe"

# optionally drop a status (e.g., Unclassified)
cfg$drop_status_values <- c("Unclassified")   # set character(0) if you want to keep them

# gender cleaning
cfg$gender_unknown_values <- c("Unknown-U")
cfg$gender_unknown_label <- "Unknown"

# composite vars
cfg$other_abuse_inputs <- c("Cocaine", "Sedative", "Cannabis", "Hallucinogens", "OtherAbuse")
cfg$other_abuse_name   <- "Other_Abuse"

cfg$ihd_inputs <- c("PCHD", "ACHD")
cfg$ihd_name   <- "Ischemic_Heart_Disease"

# file outputs

#  -------------- Imaging

cfg$imaging_data_dir       <- cfg$data_dir   # point elsewhere if Rdt files are in a different folder
cfg$imaging_acute_days     <- 30
cfg$imaging_acute_neg_days <- -2
cfg$imaging_followup_days  <- 30


#--------- Matching ----------#


cfg$do_matching <- FALSE          # set TRUE when you want matching
cfg$matching_mode  <- "binary"  # Options: "binary", "three_way_twostep"
cfg$match_seed <- 20250101        # fixed seed = reproducible
cfg$match_ratio <- 1

# Which variable defines “case vs control” for matching?
# If you match TBI vs Control, keep as "TBI_Status".
# If you match SUB vs No-SUB (within exposure only), set cfg$match_case_col <- "SUB_Status".
cfg$match_case_col <- "TBI_Status"    # OR "SUB_Status"

# Value that indicates "control" within cfg$match_case_col
cfg$match_control_value <- "None"     # for TBI_Status
cfg$match_case_value <- NULL          # if NULL -> anything not control is "case"

# Covariates to match on (must exist in table1-ready dataset)
cfg$match_formula_vars <- c("age_at_Index", "Race1", "Gender_Legal_Sex", "Follow_up_duration")

cfg$match_covariates <- c("age_at_Index", "Gender_Legal_Sex", "Race1", "Follow_up_duration")

# 3-way labels
cfg$match_three_anchor <- "Moderate/Severe"
cfg$match_three_mid    <- "Mild"
cfg$match_three_ref    <- "None"

# Match settings
cfg$match_method   <- "nearest"
cfg$match_distance <- "glm"
cfg$match_replace  <- FALSE
cfg$match_m_order  <- "data"

# ---- Tables output ----
cfg$tables_dir <- file.path(cfg$output_dir, "tables")
dir.create(cfg$tables_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Which column defines groups in tables ----
# Use your unified status choice (TBI_Status or SUB_Status)
# You can override to "Group" if you create one explicitly later.
cfg$table_group_col <- cfg$status_col

# ---- Table 1 variables (baseline characteristics) ----
# Update as your dataset evolves
cfg$table1_vars <- c(
  "age_at_Index",
  "Gender_Legal_Sex",
  "Race1",
  "Ethnic_Group",
  "Is_a_veteran",
  "Follow_up_duration",
  "#_of_Encounters_before_Index",
  "#_of_Encounters_after_Index"
)

cfg$table2_mode <- c("unfiltered", "filtered")   # run both, in this order
cfg$table2_unfiltered_exclude <- c(
  cfg$id_col,
  cfg$table_group_col,
  "Treatment",                         # if present
  "WithinBaselineWindow",
  "ToBeExcluded",
  "Index_Date", "Date", "Date_diff",
  "Follow_up_duration",
  "#_of_Encounters_before_Index",
  "#_of_Encounters_after_Index"
)


# ---- Table 2 categories (incident diagnoses columns) ----
# IMPORTANT: these must match the wide indicator column names in your dataset
cfg$table2_categories <- list(
  Cardiovascular = c("Hypertension", "CAD", "Hyperlipidemia", "Obesity"),
  Endocrine      = c("Hypothyroidism", "Pituitary_dysfunction", "DM", "Adrenal_insufficiency", "Erectile_dysfunction"),
  Psychiatric    = c("Depression", "Bipolar_disorder", "Anxiety_disorder", "Sleep_disorder",
                     "Suicide_ideation/intent/attempt", "Substance_misuse", "Opioid_misuse", "Alcohol_misuse"),
  Neurologic     = c("Ischemic_stroke/TIA", "Seizure_disorder", "Dementia"),
  Demyelinating = c("MultipleSclerosis",	"NeuromyelitisOptica",	"MOGAD",	"TransverseMyelitis",	"OtherCNSDemyelination",	"PeripheralNSDemyelination")
  
  ##
)


# ------ COX settings ------
cfg$group_col <- cfg$status_col #vs Treatment, vs SUB_Status vs "TBI_Status"  etc.
cfg$cox_covariates <- c("age_at_Index","Gender_Legal_Sex","Race1")
cfg$group_ref_level <- "None" ### Setting reference level, for TBI projects this is ususally "None"
cfg$followup_col <- "Follow_up_duration"

# ---- KM plot settings ----
cfg$km_conf_int <- TRUE          # show CI (TRUE/FALSE)

# X axis (years)
cfg$km_xlim_mode <- "fixed"      # "fixed" or "data"
cfg$km_xlim_years <- c(0, 20)    # used when mode="fixed"

# Y axis (cumulative event prob)
cfg$km_ylim_mode <- "auto"     # "fixed" | "auto" | "default"
cfg$km_ylim      <- c(0, 0.05) # only used when mode="fixed"

# Risk table
cfg$km_show_risktable <- TRUE
cfg$km_risktable_times_years <- seq(0, 25, by = 5)   # columns in risk table

cfg$km_show_pval       <- TRUE










##### Project finalized name for file outputs #####
build_project_slug <- function(cfg,
                               base_name = cfg$project_name) {
  
  # ICD type (e.g., ICD9, ICD10, ICD9ICD10)
  icd_part <- paste(sort(unique(cfg$severity_code_types)), collapse = "")
  
  # Encounter window (before_after)
  enc_part <- paste0(
    "enc",
    cfg$min_enc_before_index,
    "_",
    cfg$min_enc_after_index
  )
  
  # Baseline exclusion window (days)
  bl_part <- if (!is.null(cfg$baseline_exclusion_days)) {
    paste0("bl", cfg$baseline_exclusion_days)
  } else {
    NULL
  }
  
  # Matching
  match_part <- if (isTRUE(cfg$do_matching)) { paste0("Matched_",cfg$matching_mode) } else "Unmatched"
  
  # KM x-axis mode (only if fixed)
  km_part <- if (!is.null(cfg$km_xlim_mode) &&
                 identical(cfg$km_xlim_mode, "fixed")) {
    "KMfixed"
  } else {
    NULL
  }
  
  parts <- c(
    base_name,
    icd_part,
    enc_part,
    bl_part,
    match_part,
    km_part
  )
  
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = "_")
}

cfg$project_slug <- build_project_slug(cfg)
cfg$bundle_name  <- paste0(cfg$project_slug, "_Survival")