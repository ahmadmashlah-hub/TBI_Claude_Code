# scripts/06b_cases_output.R
# Generates a detailed incident-cases file for the outcome diagnosis,
# combining demographics, TBI index date, earliest post-index diagnosis
# date, and imaging data from patient_imaging (built in 05c).
#
# Must run after:
#   05_assign_status_severity_and_combine.R  -> Total_Dem
#   05b_assign_pte_status.R                  -> SUB_Status in Total_Dem (optional)
#   05c_imaging_analysis.R                   -> patient_imaging
#   06_exclusion_and_table1_prep.R           -> res_excl (df_with_flags), excl_ids

source("R/00_packages.R")
source("R/01_config.R")

# -------------------------------------------------------
# 0. Validate required objects are in environment
# -------------------------------------------------------
stopifnot(
  exists("Total_Dem"),
  exists("res_excl"),        # list from get_baseline_exclusion_ids()
  exists("patient_imaging")
)

id_col   <- cfg$id_col
dx_list  <- cfg$outcome_dx_values
has_sub  <- "SUB_Status" %in% names(Total_Dem)

# -------------------------------------------------------
# 1. Pull earliest INCIDENT diagnosis date per patient
#    Incident = WithinBaselineWindow == 0 AND not in excl_ids
#    AND Simplified_diagnosis is one of the outcome dx values
# -------------------------------------------------------
df_flags <- res_excl$df_with_flags

incident_dx <- df_flags %>%
  dplyr::mutate(
    !!id_col   := as.character(.data[[id_col]]),
    Date        = as.Date(Date),
    Index_Date  = as.Date(Index_Date)
  ) %>%
  dplyr::filter(
    !(.data[[id_col]] %in% res_excl$Excl_IDs),   # exclude baseline cases
    WithinBaselineWindow == 0L,                    # post-baseline only
    Simplified_diagnosis %in% dx_list             # outcome dx only
  ) %>%
  dplyr::group_by(.data[[id_col]]) %>%
  dplyr::slice_min(order_by = Date_diff, n = 1, with_ties = FALSE) %>%  # earliest
  dplyr::ungroup() %>%
  dplyr::select(
    dplyr::all_of(id_col),
    Diagnosis      = Simplified_diagnosis,
    Diagnosis_Date = Date,
    Date_diff
  ) %>%
  dplyr::mutate(
    Injury_to_diagnosis_in_months = round(Date_diff / 30, 2)
  ) %>%
  dplyr::select(-Date_diff)

message("---- Incident cases ----")
message("Unique incident patients: ", dplyr::n_distinct(incident_dx[[id_col]]))
message("Breakdown by diagnosis:")
print(incident_dx %>% dplyr::count(Diagnosis) %>% dplyr::arrange(dplyr::desc(n)))

# -------------------------------------------------------
# 2. Pull demographics columns of interest
# -------------------------------------------------------
dem_cols <- c(
  id_col,
  "Index_Date",
  cfg$disease_col,                           # TBI_Status
  if (has_sub) "SUB_Status",
  "age_at_Index",
  "Gender_Legal_Sex"
)
# Keep only columns that actually exist
dem_cols <- dem_cols[dem_cols %in% names(Total_Dem)]

dem_subset <- Total_Dem %>%
  dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
  dplyr::select(dplyr::all_of(dem_cols)) %>%
  dplyr::distinct(.data[[id_col]], .keep_all = TRUE)

# -------------------------------------------------------
# 3. Pull imaging columns from patient_imaging
# -------------------------------------------------------
imaging_cols <- c(
  id_col,
  "Acute_CT_Date",      "Acute_CT_Date_diff",
  "Acute_MRI_Date",     "Acute_MRI_Date_diff",
  "FollowUp_CT_Date",   "FollowUp_CT_Date_diff",
  "FollowUp_MRI_Date",  "FollowUp_MRI_Date_diff",
  "Has_Acute_CT",       "Has_Acute_MRI",
  "Has_FollowUp_CT",    "Has_FollowUp_MRI"
)
imaging_cols <- imaging_cols[imaging_cols %in% names(patient_imaging)]

imaging_subset <- patient_imaging %>%
  dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
  dplyr::select(dplyr::all_of(imaging_cols)) %>%
  dplyr::distinct(.data[[id_col]], .keep_all = TRUE)

# -------------------------------------------------------
# 4. Build patient-level cases dataset
#    Only incident cases (inner join on incident_dx)
# -------------------------------------------------------
cases_detail <- incident_dx %>%
  dplyr::inner_join(dem_subset,     by = id_col) %>%
  dplyr::left_join(imaging_subset,  by = id_col) %>%
  dplyr::arrange(.data[[cfg$disease_col]], Injury_to_diagnosis_in_months)

message("Final incident cases dataset: ",
        nrow(cases_detail), " rows | ",
        dplyr::n_distinct(cases_detail[[id_col]]), " unique patients")

# -------------------------------------------------------
# 5. Summary sheet
# -------------------------------------------------------
n_cases       <- dplyr::n_distinct(cases_detail[[id_col]])
n_acute_ct    <- sum(cases_detail$Has_Acute_CT    == 1L, na.rm = TRUE)
n_acute_mri   <- sum(cases_detail$Has_Acute_MRI   == 1L, na.rm = TRUE)
n_fu_ct       <- sum(cases_detail$Has_FollowUp_CT  == 1L, na.rm = TRUE)
n_fu_mri      <- sum(cases_detail$Has_FollowUp_MRI == 1L, na.rm = TRUE)
n_acute_and_fu <- sum(
  (cases_detail$Has_Acute_CT == 1L | cases_detail$Has_Acute_MRI == 1L) &
    (cases_detail$Has_FollowUp_CT == 1L | cases_detail$Has_FollowUp_MRI == 1L),
  na.rm = TRUE
)

summary_sheet <- tibble::tibble(
  Metric  = c(
    "Incident cases (N)",
    "Has acute CT scan",
    "Has acute MRI scan",
    "Has follow-up CT scan",
    "Has follow-up MRI scan",
    "Has acute AND follow-up imaging (CT or MRI)",
    "",
    "--- Definitions ---",
    paste0("Acute imaging: closest scan within -2 to <= ", cfg$imaging_acute_days, " days of injury date"),
    paste0("Follow-up imaging: latest scan that is > ", cfg$imaging_followup_days, " days from injury date"),
    paste0("Diagnoses included: ", paste(cfg$outcome_dx_values, collapse = ", "))
  ),
  N       = c(n_cases, n_acute_ct, n_acute_mri, n_fu_ct, n_fu_mri, n_acute_and_fu, NA, NA, NA, NA, NA),
  Percent = c(
    100,
    round(100 * n_acute_ct  / n_cases, 1),
    round(100 * n_acute_mri / n_cases, 1),
    round(100 * n_fu_ct     / n_cases, 1),
    round(100 * n_fu_mri      / n_cases, 1),
    round(100 * n_acute_and_fu / n_cases, 1),
    NA, NA, NA, NA, NA
  )
)

message("\n---- Cases imaging summary ----")
print(summary_sheet)

# -------------------------------------------------------
# 6. Save: two sheets in one workbook using openxlsx
# -------------------------------------------------------
out_path <- file.path(
  cfg$output_dir,
  paste0(cfg$project_slug, "__incident_cases_with_imaging.xlsx")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Summary")
openxlsx::writeData(wb, "Summary", summary_sheet)

openxlsx::addWorksheet(wb, "Patient_Detail")
openxlsx::writeData(wb, "Patient_Detail", cases_detail)

openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
message("Saved incident cases workbook to: ", out_path)