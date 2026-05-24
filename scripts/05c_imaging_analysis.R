# scripts/05c_imaging_analysis.R
# Imaging analysis: acute (Date_diff >= cfg$imaging_acute_neg_days & <= cfg$imaging_acute_days) and
# follow-up (Date_diff > cfg$imaging_followup_days) CT and MRI around TBI index date.
#
# Outputs:
#   1. Patient-level CSV: one row per patient with closest acute and
#      furthest follow-up scan dates for CT and MRI separately
#   2. Summary table CSV by TBI_Status group
#   3. Summary table CSV by SUB_Status group (if SUB_Status exists in Total_Dem)

source("R/00_packages.R")
source("R/01_config.R")
source("R/read_imaging.R")

dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------
# 0. Valid cohort IDs + Index Dates (from prior steps)
#    Total_Dem must already be in environment from step 05/06
# -------------------------------------------------------
stopifnot(exists("Total_Dem"), cfg$id_col %in% names(Total_Dem), "Index_Date" %in% names(Total_Dem))

id_col    <- cfg$id_col
has_sub   <- "SUB_Status" %in% names(Total_Dem)

if (!has_sub) message("Note: SUB_Status not found in Total_Dem — sub-group summary will be skipped.")

# Pull grouping columns that exist
group_cols <- c(cfg$disease_col, if (has_sub) "SUB_Status")

index_dates <- Total_Dem %>%
  dplyr::select(dplyr::all_of(c(id_col, "Index_Date", group_cols))) %>%
  dplyr::mutate(
    !!id_col   := as.character(.data[[id_col]]),
    Index_Date  = as.Date(Index_Date)
  ) %>%
  dplyr::distinct(.data[[id_col]], .keep_all = TRUE)

message("Cohort size for imaging analysis: ", dplyr::n_distinct(index_dates[[id_col]]), " patients")

# -------------------------------------------------------
# 1. Read + classify all _Rdt.txt files
# -------------------------------------------------------
imaging_raw <- read_and_classify_imaging(cfg, index_dates[[id_col]], verbose = TRUE)

# -------------------------------------------------------
# 2. Join Index_Date and compute Date_diff
# -------------------------------------------------------
imaging <- imaging_raw %>%
  dplyr::left_join(
    index_dates %>% dplyr::select(dplyr::all_of(c(id_col, "Index_Date"))),
    by = setNames(id_col, id_col)
  ) %>%
  dplyr::mutate(
    Date_diff = as.numeric(difftime(Imaging_Date, Index_Date, units = "days"))
  )

message("Rows after joining index dates: ", nrow(imaging))
message("Rows with NA Date_diff (no index date match): ", sum(is.na(imaging$Date_diff)))

# -------------------------------------------------------
# 3. Split into acute and follow-up windows
# -------------------------------------------------------
imaging_acute <- imaging %>%
  dplyr::filter(Date_diff >= cfg$imaging_acute_neg_days, Date_diff <= cfg$imaging_acute_days)

imaging_followup <- imaging %>%
  dplyr::filter(Date_diff > cfg$imaging_followup_days)

message("Acute window rows (>= ", cfg$imaging_acute_neg_days,
        " to <= ", cfg$imaging_acute_days, " days): ", nrow(imaging_acute))
message("Follow-up window rows (> ", cfg$imaging_followup_days, " days): ", nrow(imaging_followup))

# -------------------------------------------------------
# 4. Per-patient summaries
#    Acute    -> closest scan to index (smallest abs Date_diff)
#    Follow-up -> furthest scan from index (largest Date_diff)
# -------------------------------------------------------
summarise_window <- function(df, id_col, imaging_types, pick = c("closest", "furthest")) {
  pick <- match.arg(pick)

  lapply(imaging_types, function(itype) {
    sub <- df %>%
      dplyr::filter(Imaging_Type == itype) %>%
      dplyr::group_by(.data[[id_col]])

    if (pick == "closest") {
      sub <- sub %>%
        dplyr::slice_min(order_by = abs(Date_diff), n = 1, with_ties = FALSE)
    } else {
      sub <- sub %>%
        dplyr::slice_max(order_by = Date_diff, n = 1, with_ties = FALSE)
    }

    label <- if (pick == "closest") "Acute" else "FollowUp"
    sub %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(id_col), Imaging_Date, Date_diff) %>%
      dplyr::rename(
        !!paste0(label, "_", itype, "_Date")      := Imaging_Date,
        !!paste0(label, "_", itype, "_Date_diff") := Date_diff
      )
  }) %>%
    purrr::reduce(dplyr::full_join, by = id_col)
}

imaging_types <- c("CT", "MRI")

acute_summary    <- summarise_window(imaging_acute,    id_col, imaging_types, pick = "closest")
followup_summary <- summarise_window(imaging_followup, id_col, imaging_types, pick = "furthest")

# -------------------------------------------------------
# 5. Build patient-level dataset
#    Start from full cohort so every patient appears,
#    even those with no imaging
# -------------------------------------------------------
patient_imaging <- index_dates %>%
  dplyr::left_join(acute_summary,    by = id_col) %>%
  dplyr::left_join(followup_summary, by = id_col) %>%
  dplyr::mutate(
    Has_Acute_CT     = as.integer(!is.na(Acute_CT_Date)),
    Has_Acute_MRI    = as.integer(!is.na(Acute_MRI_Date)),
    Has_FollowUp_CT  = as.integer(!is.na(FollowUp_CT_Date)),
    Has_FollowUp_MRI = as.integer(!is.na(FollowUp_MRI_Date))
  )

# -------------------------------------------------------
# 6. Missingness report (patients with NO scan in each category)
# -------------------------------------------------------
n_total        <- nrow(patient_imaging)
n_no_acute_ct  <- sum(patient_imaging$Has_Acute_CT     == 0)
n_no_acute_mri <- sum(patient_imaging$Has_Acute_MRI    == 0)
n_no_fu_ct     <- sum(patient_imaging$Has_FollowUp_CT  == 0)
n_no_fu_mri    <- sum(patient_imaging$Has_FollowUp_MRI == 0)
n_no_any_acute <- sum(patient_imaging$Has_Acute_CT  == 0 & patient_imaging$Has_Acute_MRI == 0)
n_no_any_fu    <- sum(patient_imaging$Has_FollowUp_CT == 0 & patient_imaging$Has_FollowUp_MRI == 0)

message("\n---- Imaging coverage report (N = ", n_total, " patients) ----")
message(sprintf("  No acute CT              : %d / %d (%.1f%%)", n_no_acute_ct,  n_total, 100 * n_no_acute_ct  / n_total))
message(sprintf("  No acute MRI             : %d / %d (%.1f%%)", n_no_acute_mri, n_total, 100 * n_no_acute_mri / n_total))
message(sprintf("  No follow-up CT          : %d / %d (%.1f%%)", n_no_fu_ct,     n_total, 100 * n_no_fu_ct     / n_total))
message(sprintf("  No follow-up MRI         : %d / %d (%.1f%%)", n_no_fu_mri,    n_total, 100 * n_no_fu_mri    / n_total))
message(sprintf("  No acute scan at all     : %d / %d (%.1f%%)", n_no_any_acute, n_total, 100 * n_no_any_acute / n_total))
message(sprintf("  No follow-up scan at all : %d / %d (%.1f%%)", n_no_any_fu,    n_total, 100 * n_no_any_fu    / n_total))

# -------------------------------------------------------
# 7. Reusable summary function (used for both group columns)
# -------------------------------------------------------
make_imaging_summary <- function(df, group_col) {
  df %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      N                      = dplyr::n(),
      N_Acute_CT             = sum(Has_Acute_CT),
      Pct_Acute_CT           = round(100 * N_Acute_CT / N, 1),
      N_Acute_MRI            = sum(Has_Acute_MRI),
      Pct_Acute_MRI          = round(100 * N_Acute_MRI / N, 1),
      N_Acute_CT_or_MRI      = sum(Has_Acute_CT == 1 | Has_Acute_MRI == 1),
      Pct_Acute_CT_or_MRI    = round(100 * N_Acute_CT_or_MRI / N, 1),
      N_FollowUp_CT          = sum(Has_FollowUp_CT),
      Pct_FollowUp_CT        = round(100 * N_FollowUp_CT / N, 1),
      N_FollowUp_MRI         = sum(Has_FollowUp_MRI),
      Pct_FollowUp_MRI       = round(100 * N_FollowUp_MRI / N, 1),
      N_FollowUp_CT_or_MRI   = sum(Has_FollowUp_CT == 1 | Has_FollowUp_MRI == 1),
      Pct_FollowUp_CT_or_MRI = round(100 * N_FollowUp_CT_or_MRI / N, 1),
      .groups = "drop"
    )
}

# -------------------------------------------------------
# 8. Summary by TBI_Status
# -------------------------------------------------------
message("\n---- Summary by ", cfg$disease_col, " ----")
summary_by_tbi <- make_imaging_summary(patient_imaging, cfg$disease_col)
print(summary_by_tbi)

# -------------------------------------------------------
# 9. Summary by SUB_Status (if available)
# -------------------------------------------------------
if (has_sub) {
  message("\n---- Summary by SUB_Status ----")
  summary_by_sub <- make_imaging_summary(patient_imaging, "SUB_Status")
  print(summary_by_sub)
} else {
  summary_by_sub <- NULL
}

# -------------------------------------------------------
# 10. Save outputs
# -------------------------------------------------------
patient_out     <- file.path(cfg$output_dir, paste0(cfg$project_slug, "__imaging_patient_level.csv"))
summary_tbi_out <- file.path(cfg$output_dir, paste0(cfg$project_slug, "__imaging_summary_by_TBI_Status.csv"))

utils::write.csv(patient_imaging,  patient_out,     row.names = FALSE)
utils::write.csv(summary_by_tbi,   summary_tbi_out, row.names = FALSE)
message("Saved patient-level imaging CSV to:       ", patient_out)
message("Saved TBI_Status summary CSV to:          ", summary_tbi_out)

if (!is.null(summary_by_sub)) {
  summary_sub_out <- file.path(cfg$output_dir, paste0(cfg$project_slug, "__imaging_summary_by_SUB_Status.csv"))
  utils::write.csv(summary_by_sub, summary_sub_out, row.names = FALSE)
  message("Saved SUB_Status summary CSV to:          ", summary_sub_out)
}
