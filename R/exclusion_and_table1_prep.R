# R/exclusion_and_table1_prep.R

# Helper: message wrapper
.msg <- function(verbose, ...) if (isTRUE(verbose)) message(...)

# -----------------------------
# A) Baseline exclusion IDs
# -----------------------------
get_baseline_exclusion_ids <- function(Indexed_Diagnoses_Combined, cfg, verbose = TRUE) {
  id_col <- cfg$id_col
  dx_list <- cfg$outcome_dx_values
  window_days <- cfg$baseline_exclusion_days
  
  stopifnot(id_col %in% names(Indexed_Diagnoses_Combined))
  stopifnot(all(c("Date", "Index_Date", "Simplified_diagnosis") %in% names(Indexed_Diagnoses_Combined)))
  
  df <- Indexed_Diagnoses_Combined %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      WithinBaselineWindow = dplyr::if_else(Date_diff < window_days, 1L, 0L),
      ToBeExcluded = dplyr::if_else(
        Simplified_diagnosis %in% dx_list & WithinBaselineWindow == 1L,
        1L, 0L
      )
    )
  
  excl_ids <- df %>%
    dplyr::filter(ToBeExcluded == 1L) %>%
    dplyr::distinct(.data[[id_col]]) %>%
    dplyr::pull(.data[[id_col]])
  
  .msg(verbose, "---- Baseline exclusion ----")
  .msg(verbose, "Outcome dx list: ", paste(dx_list, collapse = ", "))
  .msg(verbose, "Window: Date_diff < ", window_days, " days")
  .msg(verbose, "IDs excluded: ", length(excl_ids))
  
  list(
    df_with_flags = df,
    Excl_IDs = excl_ids
  )
}

# -----------------------------
# B) Build WIDE "AFTER" dataset
#    AFTER is defined as WithinBaselineWindow == 0 (i.e., Date_diff >= window_days)
# -----------------------------
build_wide_after <- function(df_with_flags, Total_Dem, cfg, verbose = TRUE) {
  id_col <- cfg$id_col
  
  stopifnot(id_col %in% names(Total_Dem))
  stopifnot(all(c(id_col, "Simplified_diagnosis", "WithinBaselineWindow") %in% names(df_with_flags)))
  
  # ---- Check if incidents column exists ----
  has_incidents_col <- "incidents_of_Simplified_Diagnosis" %in% names(df_with_flags)
  min_dx_dates <- cfg$min_dx_dates %||% 1L

  if (!has_incidents_col && min_dx_dates > 1) {
    warning(
      "cfg$min_dx_dates = ", min_dx_dates,
      " but 'incidents_of_Simplified_Diagnosis' column not found in dx data. ",
      "Re-run scripts 01 and 02 to generate this column. No filtering applied."
    )
    min_dx_dates <- 1L
  }

  # ---- Apply min_dx_dates filter BEFORE pivoting ----
  # Applies to ALL diagnoses. Any dx with fewer distinct date occurrences
  # than cfg$min_dx_dates is dropped. The downstream code will select
  # outcome_dx_values as needed.
  dx_filtered <- df_with_flags

  if (has_incidents_col && min_dx_dates > 1) {
    .msg(verbose, "---- min_dx_dates filter (cfg$min_dx_dates = ", min_dx_dates, ") ----")
    .msg(verbose, "Diagnosis rows before filter: ", nrow(df_with_flags))

    dx_filtered <- dx_filtered %>%
      dplyr::filter(incidents_of_Simplified_Diagnosis >= min_dx_dates)

    .msg(verbose, "Diagnosis rows after filter:  ", nrow(dx_filtered))
  } else {
    .msg(verbose, "min_dx_dates = 1 → no incident-count filtering applied.")
  }

  # "After" = NOT within baseline window
  dx_after <- dx_filtered %>%
    dplyr::filter(WithinBaselineWindow == 0L) %>%
    dplyr::select(dplyr::all_of(c(id_col, "Simplified_diagnosis"))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(presence = 1L) %>%
    tidyr::pivot_wider(
      names_from  = Simplified_diagnosis,
      values_from = presence,
      values_fill = list(presence = 0L),
      values_fn   = list(presence = max)
    )
  
  Total_Dem_dedup <- Total_Dem %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
    dplyr::distinct(.data[[id_col]], .keep_all = TRUE)
  
  wide <- Total_Dem_dedup %>%
    dplyr::left_join(dx_after, by = setNames(id_col, id_col))
  
  # Replace NA in dx indicator columns with 0, but do NOT stomp demographic NAs
  dx_cols <- setdiff(names(wide), names(Total_Dem_dedup))
  if (length(dx_cols) > 0) {
    wide[dx_cols] <- lapply(wide[dx_cols], function(x) { x[is.na(x)] <- 0L; x })
  }
  
  .msg(verbose, "---- Wide AFTER dataset ----")
  .msg(verbose, "Total_Dem unique IDs: ", dplyr::n_distinct(Total_Dem_dedup[[id_col]]))
  .msg(verbose, "Wide AFTER rows: ", nrow(wide))
  .msg(verbose, "Number of dx indicator columns added: ", length(dx_cols))
  
  wide
}

# -----------------------------
# C) Cleaning for Table 1
#    (supports cfg$status_col = "TBI_Status" OR "SUB_Status")
# -----------------------------
clean_for_table1 <- function(wide_df, cfg, verbose = TRUE) {
  id_col <- cfg$id_col
  status_col <- cfg$status_col
  
  stopifnot(id_col %in% names(wide_df))
  stopifnot(status_col %in% names(wide_df))
  stopifnot("Race1" %in% names(wide_df))
  stopifnot("Gender_Legal_Sex" %in% names(wide_df))
  stopifnot("age_at_Index" %in% names(wide_df))
  
  # --- Race lumping diagnostics ---
  races_before <- sort(unique(as.character(wide_df$Race1)))
  races_lumped <- setdiff(races_before, cfg$race_of_interest)
  
  if (length(races_lumped) > 0) {
    .msg(verbose, "Race values that will be lumped into 'Other': ",
         paste(races_lumped, collapse = ", "))
  } else {
    .msg(verbose, "No race values will be lumped into 'Other'.")
  }
  
  # --- Gender lumping diagnostics ---
  genders_before <- sort(unique(as.character(wide_df$Gender_Legal_Sex)))
  genders_to_unknown <- intersect(genders_before, cfg$gender_unknown_values)
  
  if (length(genders_to_unknown) > 0) {
    .msg(verbose, "Gender values that will be recoded to '", cfg$gender_unknown_label, "': ",
         paste(genders_to_unknown, collapse = ", "))
  } else {
    .msg(verbose, "No gender values will be recoded to '", cfg$gender_unknown_label, "'.")
  }
  
  # --- Status lumping diagnostics ---
  statuses_before <- sort(unique(as.character(wide_df[[status_col]])))
  statuses_lumped <- intersect(statuses_before, cfg$status_lump_values)
  
  if (length(statuses_lumped) > 0) {
    .msg(verbose, status_col, " values that will be lumped into '", cfg$status_lump_to, "': ",
         paste(statuses_lumped, collapse = ", "))
  }
  
  # Do the cleaning
  out <- wide_df %>%
    dplyr::mutate(
      Race1 = dplyr::case_when(
        Race1 %in% cfg$race_of_interest ~ Race1,
        TRUE ~ "Other"
      ),
      Gender_Legal_Sex = dplyr::case_when(
        Gender_Legal_Sex %in% cfg$gender_unknown_values ~ cfg$gender_unknown_label,
        TRUE ~ Gender_Legal_Sex
      )
    )
  
  # Composites (only if columns exist)
  abuse_inputs <- cfg$other_abuse_inputs
  abuse_inputs <- abuse_inputs[abuse_inputs %in% names(out)]
  if (length(abuse_inputs) > 0) {
    out <- out %>%
      dplyr::mutate(
        !!cfg$other_abuse_name := as.integer(rowSums(dplyr::across(dplyr::all_of(abuse_inputs))) > 0)
      )
  } else {
    .msg(verbose, "Composite not created: none of cfg$other_abuse_inputs exist in dataset.")
  }
  
  ihd_inputs <- cfg$ihd_inputs
  ihd_inputs <- ihd_inputs[ihd_inputs %in% names(out)]
  if (length(ihd_inputs) > 0) {
    out <- out %>%
      dplyr::mutate(
        !!cfg$ihd_name := as.integer(rowSums(dplyr::across(dplyr::all_of(ihd_inputs))) > 0)
      )
  } else {
    .msg(verbose, "Composite not created: none of cfg$ihd_inputs exist in dataset.")
  }
  
  # Collapse Moderate/Severe if requested
  if (isTRUE(cfg$collapse_modsev)) {
    out <- out %>%
      dplyr::mutate(
        !!status_col := dplyr::if_else(
          .data[[status_col]] %in% cfg$modsev_values,
          cfg$modsev_label,
          .data[[status_col]]
        )
      )
  }
  
  # Drop unwanted status values (e.g., Unclassified)
  if (length(cfg$drop_status_values) > 0) {
    out <- out %>% dplyr::filter(!(.data[[status_col]] %in% cfg$drop_status_values))
  }
  
  # Adults only
  out <- out %>% dplyr::filter(age_at_Index >= cfg$adult_min_age)
  
  # Treatment flag (status != "None")
  out <- out %>%
    dplyr::mutate(Treatment = dplyr::if_else(.data[[status_col]] == "None", 0L, 1L))
  
  .msg(verbose, "---- Table 1 cleaned dataset ----")
  .msg(verbose, "Rows: ", nrow(out), " | Unique IDs: ", dplyr::n_distinct(out[[id_col]]))
  .msg(verbose, "Using status column: ", status_col)
  
  out
}

# Null coalescing operator (in case not loaded)
`%||%` <- function(x, y) if (!is.null(x)) x else y
