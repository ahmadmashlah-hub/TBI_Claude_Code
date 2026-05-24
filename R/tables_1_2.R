# R/tables_1_2.R

.msg <- function(verbose, ...) if (isTRUE(verbose)) message(...)

# ---- Output naming helper ----
project_file <- function(cfg, name, ext) {
  file.path(
    cfg$output_dir,
    paste0(cfg$project_slug, "__", name, ".", ext)
  )
}

project_table_file <- function(cfg, name) {
  file.path(
    cfg$tables_dir,
    paste0(cfg$project_slug, "__", name, ".html")
  )
}

# ---- Utility: keep only columns that exist, warn about missing ----
keep_existing_cols <- function(df, cols, label = "columns", verbose = TRUE) {
  present <- intersect(cols, names(df))
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    .msg(verbose, "Missing ", label, " (dropping): ", paste(missing, collapse = ", "))
  }
  present
}

# -----------------------------
# Table 1: Baseline characteristics
# -----------------------------
make_table1 <- function(df, cfg, file_name = "Table1_baseline.html", verbose = TRUE) {
  group_col <- cfg$table_group_col
  stopifnot(group_col %in% names(df))
  
  vars <- keep_existing_cols(df, cfg$table1_vars, label = "Table 1 vars", verbose = verbose)
  if (length(vars) == 0) stop("No Table 1 variables exist in the dataset.")
  
  out_path <- project_table_file(cfg, "Table1_baseline")
  
  .msg(verbose, "---- Building Table 1 ----")
  .msg(verbose, "Grouping by: ", group_col)
  .msg(verbose, "Saving to: ", out_path)
  
  tab <- df %>%
    dplyr::select(dplyr::all_of(c(group_col, vars))) %>%
    gtsummary::tbl_summary(by = !!rlang::sym(group_col)) %>%
    gtsummary::bold_labels()
  
  tab %>% gtsummary::as_gt() %>% gt::gtsave(out_path)
  
  tab
}

# Table 1.5: Existing dx at baseline (patient-level wide from dx-level file)
#   - Uses Indexed_Diagnoses_Combined + WithinBaselineWindow
#   - Restricts to IDs in analysis_df (matched/unmatched)
#   - Excludes exposure dx labels if cfg$tbi_dx_values exists (optional)
# -----------------------------
make_table1_5_baseline_dx <- function(analysis_df,
                                      Indexed_Diagnoses_Combined,
                                      cfg,
                                      file_name = "Table1_5_baseline_dx.html",
                                      verbose = TRUE) {
  
  id_col <- cfg$id_col
  group_col <- cfg$table_group_col
  
  baseline_flag_col <- "WithinBaselineWindow"  # <- change ONLY if your column name differs
  dx_col <- "Simplified_diagnosis"
  
  stopifnot(id_col %in% names(analysis_df))
  stopifnot(group_col %in% names(analysis_df))
  stopifnot(all(c(id_col, dx_col, baseline_flag_col) %in% names(Indexed_Diagnoses_Combined)))
  
  .msg(verbose, "---- Building Table 1.5 (baseline dx) ----")
  .msg(verbose, "ID col: ", id_col, " | Group col: ", group_col)
  .msg(verbose, "Baseline flag col: ", baseline_flag_col)
  
  # IDs in cohort (matched or not)
  ids <- analysis_df %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
    dplyr::distinct(.data[[id_col]]) %>%
    dplyr::pull(.data[[id_col]])
  
  # Filter dx to baseline window + cohort IDs
  dx <- Indexed_Diagnoses_Combined %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
    dplyr::filter(.data[[id_col]] %in% ids) %>%
    dplyr::filter(.data[[baseline_flag_col]] == 1L)
  
  # Optional: drop exposure dx labels from baseline table (if present in cfg)
  if (!is.null(cfg$tbi_dx_values) && length(cfg$tbi_dx_values) > 0) {
    before_n <- nrow(dx)
    dx <- dx %>% dplyr::filter(!(.data[[dx_col]] %in% cfg$tbi_dx_values))
    .msg(verbose, "Dropped exposure dx labels (cfg$tbi_dx_values): removed ",
         before_n - nrow(dx), " rows")
  }
  
  # Make patient-level wide baseline dx indicators
  baseline_wide <- dx %>%
    dplyr::distinct(.data[[id_col]], .data[[dx_col]]) %>%
    dplyr::mutate(presence = 1L) %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(dx_col),
      values_from = presence,
      values_fill = 0L
    )
  
  # Join grouping var from analysis_df (ensures same sample)
  baseline_for_table <- analysis_df %>%
    dplyr::select(dplyr::all_of(c(id_col, group_col))) %>%
    dplyr::distinct() %>%
    dplyr::left_join(baseline_wide, by = setNames(id_col, id_col))
  
  # Replace NA indicators with 0
  ind_cols <- setdiff(names(baseline_for_table), c(id_col, group_col))
  if (length(ind_cols) == 0) {
    stop("Table 1.5 has 0 dx indicator columns. Check: baseline flag definition, dx labels, or pipeline inputs.")
  }
  
  baseline_for_table[ind_cols] <- lapply(baseline_for_table[ind_cols], function(x) {
    x[is.na(x)] <- 0L
    x
  })
  
  .msg(verbose, "Cohort unique IDs: ", dplyr::n_distinct(baseline_for_table[[id_col]]))
  .msg(verbose, "Baseline dx indicator columns: ", length(ind_cols))
  .msg(verbose, "Saving: ", file.path(cfg$tables_dir, file_name))
  

  out_path <- project_table_file(cfg, "Table1_5_baseline_dx")
  
  tab <- baseline_for_table %>%
    dplyr::select(dplyr::all_of(c(group_col, ind_cols))) %>%
    gtsummary::tbl_summary(by = !!rlang::sym(group_col)) %>%
    gtsummary::bold_labels()
  
  tab %>% gtsummary::as_gt() %>% gt::gtsave(out_path)
  tab
}

# Table 2 (UNFILTERED): all indicator columns in dataset
# -----------------------------
make_table2_unfiltered <- function(df,
                                   cfg,
                                   file_name = "Table2_unfiltered_all_incident_dx.html",
                                   verbose = TRUE) {
  group_col <- cfg$table_group_col
  stopifnot(group_col %in% names(df))

  # Candidate columns = everything except known metadata columns
  exclude <- unique(cfg$table2_unfiltered_exclude)
  exclude <- exclude[exclude %in% names(df)]

  candidates <- setdiff(names(df), exclude)

  # Keep only 0/1-like columns (logical, integer, numeric with values in {0,1,NA})
  is_binaryish <- function(x) {
    if (is.logical(x)) return(TRUE)
    if (is.numeric(x) || is.integer(x)) {
      vals <- unique(x[!is.na(x)])
      return(length(vals) == 0 || all(vals %in% c(0, 1)))
    }
    FALSE
  }

  binary_cols <- candidates[vapply(df[candidates], is_binaryish, logical(1))]

  if (length(binary_cols) == 0) {
    stop("No binary indicator columns found for unfiltered Table 2. ",
         "Update cfg$table2_unfiltered_exclude or check wide-building step.")
  }

  out_path <- project_table_file(cfg, "Table2_unfiltered_all_incident_dx")

  .msg(verbose, "---- Building Table 2 (UNFILTERED) ----")
  .msg(verbose, "Grouping by: ", group_col)
  .msg(verbose, "Binary indicator columns detected: ", length(binary_cols))
  .msg(verbose, "Saving to: ", out_path)

  tab <- df %>%
    dplyr::select(dplyr::all_of(c(group_col, binary_cols))) %>%
    gtsummary::tbl_summary(by = !!rlang::sym(group_col)) %>%
    gtsummary::bold_labels()

  tab %>% gtsummary::as_gt() %>% gt::gtsave(out_path)

  tab
}

# -----------------------------
# Table 2: Incident diagnoses (wide indicators), stacked by category
# -----------------------------
make_table2 <- function(df, cfg, categories = cfg$table2_categories,
                        file_name = "Table2_incident_dx.html", verbose = TRUE) {
  group_col <- cfg$table_group_col
  stopifnot(group_col %in% names(df))
  
  .msg(verbose, "---- Building Table 2 ----")
  .msg(verbose, "Grouping by: ", group_col)
  
  tbl_list <- list()
  headers <- c()
  
  for (nm in names(categories)) {
    cols <- categories[[nm]]
    cols_present <- keep_existing_cols(df, cols, label = paste0("Table 2 '", nm, "' vars"), verbose = verbose)
    
    if (length(cols_present) == 0) {
      .msg(verbose, "Skipping category '", nm, "' (no columns present).")
      next
    }
    
    tbl_list[[length(tbl_list) + 1]] <- df %>%
      dplyr::select(dplyr::all_of(c(group_col, cols_present))) %>%
      gtsummary::tbl_summary(by = !!rlang::sym(group_col))
    
    headers <- c(headers, nm)
  }
  
  if (length(tbl_list) == 0) stop("No Table 2 categories had any columns present in the dataset.")
  
  stacked <- gtsummary::tbl_stack(tbls = tbl_list, group_header = headers)
  out_path <- project_table_file(cfg, "Table2_filtered_categories")
  
  .msg(verbose, "Saving to: ", out_path)
  stacked %>% gtsummary::as_gt() %>% gt::gtsave(out_path)
  
  stacked
}

# -----------------------------
# Patient flow CSV
# -----------------------------
save_patient_flow <- function(patient_summary, cfg, file_name = "patient_flowchart_data.csv") {
  out_path <- project_file(cfg, "patient_flowchart", "csv")
  patient_flowchart_data <- data.frame(Description = patient_summary, stringsAsFactors = FALSE)
  write.csv(patient_flowchart_data, out_path, row.names = FALSE)
  utils::write.csv(patient_flowchart_data, out_path, row.names = FALSE)
  message("Saved patient flow CSV to: ", out_path)
  patient_flowchart_data
}