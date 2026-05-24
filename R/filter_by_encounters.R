#R/filter_by_encounters.R

filter_by_encounters <- function(final_Enc,
                                 Index_Dates,
                                 final_Dem,
                                 cfg,
                                 verbose = TRUE) {
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  # Required config
  stopifnot(!is.null(cfg$id_col))
  stopifnot(!is.null(cfg$min_enc_before_index))
  stopifnot(!is.null(cfg$min_enc_after_index))
  
  id_col <- cfg$id_col
  
  # ---- 0) Basic sanity checks ----
  required_enc_cols <- c(id_col, "Dates_before_index", "Dates_after_index", "Follow_up_duration")
  missing_enc <- setdiff(required_enc_cols, names(final_Enc))
  if (length(missing_enc) > 0) {
    stop("final_Enc is missing required columns: ", paste(missing_enc, collapse = ", "))
  }
  
  required_index_cols <- c(id_col, "Index_Date")
  missing_index <- setdiff(required_index_cols, names(Index_Dates))
  if (length(missing_index) > 0) {
    stop("Index_Dates is missing required columns: ", paste(missing_index, collapse = ", "))
  }
  
  if (!(id_col %in% names(final_Dem))) {
    stop("final_Dem is missing id column: ", id_col)
  }
  
  # Coerce IDs to character for safe joins
  final_Enc <- final_Enc %>% dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
  Index_Dates <- Index_Dates %>% dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
  final_Dem <- final_Dem %>% dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
  
  msg("---- filter_by_encounters() ----")
  msg("ID column: ", id_col)
  msg("Min encounters before index: ", cfg$min_enc_before_index)
  msg("Min encounters after index:  ", cfg$min_enc_after_index)
  
  msg("Input unique IDs:")
  msg("  final_Enc:    ", dplyr::n_distinct(final_Enc[[id_col]]))
  msg("  Index_Dates:  ", dplyr::n_distinct(Index_Dates[[id_col]]))
  msg("  final_Dem:    ", dplyr::n_distinct(final_Dem[[id_col]]))
  
  # ---- 1) Filter Encounters BEFORE index ----
  final_Enc_filtered <- final_Enc %>%
    dplyr::filter(Dates_before_index >= cfg$min_enc_before_index)
  
  msg("After BEFORE filter: rows=", nrow(final_Enc_filtered),
      " | unique IDs=", dplyr::n_distinct(final_Enc_filtered[[id_col]]))
  
  # ---- 2) Filter Encounters AFTER index ----
  final_Enc_filteredx2 <- final_Enc_filtered %>%
    dplyr::filter(Dates_after_index >= cfg$min_enc_after_index)
  
  msg("After AFTER filter: rows=", nrow(final_Enc_filteredx2),
      " | unique IDs=", dplyr::n_distinct(final_Enc_filteredx2[[id_col]]))
  
  # ---- 3) Keep needed cols + rename encounter counts ----
  final_Enc_filteredx2 <- final_Enc_filteredx2 %>%
    dplyr::select(
      dplyr::all_of(id_col),
      Dates_before_index,
      Dates_after_index,
      Follow_up_duration
    ) %>%
    dplyr::rename(
      `#_of_Encounters_before_Index` = Dates_before_index,
      `#_of_Encounters_after_Index`  = Dates_after_index
    )
  
  # ---- 4) Join with Index_Dates ----
  Index_Dates_Enced <- Index_Dates %>%
    dplyr::inner_join(final_Enc_filteredx2, by = setNames(id_col, id_col))
  
  msg("After join Index+Enc: rows=", nrow(Index_Dates_Enced),
      " | unique IDs=", dplyr::n_distinct(Index_Dates_Enced[[id_col]]))
  
  # ---- 5) Join with Demographics ----
  merged_Index_Enc_Dem <- Index_Dates_Enced %>%
    dplyr::inner_join(final_Dem, by = setNames(id_col, id_col))
  
  msg("After join (+Dem): rows=", nrow(merged_Index_Enc_Dem),
      " | unique IDs=", dplyr::n_distinct(merged_Index_Enc_Dem[[id_col]]))
  
  # ---- 6) Age at index ----
  merged_Index_Enc_Dem_age <- merged_Index_Enc_Dem %>%
    dplyr::mutate(
      Date_of_Birth = lubridate::mdy(Date_of_Birth, quiet = TRUE),
      age_at_Index = round(as.numeric(difftime(Index_Date, Date_of_Birth, units = "days")) / 365.25)
    )
  
  msg("Age calc: NA DOBs=", sum(is.na(merged_Index_Enc_Dem_age$Date_of_Birth)),
      " | NA ages=", sum(is.na(merged_Index_Enc_Dem_age$age_at_Index)))
  
  # Return everything you were using downstream
  list(
    final_Exp_Enc_filtered    = final_Enc_filtered,
    final_Exp_Enc_filteredx2  = final_Enc_filteredx2,
    Index_Dates_Enced         = Index_Dates_Enced,
    merged_Index_Enc_Dem      = merged_Index_Enc_Dem,
    merged_Index_Enc_Dem_age  = merged_Index_Enc_Dem_age
  )
}
