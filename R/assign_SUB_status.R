# R/05b_assign_sub_status.R

assign_sub_status <- function(Indexed_Diagnoses_Combined,
                              Total_Dem,
                              cfg,
                              verbose = TRUE) {
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  id_col <- cfg$id_col
  stopifnot(id_col %in% names(Total_Dem))
  stopifnot(all(c(id_col, "Date", "Index_Date", "Simplified_diagnosis") %in% names(Indexed_Diagnoses_Combined)))
  
  sub_dx <- cfg$sub_dx_values
  window_days <- cfg$sub_window_days
  
  # --- Include ALL IDs (no exposure_id filtering) ---
  stopifnot("TBI_Status" %in% names(Total_Dem))
  all_ids <- Total_Dem %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
    dplyr::distinct(.data[[id_col]]) %>%
    dplyr::pull(.data[[id_col]])
  
  msg("---- SUB: assigning SUB_Status ----")
  msg("All IDs: ", length(all_ids))
  
  dx_all <- Indexed_Diagnoses_Combined %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      Date = as.Date(Date),
      Index_Date = as.Date(Index_Date),
      Date_diff = as.numeric(difftime(Date, Index_Date, units = "days"))
    ) %>%
    dplyr::filter(.data[[id_col]] %in% all_ids)
  
  # --- PRIOR SUB: any SUB diagnosis BEFORE index (not removed; explicitly labeled) ---
  prior_window_days <- if (!is.null(cfg$sub_prior_window_days)) cfg$sub_prior_window_days else Inf
  
  prior_sub_yes <- dx_all %>%
    dplyr::filter(Simplified_diagnosis %in% sub_dx) %>%
    dplyr::filter(!is.na(Date_diff)) %>%
    dplyr::filter(Date_diff < 0) %>%                            # BEFORE index
    dplyr::filter(abs(Date_diff) <= prior_window_days) %>%      # optional lookback cap
    dplyr::arrange(.data[[id_col]], dplyr::desc(Date)) %>%      # most recent PRIOR date
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      Prior_SUB = TRUE,
      Prior_SUB_Date = Date
    )
  
  # --- Determine SUB Yes/No within window (per ID) ---
  sub_yes <- dx_all %>%
    dplyr::filter(Simplified_diagnosis %in% sub_dx) %>%
    dplyr::filter(!is.na(Date_diff)) %>%
    {
      if (isTRUE(cfg$sub_require_after_index)) dplyr::filter(., Date_diff >= 0) else .
    } %>%
    dplyr::filter(Date_diff <= window_days) %>%
    dplyr::arrange(.data[[id_col]], Date) %>%   # earliest SUB dx date
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      SUB_Any = TRUE,
      SUB_Dx_Date = Date
    )
  
  msg("IDs with SUB_Any=TRUE: ", dplyr::n_distinct(sub_yes[[id_col]]))
  
  sub_flag_all <- tibble::tibble(!!id_col := all_ids) %>%
    dplyr::left_join(sub_yes,       by = setNames(id_col, id_col)) %>%
    dplyr::left_join(prior_sub_yes, by = setNames(id_col, id_col)) %>%
    dplyr::mutate(
      SUB_Any    = dplyr::if_else(is.na(SUB_Any), FALSE, SUB_Any),
      Prior_SUB  = dplyr::if_else(is.na(Prior_SUB), FALSE, Prior_SUB)
    )
  # --- Build 4-category SUB_Status outcome ---
  # Categories:
  # 1) cfg$index_dx_set_name AND SUB
  # 2) cfg$index_dx_set_name Only
  # 3) SUB Only
  # 4) None
  # Code-friendly index code (e.g., "tbi")
  index_code <- tolower(gsub("[^A-Za-z0-9]+", "_", cfg$index_dx_set_name))
  
  # Prevent join suffix issues if re-running
  Total_Dem_clean <- Total_Dem %>%
    dplyr::select(-dplyr::any_of(c("SUB_Any","Prior_SUB","SUB_Dx_Date","Prior_SUB_Date","SUB_Status")))
  
  Total_Dem_with_SUB <- Total_Dem_clean %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
    dplyr::left_join(sub_flag_all, by = setNames(id_col, id_col)) %>%
    dplyr::mutate(
      # Ensure these are present/usable
      SUB_Any   = dplyr::coalesce(SUB_Any, FALSE),
      Prior_SUB = dplyr::coalesce(Prior_SUB, FALSE),
      
      Has_Index = .data[["TBI_Status"]] != "None",
      
      # Prior_SUB checked FIRST if it overrides
      SUB_Status = dplyr::case_when(
        Prior_SUB            ~ "prior_sub",
        Has_Index & SUB_Any  ~ paste0(index_code, "_and_sub"),
        Has_Index & !SUB_Any ~ paste0(index_code, "_only"),
        !Has_Index & SUB_Any ~ "sub_only",
        TRUE                 ~ "none"
      ),
      
      SUB_Status = factor(
        SUB_Status,
        levels = c("none", "sub_only", paste0(index_code, "_only"), paste0(index_code, "_and_sub"), "prior_sub")
      )
    ) %>%
    dplyr::select(-Has_Index)
  
  # Attach same 4-category status to diagnoses-level table too
  Indexed_Diagnoses_Combined_with_SUB <- Indexed_Diagnoses_Combined %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]])) %>%
    dplyr::left_join(
      Total_Dem_with_SUB %>% dplyr::select(.data[[id_col]], SUB_Status, SUB_Any, SUB_Dx_Date),
      by = setNames(id_col, id_col)
    )
  
  msg("SUB_Status counts (all IDs):")
  print(Total_Dem_with_SUB %>% dplyr::count(SUB_Status))
  
  list(
    SUB_Status = Total_Dem_with_SUB %>% dplyr::select(.data[[id_col]], SUB_Status, SUB_Any, SUB_Dx_Date),
    Total_Dem_with_SUB = Total_Dem_with_SUB,
    Indexed_Diagnoses_Combined_with_SUB = Indexed_Diagnoses_Combined_with_SUB
  )
}
