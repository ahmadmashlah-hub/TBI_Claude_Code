build_index_dates <- function(final_dataframe,
                              id_col,
                              dx_date_col,
                              dx_name_col,
                              index_dx_values,
                              verbose = TRUE,
                              show_examples = 10) {
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  # ---- 0) Basic sanity checks ----
  msg("---- build_index_dates() diagnostics ----")
  msg("Rows in final_dataframe: ", nrow(final_dataframe))
  msg("Cols in final_dataframe: ", ncol(final_dataframe))
  msg("Using id_col='", id_col, "', dx_date_col='", dx_date_col,
      "', dx_name_col='", dx_name_col, "'")
  
  missing_cols <- setdiff(c(id_col, dx_date_col, dx_name_col), names(final_dataframe))
  if (length(missing_cols) > 0) {
    msg("ERROR: Missing required columns: ", paste(missing_cols, collapse = ", "))
    msg("Available columns (first 30): ", paste(head(names(final_dataframe), 30), collapse = ", "))
    stop("Missing columns in final_dataframe; cannot build Index_Dates.")
  }
  
  # ---- 1) Check diagnosis label overlap ----
  dx_vals <- final_dataframe[[dx_name_col]]
  dx_vals <- as.character(dx_vals)
  
  msg("Unique dx labels in final_dataframe: ", length(unique(dx_vals)))
  msg("Index dx values provided: ", length(index_dx_values))
  
  overlap <- intersect(unique(dx_vals), index_dx_values)
  msg("Overlap between dx labels and index_dx_values: ", length(overlap))
  
  if (length(overlap) == 0) {
    msg("WARNING: No overlap found. This will produce an empty Index_Dates.")
    msg("Examples of dx labels in data (first ", show_examples, "): ",
        paste(head(sort(unique(dx_vals)), show_examples), collapse = " | "))
    msg("Examples of index_dx_values (first ", show_examples, "): ",
        paste(head(index_dx_values, show_examples), collapse = " | "))
    msg("Tip: check capitalization/spelling OR whether you meant SCI/TBI labels OR whether dx_name_col is correct.")
  }
  
  # ---- 2) Subset to needed columns ----
  df_small <- final_dataframe %>%
    dplyr::select(dplyr::all_of(c(id_col, dx_date_col, dx_name_col))) %>%
    dplyr::mutate(
      "{dx_name_col}" := as.character(.data[[dx_name_col]])
    )
  
  msg("After select(): rows=", nrow(df_small))
  
  # ---- 3) Filter to index diagnoses ----
  df_dx <- df_small %>%
    dplyr::filter(.data[[dx_name_col]] %in% index_dx_values)
  
  msg("After dx filter: rows=", nrow(df_dx),
      " | unique patients=", dplyr::n_distinct(df_dx[[id_col]]))
  
  if (nrow(df_dx) == 0) {
    msg("STOPPING EARLY: dx filter removed everything.")
    return(df_dx %>% dplyr::transmute(!!id_col := .data[[id_col]], Index_Date = as.Date(NA)))
  }
  
  # ---- 4) Parse dates robustly ----
  x <- df_dx[[dx_date_col]]
  
  # If already Date/POSIXct, keep; else try mdy parsing
  if (inherits(x, "Date")) {
    msg("dx_date_col already class Date.")
    df_dx[[dx_date_col]] <- x
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    msg("dx_date_col is POSIXt; converting to Date.")
    df_dx[[dx_date_col]] <- as.Date(x)
  } else {
    msg("dx_date_col class is ", paste(class(x), collapse = "/"),
        " → attempting lubridate::mdy() parse.")
    parsed <- lubridate::mdy(as.character(x), quiet = TRUE)
    df_dx[[dx_date_col]] <- parsed
  }
  
  na_dates <- sum(is.na(df_dx[[dx_date_col]]))
  msg("After date parse: NA dates=", na_dates, " out of ", nrow(df_dx))
  
  if (na_dates == nrow(df_dx)) {
    msg("WARNING: All dates failed to parse. Likely wrong format (not mdy).")
    msg("Examples of raw date strings (first ", show_examples, "): ",
        paste(head(as.character(x), show_examples), collapse = " | "))
    msg("Tip: if your dates are YMD (e.g., 2020-01-31) use lubridate::ymd().")
  }
  
  # ---- 5) Drop NA dates ----
  df_dx2 <- df_dx %>% dplyr::filter(!is.na(.data[[dx_date_col]]))
  msg("After removing NA dates: rows=", nrow(df_dx2),
      " | unique patients=", dplyr::n_distinct(df_dx2[[id_col]]))
  
  if (nrow(df_dx2) == 0) {
    msg("STOPPING EARLY: all rows removed due to NA dates after parsing.")
    return(df_dx2 %>% dplyr::transmute(!!id_col := .data[[id_col]], Index_Date = as.Date(NA)))
  }
  
  # ---- 6) Build Index_Dates: earliest per patient ----
  Index_Dates <- df_dx2 %>%
    dplyr::arrange(.data[[id_col]], .data[[dx_date_col]]) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      Index_Date = .data[[dx_date_col]]
    )
  
  msg("FINAL Index_Dates: rows=", nrow(Index_Dates),
      " | unique patients=", dplyr::n_distinct(Index_Dates[[id_col]]))
  
  # small peek
  if (isTRUE(verbose)) {
    msg("Index_Dates preview:")
    print(utils::head(Index_Dates, show_examples))
  }
  
  Index_Dates
}