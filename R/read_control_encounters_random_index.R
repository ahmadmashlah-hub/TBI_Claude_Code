build_control_encounters_with_random_index <- function(cfg,
                                                       Enc_select_columns = c(cfg$id_col, "Admit_Date", "Inpatient_Outpatient"),
                                                       verbose = TRUE,
                                                       sample_preview_n = 50) {
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  id_col <- cfg$id_col
  set.seed(cfg$control_index_seed)
  
  # ---- list Enc + Control files ----
  all_files <- list.files(cfg$data_dir, full.names = TRUE)
  Enc_Control_files <- all_files[grepl("Enc", all_files) & grepl("Control", all_files)]
  n_files <- length(Enc_Control_files)
  
  msg("Control encounter files found: ", n_files)
  if (n_files == 0) stop("No encounter files matched Enc + Control in: ", cfg$data_dir)
  
  read_one_file <- function(file_path, i) {
    msg(sprintf("Processing control Enc file %d / %d: %s", i, n_files, basename(file_path)))
    
    df <- read.table(
      file_path,
      header = TRUE,
      sep = "|",
      quote = "",
      fill = TRUE,
      stringsAsFactors = FALSE
    )
    
    # Safe select: keep only columns that exist
    cols_present <- intersect(Enc_select_columns, names(df))
    df <- df %>% dplyr::select(dplyr::all_of(cols_present))
    
    # Coerce ID + parse date
    df <- df %>%
      dplyr::mutate(
        !!id_col := as.character(.data[[id_col]]),
        Admit_Date = lubridate::mdy(as.character(Admit_Date), quiet = TRUE)
      ) %>%
      dplyr::filter(!is.na(Admit_Date)) %>%
      dplyr::distinct(.data[[id_col]], Admit_Date, .keep_all = TRUE)
    
    msg(sprintf("  → rows=%d | unique IDs=%d", nrow(df), dplyr::n_distinct(df[[id_col]])))
    df
  }
  
  enc_all <- lapply(seq_along(Enc_Control_files), function(i) read_one_file(Enc_Control_files[[i]], i)) %>%
    dplyr::bind_rows()
  
  msg("After binding all control encounters: rows=", nrow(enc_all),
      " | unique IDs=", dplyr::n_distinct(enc_all[[id_col]]))
  
 
  
  # ---- reproducible random index date per EMPI (from first half) ----
  # IMPORTANT: to make this robust to file/order changes, we sample per-EMPI with a deterministic per-ID seed.
  sample_index_date <- function(dates, empi_chr) {
    dates <- sort(dates)
    if (length(dates) == 0) return(as.Date(NA))
    
    # pick from first half
    k <- ceiling(length(dates) / 2)
    pool <- dates[1:k]
    
    # deterministic “random”: seed depends on global seed + EMPI
    # (stable even if file order changes)
    empi_int <- sum(utf8ToInt(empi_chr))
    set.seed(cfg$control_index_seed + empi_int)
    
    sample(pool, 1)
  }
  
  index_dates <- enc_all %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      Index_Date = sample_index_date(Admit_Date, dplyr::first(.data[[id_col]])),
      .groups = "drop"
    )
  
  msg("Built Index_Date for control IDs: ", nrow(index_dates))
  
  # Preview sample (like your code)
  if (isTRUE(verbose)) {
    msg("Preview of Index_Date assignment (random sample):")
    print(index_dates %>% dplyr::slice_sample(n = min(sample_preview_n, nrow(index_dates))))
  }
  
  # ---- merge index date back to encounters and compute metrics ----
  merged_df <- enc_all %>%
    dplyr::inner_join(index_dates, by = setNames(id_col, id_col)) %>%
    dplyr::mutate(
      Date_diff = as.numeric(difftime(Admit_Date, Index_Date, units = "days")),
      Dates_before_index = ifelse(Date_diff < 0, 1L, 0L),
      Dates_after_index  = ifelse(Date_diff > 0, 1L, 0L)
    )
  
  final_Control_Enc <- merged_df %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      Dates_before_index = sum(Dates_before_index, na.rm = TRUE),
      Dates_after_index  = sum(Dates_after_index, na.rm = TRUE),
      Follow_up_duration = max(Date_diff, na.rm = TRUE),
      Index_Date         = dplyr::first(Index_Date),
      .groups = "drop"
    )
  
  msg("FINAL final_Control_Enc: rows=", nrow(final_Control_Enc),
      " | unique IDs=", dplyr::n_distinct(final_Control_Enc[[id_col]]))
  
  final_Control_Enc
}