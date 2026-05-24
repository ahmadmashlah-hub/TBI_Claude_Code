read_pipe_table_select <- function(file_path, select_cols) {
  df <- read.table(
    file_path,
    header = TRUE,
    sep = "|",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE
  )
  
  df %>% dplyr::select(dplyr::all_of(select_cols))
}

list_enc_files <- function(enc_data_dir) {
  all_files <- list.files(enc_data_dir, full.names = TRUE)
  all_files[grepl("Enc", all_files)]
}

build_encounter_summary <- function(enc_data_dir,
                                    Index_Dates,
                                    id_col,
                                    enc_date_col,
                                    enc_select_cols,
                                    verbose = TRUE,
                                    show_examples = 10) {
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  msg("---- build_encounter_summary() diagnostics ----")
  msg("enc_data_dir: ", enc_data_dir)
  msg("id_col='", id_col, "' | enc_date_col='", enc_date_col, "'")
  
  # ---- list encounter files ----
  all_files <- list.files(enc_data_dir, full.names = TRUE)
  enc_files <- all_files[grepl("Enc", all_files) & grepl("Exp", all_files)]
  n_files <- length(enc_files)
  
  msg("Encounter files found: ", n_files)
  if (n_files == 0) stop("No encounter files matched pattern 'Enc' in enc_data_dir.")
  
  # ---- confirm Index_Dates has required cols ----
  if (!all(c(id_col, "Index_Date") %in% names(Index_Dates))) {
    stop("Index_Dates must contain columns: ", id_col, " and Index_Date")
  }
  
  # Force Index_Dates ID to character
  Index_Dates <- Index_Dates %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
  
  msg("Index_Dates ID type after coercion: ", class(Index_Dates[[id_col]])[1])
  msg("Index_Dates rows: ", nrow(Index_Dates),
      " | unique IDs: ", dplyr::n_distinct(Index_Dates[[id_col]]))
  
  # ---- read and bind all encounter rows ----
  read_one_enc <- function(file_path, i) {
    
    msg(sprintf("Processing encounter file %d / %d: %s",
                i, n_files, basename(file_path)))
    
    df <- read.table(
      file_path,
      header = TRUE,
      sep = "|",
      quote = "",
      fill = TRUE,
      stringsAsFactors = FALSE
    )
    
    # select columns (will error loudly if missing)
    df <- df %>% dplyr::select(dplyr::all_of(enc_select_cols))
    
    # Coerce ID + date to character immediately
    df <- df %>%
      dplyr::mutate(
        !!id_col := as.character(.data[[id_col]]),
        !!enc_date_col := as.character(.data[[enc_date_col]])
      )
    
    msg(sprintf("  → rows read: %d | unique IDs: %d",
                nrow(df), dplyr::n_distinct(df[[id_col]])))
    
    df
  }
  
  enc_list <- lapply(seq_along(enc_files), function(i) {
    read_one_enc(enc_files[[i]], i)
  })
  
  enc_all <- dplyr::bind_rows(enc_list)
  
  msg("After reading ALL encounter files: rows=", nrow(enc_all),
      " | unique IDs=", dplyr::n_distinct(enc_all[[id_col]]))
  
  # ---- parse dates ----
  enc_all <- enc_all %>%
    dplyr::mutate(
      !!enc_date_col := lubridate::mdy(.data[[enc_date_col]], quiet = TRUE)
    )
  
  na_enc_dates <- sum(is.na(enc_all[[enc_date_col]]))
  msg("Encounter date parse: NA dates=", na_enc_dates, " / ", nrow(enc_all))
  
  enc_all <- enc_all %>% dplyr::filter(!is.na(.data[[enc_date_col]]))
  msg("After dropping NA encounter dates: rows=", nrow(enc_all))
  
  # ---- deduplicate encounters ----
  enc_all <- enc_all %>%
    dplyr::distinct(.data[[id_col]], .data[[enc_date_col]], .keep_all = TRUE)
  
  msg("After distinct(ID, date): rows=", nrow(enc_all),
      " | unique IDs=", dplyr::n_distinct(enc_all[[id_col]]))
  
  # ---- join ----
  msg("Joining encounters to Index_Dates by '", id_col, "'")
  
  merged_df <- enc_all %>%
    dplyr::inner_join(Index_Dates, by = setNames(id_col, id_col))
  
  msg("After join: rows=", nrow(merged_df),
      " | unique IDs=", dplyr::n_distinct(merged_df[[id_col]]))
  
  if (nrow(merged_df) == 0) {
    msg("WARNING: join produced 0 rows.")
    msg("Example Index_Dates IDs: ",
        paste(head(Index_Dates[[id_col]], show_examples), collapse = " | "))
    msg("Example Enc IDs: ",
        paste(head(enc_all[[id_col]], show_examples), collapse = " | "))
  }
  
  # ---- compute diffs + summarize ----
  merged_df <- merged_df %>%
    dplyr::mutate(
      Date_diff = as.numeric(difftime(.data[[enc_date_col]], Index_Date, units = "days")),
      Dates_before_index = ifelse(Date_diff < 0, 1L, 0L),
      Dates_after_index  = ifelse(Date_diff > 0, 1L, 0L)
    )
  
  final_Exp_Enc <- merged_df %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      Dates_before_index = sum(Dates_before_index, na.rm = TRUE),
      Dates_after_index  = sum(Dates_after_index, na.rm = TRUE),
      Follow_up_duration = max(Date_diff, na.rm = TRUE),
      .groups = "drop"
    )
  
  missing_ids <- setdiff(Index_Dates[[id_col]], final_Exp_Enc[[id_col]])
  msg("FINAL: encounter summary rows=", nrow(final_Exp_Enc),
      " | unique IDs=", dplyr::n_distinct(final_Exp_Enc[[id_col]]))
  msg("Patients in Index_Dates missing encounters: ", length(missing_ids))
  
  final_Exp_Enc
}