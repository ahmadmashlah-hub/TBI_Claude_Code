# R/02_read_exposure_inputs.R

read_pipe_table <- function(file_path) {
  read.table(
    file_path,
    header = TRUE,
    sep = "|",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE
  )
}

read_icd_excel <- function(excel_file) {
  df <- readxl::read_excel(excel_file)
  df[] <- lapply(df, as.character)
  
  ICDCodesCombined <- lapply(seq_len(ncol(df)), function(i) {
    col <- df[[i]]
    col <- col[!is.na(col) & col != ""]
    col
  })
  names(ICDCodesCombined) <- colnames(df)
  
  # Manual fixes (force character)
  ICDCodesCombined[["HLD"]][[2]]                 <- "272.1"
  ICDCodesCombined[["HLD"]][[5]]                 <- "272.4"
  ICDCodesCombined[["Insomnia"]][[4]]            <- "307.4"
  ICDCodesCombined[["AUD"]][[2]]                 <- "291.1"
  ICDCodesCombined[["AUD"]][[5]]                 <- "291.4"
  ICDCodesCombined[["AUD"]][[10]]                <- "303.9"
  ICDCodesCombined[["AUD"]][[8]]                 <- "291.9"
  ICDCodesCombined[["AUD"]][[14]]                <- "535.3"
  ICDCodesCombined[["AUD"]][[17]]                <- "571.2"
  ICDCodesCombined[["AUD"]][[18]]                <- "571.3"
  ICDCodesCombined[["OtherAbuse"]][[1]]          <- "304.6"
  ICDCodesCombined[["OtherAbuse"]][[3]]          <- "304.9"
  ICDCodesCombined[["OtherAbuse"]][[4]]          <- "305.9"
  ICDCodesCombined[["OtherAbuse"]][[5]]          <- "648.3"
  ICDCodesCombined[["DrugInducedMental"]][[9]]   <- "292.85"
  ICDCodesCombined[["DrugInducedMental"]][[8]]   <- "292.84"
  ICDCodesCombined[["DrugInducedMental"]][[11]]  <- "292.9"
  ICDCodesCombined[["Sedative"]][[1]]            <- "304.1"
  ICDCodesCombined[["Sedative"]][[2]]            <- "305.4"
  ICDCodesCombined[["Amphetamine"]][[1]]         <- "304.4"
  
  ICDCodesCombined
}

# -------------------------
# Generalized file lister
# -------------------------
list_group_files <- function(data_dir,
                             type = c("Dia", "Dem", "Enc"),
                             group_tag = c("Exp", "Control")) {
  type <- match.arg(type)
  group_tag <- match.arg(group_tag)
  all_files <- list.files(data_dir, full.names = TRUE)
  all_files[grepl(type, all_files) & grepl(group_tag, all_files)]
}

# Backwards-compatible
list_exp_files <- function(data_dir, type = c("Dia", "Dem")) {
  type <- match.arg(type)
  list_group_files(data_dir, type, group_tag = "Exp")
}

# Helper: select only columns that exist (prevents Zip_code errors)
safe_select <- function(df, cols) {
  cols_present <- intersect(cols, names(df))
  df %>% dplyr::select(dplyr::all_of(cols_present))
}

# -------------------------
# Shared diagnosis reader
# -------------------------
read_diagnoses_group <- function(cfg,
                                 Dia_select_columns,
                                 ICDCodesCombined,
                                 group_tag = c("Exp", "Control")) {
  
  group_tag <- match.arg(group_tag)
  id_col <- cfg$id_col
  
  dia_files <- list_group_files(cfg$data_dir, "Dia", group_tag)
  if (length(dia_files) == 0) stop("No Dia files found for group_tag='", group_tag, "' in ", cfg$data_dir)
  
  read_one_dia <- function(file_path) {
    df <- read_pipe_table(file_path) %>%
      safe_select(Dia_select_columns) %>%
      dplyr::mutate(
        Date = lubridate::mdy(Date, quiet = TRUE),
        Code = as.character(Code),
        Code_Type = as.character(Code_Type),
        !!id_col := as.character(.data[[id_col]])
      ) %>%
      dplyr::filter(!is.na(Date)) %>%
      dplyr::arrange(Date) %>%
      dplyr::filter(Code_Type %in% c("ICD9", "ICD10")) %>%
      dplyr::distinct(.data[[id_col]], Code, .keep_all = TRUE) %>%
      dplyr::mutate(Simplified_diagnosis = NA_character_)
    
    for (diagnosis in names(ICDCodesCombined)) {
      pattern <- paste0("^(", paste(ICDCodesCombined[[diagnosis]], collapse = "|"), ")")
      df <- df %>%
        dplyr::mutate(
          Simplified_diagnosis = dplyr::if_else(
            stringr::str_detect(Code, pattern),
            diagnosis,
            Simplified_diagnosis
          )
        )
    }
    
    # ---- Keep only rows with a matched diagnosis ----
    df <- df %>%
      dplyr::filter(!is.na(Simplified_diagnosis))

    # ---- Count distinct dates per EPIC_PMRN / Simplified_diagnosis ----
    # This happens BEFORE deduplication so it captures all unique dates,
    # regardless of whether the exact ICD code differs.
    dx_counts <- df %>%
      dplyr::group_by(.data[[id_col]], Simplified_diagnosis) %>%
      dplyr::summarise(
        incidents_of_Simplified_Diagnosis = dplyr::n_distinct(Date),
        .groups = "drop"
      )

    # ---- Deduplicate: keep earliest date per EPIC_PMRN / Simplified_diagnosis ----
    df <- df %>%
      dplyr::distinct(.data[[id_col]], Simplified_diagnosis, .keep_all = TRUE) %>%
      dplyr::left_join(dx_counts, by = c(id_col, "Simplified_diagnosis"))
    
    message("Finished processing file: ", file_path)
    df
  }
  
  processed <- lapply(dia_files, read_one_dia)
  out <- dplyr::bind_rows(processed)

  # ---- Re-aggregate counts across files ----
  # Each file is processed separately, so a patient may appear in multiple
  # files. We re-sum the counts and re-deduplicate after binding all files.
  out <- out %>%
    dplyr::group_by(.data[[id_col]], Simplified_diagnosis) %>%
    dplyr::mutate(
      incidents_of_Simplified_Diagnosis = sum(incidents_of_Simplified_Diagnosis, na.rm = TRUE)
    ) %>%
    dplyr::arrange(Date) %>%
    dplyr::slice(1) %>%   # keep earliest date row across all files
    dplyr::ungroup()

  message("Number of unique ", id_col, "s in the diagnosis dataset (", group_tag, "): ",
          dplyr::n_distinct(out[[id_col]]))
  
  out
}

# Backwards compatible exposure wrapper
read_diagnoses_exp <- function(data_dir, Dia_select_columns, ICDCodesCombined) {
  stop("Use read_diagnoses_group(cfg, ...) instead. This wrapper is deprecated to avoid mismatched cfg/data_dir.")
}

# New "correct" wrappers using cfg
read_diagnoses_exp_cfg <- function(cfg, Dia_select_columns, ICDCodesCombined) {
  read_diagnoses_group(cfg, Dia_select_columns, ICDCodesCombined, group_tag = "Exp")
}

read_diagnoses_control_cfg <- function(cfg, Dia_select_columns, ICDCodesCombined) {
  read_diagnoses_group(cfg, Dia_select_columns, ICDCodesCombined, group_tag = "Control")
}

# -------------------------
# Shared demographics reader
# -------------------------
read_demographics_group <- function(cfg,
                                    Dem_select_columns,
                                    group_tag = c("Exp", "Control")) {
  
  group_tag <- match.arg(group_tag)
  id_col <- cfg$id_col
  
  dem_files <- list_group_files(cfg$data_dir, "Dem", group_tag)
  if (length(dem_files) == 0) stop("No Dem files found for group_tag='", group_tag, "' in ", cfg$data_dir)
  
  read_one_dem <- function(file_path) {
    df <- read_pipe_table(file_path) %>%
      safe_select(Dem_select_columns) %>%
      dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
    
    # Zip_code sometimes comes in numeric: always coerce if present
    if ("Zip_code" %in% names(df)) {
      df <- df %>% dplyr::mutate(Zip_code = as.character(Zip_code))
    }
    
    message("Finished processing file: ", file_path)
    df
  }
  
  processed <- lapply(dem_files, read_one_dem)
  out <- dplyr::bind_rows(processed)
  
  message("Number of unique ", id_col, "s in the demographic dataset (", group_tag, "): ",
          dplyr::n_distinct(out[[id_col]]))
  
  out
}

# Deprecated old wrappers (to avoid cfg mismatch)
read_demographics_exp <- function(data_dir, Dem_select_columns) {
  stop("Use read_demographics_group(cfg, ...) instead. This wrapper is deprecated.")
}

# Correct cfg-based wrappers
read_demographics_exp_cfg <- function(cfg, Dem_select_columns) {
  read_demographics_group(cfg, Dem_select_columns, group_tag = "Exp")
}

read_demographics_control_cfg <- function(cfg, Dem_select_columns) {
  read_demographics_group(cfg, Dem_select_columns, group_tag = "Control")
}
