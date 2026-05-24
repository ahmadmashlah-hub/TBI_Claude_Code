# R/read_imaging.R
# Reads and classifies _Rdt.txt radiology files (pipe-delimited, 14 columns with header).
# Filters to a provided set of EPIC_PMRNs and classifies each row
# as CT or MRI based on HDNK body region + Mode column.

read_and_classify_imaging <- function(cfg, valid_ids, verbose = TRUE) {

  msg <- function(...) if (isTRUE(verbose)) message(...)

  id_col <- cfg$id_col

  # ---- Locate all _Rdt.txt files ----
  rdt_files <- list.files(
    path        = cfg$imaging_data_dir,
    pattern     = "_Rdt\\.txt$",
    full.names  = TRUE,
    ignore.case = TRUE
  )

  if (length(rdt_files) == 0) stop("No _Rdt.txt files found in: ", cfg$imaging_data_dir)
  msg("Found ", length(rdt_files), " _Rdt.txt file(s) to read.")

  # ---- Column names (14 columns, header present) ----
  # Col 01: EMPI
  # Col 02: EPIC_PMRN      <- patient ID
  # Col 03: MRN_Type
  # Col 04: MRN
  # Col 05: Date           <- imaging date
  # Col 06: Mode           <- modality (CT, MRI, etc.)
  # Col 07: Group          <- body region (HDNK, etc.)
  # Col 08: Test_Code
  # Col 09: Test_Description
  # Col 10: Accession_Number
  # Col 11: Provider
  # Col 12: Clinic
  # Col 13: Hospital
  # Col 14: Inpatient_Outpatient

  col_names <- c(
    "EMPI", "EPIC_PMRN", "MRN_Type", "MRN",
    "Imaging_Date", "Mode", "Body_Region",
    "Test_Code", "Test_Description", "Accession_Number",
    "Provider", "Clinic", "Hospital", "Inpatient_Outpatient"
  )

  # ---- Read + row-bind all files ----
  raw_list <- lapply(rdt_files, function(f) {
    msg("  Reading: ", basename(f))
    df <- utils::read.table(
      f,
      sep              = "|",
      header           = TRUE,       # first row is header
      quote            = "\"",
      fill             = TRUE,
      stringsAsFactors = FALSE,
      comment.char     = ""
    )
    # Rename columns to our standardised names regardless of source header
    if (ncol(df) == length(col_names)) {
      colnames(df) <- col_names
    } else {
      warning("Unexpected column count (", ncol(df), ") in file: ", basename(f),
              " — expected ", length(col_names), ". Skipping.")
      return(NULL)
    }
    df
  })

  # Drop any NULL (skipped) files
  raw_list <- Filter(Negate(is.null), raw_list)
  if (length(raw_list) == 0) stop("No files were read successfully.")

  raw <- dplyr::bind_rows(raw_list)
  msg("Total raw imaging rows across all files: ", nrow(raw))

  # ---- Basic cleaning ----
  raw <- raw %>%
    dplyr::mutate(
      !!id_col      := as.character(trimws(.data[["EPIC_PMRN"]])),
      Imaging_Date   = as.Date(trimws(Imaging_Date), format = "%m/%d/%Y"),
      Mode           = trimws(toupper(Mode)),
      Body_Region    = trimws(toupper(Body_Region)),
      Test_Description = trimws(toupper(Test_Description))
    )

  # ---- Filter to valid cohort IDs ----
  valid_ids_chr <- as.character(valid_ids)
  before_ids <- dplyr::n_distinct(raw[[id_col]])
  raw <- raw %>% dplyr::filter(.data[[id_col]] %in% valid_ids_chr)
  msg("After cohort ID filter: ", nrow(raw), " rows | ",
      dplyr::n_distinct(raw[[id_col]]), " unique IDs (from ",
      before_ids, " in file)")

  # ---- Filter to HDNK + CT or MRI ----
  imaging <- raw %>%
    dplyr::filter(
      Body_Region == "HDNK",
      Mode %in% c("CT", "MRI")
    )
  msg("After HDNK + CT/MRI filter: ", nrow(imaging), " rows | ",
      dplyr::n_distinct(imaging[[id_col]]), " unique IDs")

  # ---- Classify as CT or MRI ----
  imaging <- imaging %>%
    dplyr::mutate(
      Imaging_Type = dplyr::case_when(
        Mode == "CT"  ~ "CT",
        Mode == "MRI" ~ "MRI",
        TRUE          ~ "Other"
      )
    )

  # ---- Remove exact duplicate rows ----
  before_dedup <- nrow(imaging)
  imaging <- imaging %>%
    dplyr::distinct(.data[[id_col]], Imaging_Date, Imaging_Type, .keep_all = TRUE)
  msg("After deduplication: ", nrow(imaging), " rows (removed ",
      before_dedup - nrow(imaging), " duplicates)")

  imaging

}
