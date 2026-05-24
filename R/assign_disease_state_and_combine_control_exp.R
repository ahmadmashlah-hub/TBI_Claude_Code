# R/assign_disease_state_and_combine_control_exp.R

assign_tbi_status_and_build_cohort <- function(cfg,
                                               exp_dia, exp_dem_indexed,
                                               control_dia, control_dem_indexed,
                                               verbose = TRUE) {
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  id_col <- cfg$id_col
  disease_col <- cfg$disease_col
  dx_list <- cfg$baseline_exclusion_dx
  window_days <- cfg$baseline_exclusion_days

  stopifnot(!is.null(dx_list), !is.null(window_days))  

  tbi_dx <- cfg$tbi_dx_values
  sev_types <- cfg$severity_code_types
  sev_rank <- cfg$severity_rank
  
  # -----------------------------
  # Helper: coerce ID + date cols
  # -----------------------------
  exp_dia <- exp_dia %>% dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
  exp_dem_indexed <- exp_dem_indexed %>% dplyr::mutate(!!id_col := as.character(.data[[id_col]]))

  if (isTRUE(cfg$include_controls)) {
    control_dia <- control_dia %>% dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
    control_dem_indexed <- control_dem_indexed %>% dplyr::mutate(!!id_col := as.character(.data[[id_col]]))
  }

  # ============================================================
  # A) CONTROLS: exclude anyone with ANY TBI dx anywhere
  # ============================================================
  if (isTRUE(cfg$include_controls)) {
    msg("---- Controls: excluding any TBI diagnoses ----")

    control_ids_with_tbi <- control_dia %>%
      dplyr::filter(Simplified_diagnosis %in% tbi_dx) %>%
      dplyr::distinct(.data[[id_col]]) %>%
      dplyr::pull(.data[[id_col]])

    msg("Control IDs with any TBI dx (excluded): ", length(control_ids_with_tbi))

    True_Controls_Dia <- control_dia %>%
      dplyr::filter(!(.data[[id_col]] %in% control_ids_with_tbi))

    true_control_ids <- True_Controls_Dia %>%
      dplyr::distinct(.data[[id_col]]) %>%
      dplyr::pull(.data[[id_col]])

    msg("True control IDs remaining: ", length(true_control_ids))

    Controls_TBI_Status <- tibble::tibble(
      !!id_col := true_control_ids,
      !!disease_col := "None"
    )

    Dem_Control_TBI_Status <- control_dem_indexed %>%
      dplyr::inner_join(Controls_TBI_Status, by = setNames(id_col, id_col))

    msg("Controls (Dem + Index + Enc) after TBI exclusion: ",
        dplyr::n_distinct(Dem_Control_TBI_Status[[id_col]]))
  } else {
    msg("cfg$include_controls = FALSE → skipping control TBI exclusion.")
    True_Controls_Dia      <- NULL
    Dem_Control_TBI_Status <- NULL
  }

  # ============================================================
  # B) EXPOSURE: assign TBI_Status as highest severity per patient
  # ============================================================
  msg("---- Exposure: assigning severity-based TBI_Status ----")
  
  # Filter down to TBI dx rows used for severity classification
  TBI_Status <- exp_dia %>%
    dplyr::filter(Code_Type %in% sev_types) %>%
    dplyr::filter(Simplified_diagnosis %in% tbi_dx) %>%
    dplyr::mutate(.sev = match(Simplified_diagnosis, sev_rank)) %>%
    dplyr::filter(!is.na(.sev)) %>%
    dplyr::arrange(.data[[id_col]], .sev) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      !!disease_col := Simplified_diagnosis
    )
  
  msg("Exposure IDs with assigned TBI_Status: ",
      dplyr::n_distinct(TBI_Status[[id_col]]))
  
  # Merge Exp Dem-indexed with TBI_Status (keeps your pipeline intent)
  Dem_TBI <- exp_dem_indexed %>%
    dplyr::inner_join(TBI_Status, by = setNames(id_col, id_col))
  
  msg("Exposure (Dem + Index + Enc) with TBI_Status: ",
      dplyr::n_distinct(Dem_TBI[[id_col]]))
  
  # ============================================================
  # C) DIAGNOSES datasets: remove non-penetrating TBI dx from EXP
  #    and combine indexed diagnoses across groups
  # ============================================================
  msg("---- Building combined indexed diagnoses ----")
  
  # Remove TBI dx (except Penetrating) from exposure diagnoses
  dx_to_remove_from_exp <- setdiff(tbi_dx, "Penetrating")
  
  Exp_Dia_clean <- exp_dia %>%
    dplyr::filter(!(Simplified_diagnosis %in% dx_to_remove_from_exp))
  
  if (!("Index_Date" %in% names(exp_dem_indexed))) stop("exp_dem_indexed must contain Index_Date")

  Exp_Dia_Indexed <- Exp_Dia_clean %>%
    dplyr::inner_join(
      exp_dem_indexed %>% dplyr::select(dplyr::all_of(c(id_col, "Index_Date"))),
      by = setNames(id_col, id_col)
    )

  if (isTRUE(cfg$include_controls)) {
    if (!("Index_Date" %in% names(control_dem_indexed))) stop("control_dem_indexed must contain Index_Date")

    True_Controls_Dia_Indexed <- True_Controls_Dia %>%
      dplyr::inner_join(
        control_dem_indexed %>% dplyr::select(dplyr::all_of(c(id_col, "Index_Date"))),
        by = setNames(id_col, id_col)
      )

    Indexed_Diagnoses_Combined <- dplyr::bind_rows(
      Exp_Dia_Indexed,
      True_Controls_Dia_Indexed
    )
  } else {
    msg("cfg$include_controls = FALSE → indexed diagnoses contain exposure only.")
    Indexed_Diagnoses_Combined <- Exp_Dia_Indexed
  }

  msg("Combined indexed diagnoses unique IDs: ",
      dplyr::n_distinct(Indexed_Diagnoses_Combined[[id_col]]))

  # ---- Add Date_diff + baseline window flags ----
  stopifnot(id_col %in% names(Indexed_Diagnoses_Combined))
  stopifnot(all(c("Date", "Index_Date", "Simplified_diagnosis") %in% names(Indexed_Diagnoses_Combined)))

  Indexed_Diagnoses_Combined <- Indexed_Diagnoses_Combined %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      Date = as.Date(Date),
      Index_Date = as.Date(Index_Date),
      Date_diff = as.numeric(difftime(Date, Index_Date, units = "days")),
      WithinBaselineWindow = dplyr::if_else(Date_diff < window_days, 1L, 0L),
      ToBeExcluded = dplyr::if_else(
        Simplified_diagnosis %in% dx_list & WithinBaselineWindow == 1L,
        1L, 0L
      )
    )

  msg("Combined indexed diagnoses unique IDs: ",
      dplyr::n_distinct(Indexed_Diagnoses_Combined[[id_col]]))
  msg("Rows flagged for baseline exclusion (ToBeExcluded==1): ",
      sum(Indexed_Diagnoses_Combined$ToBeExcluded == 1L, na.rm = TRUE))
  
  # ============================================================
  # D) Combine Dem cohorts (Exp + Controls)
  # ============================================================
  msg("---- Building combined demographic cohort ----")
  
  if (isTRUE(cfg$include_controls)) {
    # Ensure same columns before bind
    common_cols <- intersect(names(Dem_TBI), names(Dem_Control_TBI_Status))
    Dem_TBI_out <- Dem_TBI %>% dplyr::select(dplyr::all_of(common_cols))
    Dem_Control_TBI_Status <- Dem_Control_TBI_Status %>% dplyr::select(dplyr::all_of(common_cols))
    Total_Dem <- dplyr::bind_rows(Dem_TBI_out, Dem_Control_TBI_Status)
  } else {
    msg("cfg$include_controls = FALSE → Total_Dem contains exposure group only.")
    Total_Dem <- Dem_TBI
  }
  
  msg("Total cohort unique IDs: ", dplyr::n_distinct(Total_Dem[[id_col]]))
  msg("Cohort breakdown (", disease_col, "):")
  print(Total_Dem %>% dplyr::count(.data[[disease_col]]))
  
  # Return key outputs
  list(
    TBI_Status = TBI_Status,
    True_Controls_Dia = True_Controls_Dia,
    Dem_TBI = Dem_TBI,
    Dem_Control_TBI_Status = Dem_Control_TBI_Status,
    Indexed_Diagnoses_Combined = Indexed_Diagnoses_Combined,
    Total_Dem = Total_Dem
  )
}