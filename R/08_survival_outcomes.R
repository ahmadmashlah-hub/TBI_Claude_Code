# scripts/08_survival_outcomes.R
# KM + Cox for each dx in cfg$outcome_dx_values
# Outputs include cfg$project_slug in filenames.

suppressPackageStartupMessages({
  # relies on packages already loaded by R/00_packages.R
})

# -----------------------------
# Helpers
# -----------------------------
.msg <- function(verbose, ...) if (isTRUE(verbose)) message(...)

safe_name <- function(x) {
  x <- as.character(x)
  x <- gsub("[/\\\\]+", "_", x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

out_path <- function(cfg, subdir, stem, ext) {
  stopifnot(!is.null(cfg$output_dir), !is.null(cfg$project_slug))
  dir <- file.path(cfg$output_dir, "survival", subdir)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.path(dir, paste0(cfg$project_slug, "__", stem, ".", ext))
}

# -----------------------------
# Build time-to-event dataset (1 row per ID)
# -----------------------------
build_time_to_event_df <- function(outcome_dx,
                                   analysis_df,
                                   Indexed_Diagnoses_Combined,
                                   cfg) {
  id_col    <- cfg$id_col
  group_col <- cfg$group_col
  
  stopifnot(id_col %in% names(analysis_df))
  stopifnot(group_col %in% names(analysis_df))
  stopifnot(id_col %in% names(Indexed_Diagnoses_Combined))
  stopifnot(all(c("Simplified_diagnosis", "Date_diff") %in% names(Indexed_Diagnoses_Combined)))
  stopifnot(cfg$followup_col %in% names(analysis_df))
  
  keep_cols <- unique(c(id_col, group_col, cfg$followup_col, cfg$cox_covariates))
  missing_cov <- setdiff(keep_cols, names(analysis_df))
  if (length(missing_cov) > 0) {
    stop("analysis_df missing columns required for survival: ",
         paste(missing_cov, collapse = ", "))
  }
  
  cohort <- analysis_df %>%
    dplyr::select(dplyr::all_of(keep_cols)) %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      !!group_col := as.character(.data[[group_col]])
    )
  
  # Set reference level
  cohort[[group_col]] <- factor(cohort[[group_col]])
  if (!is.null(cfg$group_ref_level) && cfg$group_ref_level %in% levels(cohort[[group_col]])) {
    cohort[[group_col]] <- stats::relevel(cohort[[group_col]], ref = cfg$group_ref_level)
  }
  
  dx <- Indexed_Diagnoses_Combined %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      Date_diff = as.numeric(.data[["Date_diff"]])
    ) %>%
    dplyr::filter(.data[["Simplified_diagnosis"]] == outcome_dx)
  
  # Earliest event per ID (days)
  events <- dx %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(event_days = min(Date_diff, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(event = 1L)
  
  df <- cohort %>%
    dplyr::left_join(events, by = setNames(id_col, id_col))
  
  # Follow-up days should be numeric
  df[[cfg$followup_col]] <- as.numeric(df[[cfg$followup_col]])
  
  # Time and censoring
  df <- df %>%
    dplyr::mutate(
      event = dplyr::if_else(is.na(.data[["event"]]), 0L, .data[["event"]]),
      time_days = dplyr::if_else(
        .data[["event"]] == 1L,
        .data[["event_days"]],
        .data[[cfg$followup_col]]
      ),
      time_days = dplyr::if_else(is.na(time_days) | time_days < 0, .data[[cfg$followup_col]], time_days),
      time_years = time_days / 365.25
    )
  
  df
}

# -----------------------------
# KM plot via survminer (with the critical fix)
# -----------------------------
plot_km_survminer <- function(df,
                              time_col,
                              event_col,
                              group_col,
                              outcome_label,
                              cfg) {
  stopifnot(all(c(time_col, event_col, group_col) %in% names(df)))
  
  # survminer can be picky about group labels -> keep simple
  df$group_plot <- as.character(df[[group_col]])
  df$group_plot[is.na(df$group_plot)] <- "NA"
  
  # If you still have Moderate/Severe anywhere, normalize it
  df$group_plot <- dplyr::case_when(
    df$group_plot == "Moderate/Severe" ~ "ModerateSevere",
    TRUE ~ df$group_plot
  )
  df$group_plot <- factor(df$group_plot)
  
  if (!is.null(cfg$group_ref_level) && cfg$group_ref_level %in% levels(df$group_plot)) {
    df$group_plot <- stats::relevel(df$group_plot, ref = cfg$group_ref_level)
  }
  
  surv_fml <- stats::as.formula(
    paste0("survival::Surv(", time_col, ", ", event_col, ") ~ group_plot")
  )
  
  km_fit <- survival::survfit(surv_fml, data = df)
  
  # ✅ Critical survminer fix:
  km_fit$call$data <- df
  km_fit$call$formula <- surv_fml
  
  xlim <- NULL
  if (!is.null(cfg$km_xlim_mode) && identical(cfg$km_xlim_mode, "fixed")) {
    xlim <- cfg$km_xlim_years
  }
  
  ylim <- NULL
  
  km_ylim_mode <- if (is.null(cfg$km_ylim_mode)) "default" else cfg$km_ylim_mode
  
  if (identical(km_ylim_mode, "fixed")) {
    ylim <- cfg$km_ylim
    
  } else if (identical(km_ylim_mode, "auto")) {
    # Auto: set max based on fitted event curve (fun="event" => 1 - S(t))
    # Use survfit summary so we don't depend on plotting internals.
    sf <- summary(km_fit)
    # sf$surv is survival probability; convert to cumulative event prob
    event_prob <- 1 - sf$surv
    event_prob <- event_prob[is.finite(event_prob)]
    if (length(event_prob) > 0) {
      ymax <- max(event_prob, na.rm = TRUE)
      # add a small headroom; clamp to [0,1]
      ymax <- min(1, ymax * 1.05 + 0.001)
      ylim <- c(0, ymax)
    } else {
      ylim <- NULL
    }
    
  } else if (identical(km_ylim_mode, "default")) {
    # Default: let survminer/ggplot decide
    ylim <- NULL
    
  } else {
    stop("Unknown cfg$km_ylim_mode='", km_ylim_mode,
         "'. Use one of: 'fixed', 'auto', 'default'.")
  }
  
  survminer::ggsurvplot(
    fit        = km_fit,
    data       = df,
    fun        = "event",
    censor     = FALSE,
    risk.table = isTRUE(cfg$km_show_risktable),
    break.time.by = 5,   # years
    conf.int   = isTRUE(cfg$km_conf_int),
    pval       = isTRUE(cfg$km_show_pval),
    palette    = "Dark2",
    ggtheme    = ggplot2::theme_minimal(),
    legend.title = group_col,
    xlab = "Time (years)",
    ylab = paste("Cumulative probability of", outcome_label),
    title = paste("Inverse Kaplan–Meier:", outcome_label),
    xlim = xlim,
    ylim = ylim
  )
}

# -----------------------------
# One-outcome analysis (KM + Cox)
# -----------------------------
analyze_one_outcome <- function(outcome_dx,
                                analysis_df,
                                Indexed_Diagnoses_Combined,
                                cfg,
                                verbose = TRUE) {
  id_col    <- cfg$id_col
  group_col <- cfg$group_col
  covars    <- cfg$cox_covariates
  
  time_df <- build_time_to_event_df(
    outcome_dx = outcome_dx,
    analysis_df = analysis_df,
    Indexed_Diagnoses_Combined = Indexed_Diagnoses_Combined,
    cfg = cfg
  )
  
  cohort_ids <- dplyr::n_distinct(time_df[[id_col]])
  events <- sum(time_df$event == 1, na.rm = TRUE)
  .msg(verbose, "Outcome: ", outcome_dx, " | IDs=", cohort_ids, " | events=", events)
  
  # --- KM PDF ---
  km_ok <- TRUE
  km_err <- NA_character_
  
  km_pdf <- out_path(cfg, "km", paste0(cfg$project_slug, "__KM_", safe_name(outcome_dx)), "pdf")
  
  tryCatch({
    dir.create(dirname(km_pdf), showWarnings = FALSE, recursive = TRUE)
    grDevices::pdf(
      km_pdf,
      width = 8,
      height = if (isTRUE(cfg$km_show_risktable)) 10 else 6,
      useDingbats = FALSE,
      compress = FALSE
    )
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    
    km_plot <- plot_km_survminer(
      df = time_df,
      time_col = "time_years",
      event_col = "event",
      group_col = group_col,
      outcome_label = outcome_dx,
      cfg = cfg
    )
    
    print(km_plot)
    .msg(verbose, "Saved KM: ", km_pdf)
  }, error = function(e) {
    km_ok <<- FALSE
    km_err <<- conditionMessage(e)
    .msg(verbose, "KM failed for ", outcome_dx, ": ", km_err)
  })
  
  # --- Cox model ---
  rhs <- c(group_col, covars)
  cox_fml <- stats::as.formula(
    paste0("survival::Surv(time_years, event) ~ ", paste(rhs, collapse = " + "))
  )
  
  time_df[[group_col]] <- factor(as.character(time_df[[group_col]]))
  if (!is.null(cfg$group_ref_level) && cfg$group_ref_level %in% levels(time_df[[group_col]])) {
    time_df[[group_col]] <- stats::relevel(time_df[[group_col]], ref = cfg$group_ref_level)
  }
  
  cox_fit <- survival::coxph(cox_fml, data = time_df)
  cox_sum <- summary(cox_fit)
  
  coef_tbl <- as.data.frame(cox_sum$coefficients)
  conf_tbl <- as.data.frame(cox_sum$conf.int)
  
  group_rows <- grep(paste0("^", group_col), rownames(coef_tbl), value = TRUE)
  if (length(group_rows) == 0) {
    stop("No group contrasts found in Cox model. group_col=", group_col)
  }
  
  out <- dplyr::bind_rows(lapply(group_rows, function(rn) {
    hr <- conf_tbl[rn, "exp(coef)"]
    lo <- conf_tbl[rn, "lower .95"]
    hi <- conf_tbl[rn, "upper .95"]
    pv <- coef_tbl[rn, "Pr(>|z|)"]
    
    data.frame(
      Diagnosis = outcome_dx,
      Contrast  = rn,
      HR        = as.numeric(hr),
      CI_Lower  = as.numeric(lo),
      CI_Upper  = as.numeric(hi),
      HR_CI     = sprintf("%.2f (%.2f–%.2f)", hr, lo, hi),
      P_Value   = ifelse(is.na(pv), NA_character_,
                         ifelse(pv < 0.001, "<0.001", sprintf("%.3f", pv))),
      Events    = events,
      KM_OK     = km_ok,
      KM_Error  = ifelse(km_ok, NA_character_, km_err),
      stringsAsFactors = FALSE
    )
  }))
  
  out
}

# -----------------------------
# Forest plot
# -----------------------------
save_forestplot <- function(results_df, cfg, verbose = TRUE) {
  if (!requireNamespace("forestplot", quietly = TRUE)) {
    stop("Package 'forestplot' is required. Install it once, then rerun.")
  }
  
  fp <- results_df %>%
    dplyr::arrange(Diagnosis, Contrast) %>%
    dplyr::mutate(Label = paste0(Diagnosis, " — ", Contrast))
  
  pdf_path <- out_path(cfg, "forest", paste0(cfg$project_slug, "__forestplot_hr"), "pdf")
  dir.create(dirname(pdf_path), showWarnings = FALSE, recursive = TRUE)
  
  grDevices::pdf(pdf_path, width = 10, height = max(6, 0.30 * nrow(fp) + 2),
                 useDingbats = FALSE, compress = FALSE)
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
  
  forestplot::forestplot(
    labeltext = fp$Label,
    mean  = fp$HR,
    lower = fp$CI_Lower,
    upper = fp$CI_Upper,
    xlog = TRUE,
    zero = 1,
    xlab = "Hazard Ratio"
  )
  
  .msg(verbose, "Saved forest plot: ", pdf_path)
  invisible(pdf_path)
}

# -----------------------------
# Main runner
# -----------------------------
run_survival_bundle <- function(analysis_df,
                                Indexed_Diagnoses_Combined,
                                cfg,
                                verbose = TRUE) {
  stopifnot(!is.null(cfg$project_slug))
  stopifnot(!is.null(cfg$group_col))
  stopifnot(!is.null(cfg$outcome_dx_values))
  stopifnot(!is.null(cfg$cox_covariates))
  stopifnot(!is.null(cfg$followup_col))
  
  cfg$outcome_dx_values <- as.character(cfg$outcome_dx_values)
  
  all_res <- lapply(cfg$outcome_dx_values, function(dx) {
    analyze_one_outcome(
      outcome_dx = dx,
      analysis_df = analysis_df,
      Indexed_Diagnoses_Combined = Indexed_Diagnoses_Combined,
      cfg = cfg,
      verbose = verbose
    )
  })
  
  results_df <- dplyr::bind_rows(all_res)
  
  csv_path <- out_path(cfg, "results", paste0(cfg$project_slug, "__cox_results"), "csv")
  dir.create(dirname(csv_path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(results_df, csv_path, row.names = FALSE)
  .msg(verbose, "Saved results CSV: ", csv_path)
  
  save_forestplot(results_df, cfg, verbose = verbose)
  
  results_df
}
