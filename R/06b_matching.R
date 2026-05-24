# R/run_matching.R
# Supports:
#   - cfg$matching_mode = "binary"
#   - cfg$matching_mode = "three_way_twostep"
#
# Determinism:
#   - stable sort
#   - set.seed(cfg$match_seed)
#   - m.order = "data"
#   - tie-breaking uses row order

`%||%` <- function(x, y) if (!is.null(x)) x else y

.msg <- function(verbose, ...) if (isTRUE(verbose)) message(...)

run_matching <- function(df, cfg, verbose = TRUE) {
  mode <- cfg$matching_mode %||% "binary"
  stopifnot(mode %in% c("binary", "three_way_twostep"))
  
  if (!isTRUE(cfg$do_matching)) {
    .msg(verbose, "Matching OFF (cfg$do_matching = FALSE). Returning df unchanged.")
    return(list(matched_df = df, match_model = NULL, meta = list(mode = "off")))
  }
  
  if (identical(mode, "binary")) {
    return(run_matching_binary(df, cfg, verbose = verbose))
  } else {
    return(run_matching_three_way_twostep(df, cfg, verbose = verbose))
  }
}

# -----------------------------
# 1) Binary matching (your existing logic)
# -----------------------------
run_matching_binary <- function(df, cfg, verbose = TRUE) {
  
  stopifnot(!is.null(cfg$id_col), !is.null(cfg$group_col), !is.null(cfg$match_covariates))
  id_col    <- cfg$id_col
  group_col <- cfg$group_col
  covars    <- cfg$match_covariates
  
  missing_cov <- setdiff(covars, names(df))
  if (length(missing_cov) > 0) stop("Missing match covariates: ", paste(missing_cov, collapse = ", "))
  
  if (!is.null(cfg$match_seed)) set.seed(cfg$match_seed)
  
  df <- df %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      !!group_col := as.character(.data[[group_col]])
    ) %>%
    dplyr::arrange(.data[[group_col]], .data[[id_col]])
  
  df[[group_col]] <- factor(df[[group_col]])
  if (!is.null(cfg$group_ref_level) && cfg$group_ref_level %in% levels(df[[group_col]])) {
    df[[group_col]] <- stats::relevel(df[[group_col]], ref = cfg$group_ref_level)
  }
  
  fml <- stats::as.formula(paste(group_col, "~", paste(covars, collapse = " + ")))
  
  m <- MatchIt::matchit(
    formula  = fml,
    data     = df,
    method   = cfg$match_method   %||% "nearest",
    distance = cfg$match_distance %||% "glm",
    ratio    = cfg$match_ratio    %||% 1,
    replace  = cfg$match_replace  %||% FALSE,
    m.order  = cfg$match_m_order  %||% "data"
  )
  
  matched_df <- MatchIt::match.data(m)
  
  .msg(verbose, "Binary matched rows: ", nrow(matched_df),
       " | unique IDs: ", dplyr::n_distinct(matched_df[[id_col]]))
  
  list(
    matched_df = matched_df,
    match_model = m,
    meta = list(mode = "binary")
  )
}

# -----------------------------
# Helper: one-vs-one match (anchor vs target)
# Returns a data.frame with:
#   anchor_id, target_id, anchor_row, target_row
# -----------------------------
match_one_vs_one <- function(df,
                             id_col,
                             group_col,
                             anchor_level,
                             target_level,
                             covars,
                             seed,
                             method = "nearest",
                             distance = "glm",
                             ratio = 1,
                             replace = FALSE,
                             m_order = "data") {
  
  if (!is.null(seed)) set.seed(seed)
  
  sub <- df %>%
    dplyr::filter(.data[[group_col]] %in% c(anchor_level, target_level)) %>%
    dplyr::mutate(
      ..grp_bin = dplyr::if_else(.data[[group_col]] == anchor_level, 1L, 0L)
    ) %>%
    dplyr::arrange(.data[[group_col]], .data[[id_col]])
  
  fml <- stats::as.formula(paste("..grp_bin ~", paste(covars, collapse = " + ")))
  
  m <- MatchIt::matchit(
    formula  = fml,
    data     = sub,
    method   = method,
    distance = distance,
    ratio    = ratio,
    replace  = replace,
    m.order  = m_order
  )
  
  # MatchIt stores pairings in match.matrix (rows = treated = ..grp_bin==1)
  mm <- m$match.matrix
  if (is.null(mm) || nrow(mm) == 0) {
    return(list(pairs = dplyr::tibble(anchor_id = character(0), target_id = character(0)),
                match_model = m))
  }
  
  # Convert matrix to pairs (1:1 expected)
  anchor_row_names <- rownames(mm)
  target_row_names <- as.character(mm[, 1])
  
  pairs <- dplyr::tibble(
    anchor_row = anchor_row_names,
    target_row = target_row_names
  ) %>%
    dplyr::filter(!is.na(target_row) & target_row != "") %>%
    dplyr::mutate(
      anchor_id = sub[[id_col]][match(anchor_row, rownames(sub))],
      target_id = sub[[id_col]][match(target_row, rownames(sub))]
    ) %>%
    dplyr::select(anchor_id, target_id, anchor_row, target_row)
  
  list(pairs = pairs, match_model = m)
}

# -----------------------------
# 2) Three-way two-step nearest neighbor matching
# Produces triads:
#   anchor (e.g., ModerateSevere) + 1 Mild + 1 None
# Deterministic + repeatable.
# -----------------------------
run_matching_three_way_twostep <- function(df, cfg, verbose = TRUE) {
  
  stopifnot(!is.null(cfg$id_col), !is.null(cfg$group_col), !is.null(cfg$match_covariates))
  id_col    <- cfg$id_col
  group_col <- cfg$group_col
  covars    <- cfg$match_covariates
  
  # Which 3 levels?
  anchor_level <- cfg$match_three_anchor %||% "ModerateSevere"
  mid_level    <- cfg$match_three_mid    %||% "Mild"
  ref_level    <- cfg$match_three_ref    %||% (cfg$group_ref_level %||% "None")
  
  needed <- c(anchor_level, mid_level, ref_level)
  missing_levels <- setdiff(needed, unique(as.character(df[[group_col]])))
  if (length(missing_levels) > 0) {
    stop("3-way matching missing group level(s): ", paste(missing_levels, collapse = ", "),
         "\nAvailable: ", paste(sort(unique(as.character(df[[group_col]]))), collapse = ", "))
  }
  
  missing_cov <- setdiff(covars, names(df))
  if (length(missing_cov) > 0) stop("Missing match covariates: ", paste(missing_cov, collapse = ", "))
  
  # Stable prep
  df0 <- df %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      !!group_col := as.character(.data[[group_col]])
    ) %>%
    dplyr::arrange(.data[[group_col]], .data[[id_col]])
  
  seed_base <- cfg$match_seed %||% 12345
  
  .msg(verbose, "3-way two-step matching:")
  .msg(verbose, "  anchor=", anchor_level, " | mid=", mid_level, " | ref=", ref_level)
  .msg(verbose, "  covars=", paste(covars, collapse = ", "))
  .msg(verbose, "  seed=", seed_base, " | replace=", cfg$match_replace %||% FALSE)
  
  # Step A: anchor vs ref
  stepA <- match_one_vs_one(
    df = df0,
    id_col = id_col,
    group_col = group_col,
    anchor_level = anchor_level,
    target_level = ref_level,
    covars = covars,
    seed = seed_base + 1,
    method   = cfg$match_method   %||% "nearest",
    distance = cfg$match_distance %||% "glm",
    ratio    = 1,
    replace  = cfg$match_replace  %||% FALSE,
    m_order  = cfg$match_m_order  %||% "data"
  )
  
  pairs_ref <- stepA$pairs
  .msg(verbose, "  Step A pairs (anchor→ref): ", nrow(pairs_ref))
  
  # Step B: anchor vs mid
  stepB <- match_one_vs_one(
    df = df0,
    id_col = id_col,
    group_col = group_col,
    anchor_level = anchor_level,
    target_level = mid_level,
    covars = covars,
    seed = seed_base + 2,
    method   = cfg$match_method   %||% "nearest",
    distance = cfg$match_distance %||% "glm",
    ratio    = 1,
    replace  = cfg$match_replace  %||% FALSE,
    m_order  = cfg$match_m_order  %||% "data"
  )
  
  pairs_mid <- stepB$pairs
  .msg(verbose, "  Step B pairs (anchor→mid): ", nrow(pairs_mid))
  
  # Keep anchors present in BOTH steps -> triads
  triads <- pairs_ref %>%
    dplyr::inner_join(
      pairs_mid,
      by = "anchor_id",
      suffix = c("_ref", "_mid")
    ) %>%
    dplyr::transmute(
      anchor_id = anchor_id,
      ref_id = target_id_ref,
      mid_id = target_id_mid
    )
  
  # Optional: enforce unique ref/mid controls (no reuse) if replace=FALSE,
  # but ties can still create overlaps across steps. We'll drop duplicates deterministically.
  # Keep first occurrence based on sorted anchor_id.
  triads <- triads %>%
    dplyr::arrange(anchor_id) %>%
    dplyr::filter(!duplicated(ref_id)) %>%
    dplyr::filter(!duplicated(mid_id))
  
  .msg(verbose, "  Triads retained after overlap drops: ", nrow(triads))
  
  if (nrow(triads) == 0) {
    stop("3-way matching produced 0 triads. Try allowing replace=TRUE or loosen covariates/ratio.")
  }
  
  # Build matched cohort = all three arms
  keep_ids <- unique(c(triads$anchor_id, triads$ref_id, triads$mid_id))
  
  matched_df <- df0 %>%
    dplyr::filter(.data[[id_col]] %in% keep_ids)
  
  # Add a triad ID (so you can stratify/cluster later if needed)
  triad_map <- dplyr::bind_rows(
    dplyr::tibble(!!id_col := triads$anchor_id, triad_id = triads$anchor_id, triad_role = "anchor"),
    dplyr::tibble(!!id_col := triads$ref_id,    triad_id = triads$anchor_id, triad_role = "ref"),
    dplyr::tibble(!!id_col := triads$mid_id,    triad_id = triads$anchor_id, triad_role = "mid")
  )
  
  matched_df <- matched_df %>%
    dplyr::left_join(triad_map, by = setNames(id_col, id_col))
  
  .msg(verbose,
       "3-way matched cohort rows: ", nrow(matched_df),
       " | unique IDs: ", dplyr::n_distinct(matched_df[[id_col]]),
       " | triads: ", dplyr::n_distinct(matched_df$triad_id))
  
  list(
    matched_df = matched_df,
    match_model = list(step_anchor_ref = stepA$match_model, step_anchor_mid = stepB$match_model),
    meta = list(
      mode = "three_way_twostep",
      anchor_level = anchor_level,
      mid_level = mid_level,
      ref_level = ref_level,
      triads = triads
    )
  )
}