# scripts/03_filtering_encounters.R
# Filtering by encounters (Exposure + Controls)

source("R/filter_by_encounters.R")

# -------------------------
# EXPOSURE
# -------------------------
res_exp <- filter_by_encounters(
  final_Enc   = final_Exp_Enc,
  Index_Dates = Index_Dates,
  final_Dem   = final_Exp_Dem,
  cfg         = cfg,
  verbose     = TRUE
)

final_Exp_Enc_filtered   <- res_exp$final_Exp_Enc_filtered
final_Exp_Enc_filteredx2 <- res_exp$final_Exp_Enc_filteredx2
Index_Dates_Enced        <- res_exp$Index_Dates_Enced
merged_Index_Enc_Dem     <- res_exp$merged_Index_Enc_Dem
merged_Index_Enc_Dem_age <- res_exp$merged_Index_Enc_Dem_age

#### Making sure exposure encounters + Dem have no duplicate IDs ####
id_col <- cfg$id_col

before_n   <- nrow(merged_Index_Enc_Dem_age)
before_ids <- dplyr::n_distinct(merged_Index_Enc_Dem_age[[id_col]])

merged_Index_Enc_Dem_age <- merged_Index_Enc_Dem_age %>%
  dplyr::arrange(.data[[id_col]], Index_Date) %>%   # keep earliest index date
  dplyr::distinct(.data[[id_col]], .keep_all = TRUE)

after_n   <- nrow(merged_Index_Enc_Dem_age)
after_ids <- dplyr::n_distinct(merged_Index_Enc_Dem_age[[id_col]])

message(
  "Exposure merged dataset deduplicated by ", id_col, ": ",
  before_n, " → ", after_n, " rows | ",
  before_ids, " → ", after_ids, " unique IDs"
)


# -------------------------
# CONTROLS
# -------------------------
if (!isTRUE(cfg$include_controls)) {
  message("cfg$include_controls = FALSE → skipping control encounter filtering.")
  final_Control_Enc_filtered        <- NULL
  final_Control_Enc_filteredx2      <- NULL
  Index_Dates_Control_Enced         <- NULL
  merged_Control_Index_Enc_Dem      <- NULL
  merged_Control_Index_Enc_Dem_age  <- NULL
} else {
  
  # Build Index_Dates_Control from the randomized control index dates
  Index_Dates_Control <- final_Control_Enc %>%
    dplyr::select(dplyr::all_of(cfg$id_col), Index_Date) %>%
    dplyr::mutate(!!cfg$id_col := as.character(.data[[cfg$id_col]])) %>%
    dplyr::distinct()
  
  res_ctl <- filter_by_encounters(
    final_Enc   = final_Control_Enc,
    Index_Dates = Index_Dates_Control,
    final_Dem   = final_Control_Dem,
    cfg         = cfg,
    verbose     = TRUE
  )
  
  # Rename outputs to "Control" objects (same structure as exposure)
  final_Control_Enc_filtered   <- res_ctl$final_Exp_Enc_filtered
  final_Control_Enc_filteredx2 <- res_ctl$final_Exp_Enc_filteredx2
  Index_Dates_Control_Enced    <- res_ctl$Index_Dates_Enced
  merged_Control_Index_Enc_Dem <- res_ctl$merged_Index_Enc_Dem
  merged_Control_Index_Enc_Dem_age <- res_ctl$merged_Index_Enc_Dem_age
  
  #### Making sure control encounters + Dem have no duplicate IDs ####
  before_n   <- nrow(merged_Control_Index_Enc_Dem_age)
  before_ids <- dplyr::n_distinct(merged_Control_Index_Enc_Dem_age[[id_col]])
  
  merged_Control_Index_Enc_Dem_age <- merged_Control_Index_Enc_Dem_age %>%
    dplyr::arrange(.data[[id_col]], Index_Date) %>%   # keep earliest index date
    dplyr::distinct(.data[[id_col]], .keep_all = TRUE)
  
  after_n   <- nrow(merged_Control_Index_Enc_Dem_age)
  after_ids <- dplyr::n_distinct(merged_Control_Index_Enc_Dem_age[[id_col]])
  
  message(
    "Control merged dataset deduplicated by ", id_col, ": ",
    before_n, " → ", after_n, " rows | ",
    before_ids, " → ", after_ids, " unique IDs"
  )
  
}