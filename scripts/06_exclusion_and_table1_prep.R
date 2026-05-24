
# scripts/06_exclusion_and_table1_prep.R

source("R/00_packages.R")
source("R/01_config.R")
source("R/exclusion_and_table1_prep.R")

# Requires these to exist from prior steps:
# - Indexed_Diagnoses_Combined
# - Total_Dem
stopifnot(exists("Indexed_Diagnoses_Combined"))
stopifnot(exists("Total_Dem"))

# 1) Add flags + get baseline exclusion IDs
res_excl <- get_baseline_exclusion_ids(
  Indexed_Diagnoses_Combined = Indexed_Diagnoses_Combined,
  cfg = cfg,
  verbose = TRUE
)

Indexed_Diagnoses_Combined <- res_excl$df_with_flags
Excl_EMPI_baseline <- res_excl$Excl_IDs

# 2) Build WIDE after dataset
Wide_everything_After <- build_wide_after(
  df_with_flags = Indexed_Diagnoses_Combined,
  Total_Dem = Total_Dem,
  cfg = cfg,
  verbose = TRUE
)

# 3) Clean for Table 1 (config-driven; supports TBI_Status OR SUB_Status)
Young_Clean_After <- clean_for_table1(
  wide_df = Wide_everything_After,
  cfg = cfg,
  verbose = TRUE
)

# 4) Apply baseline exclusion to create “No baseline outcome” cohort
id_col <- cfg$id_col
Young_Clean_NoBS_After <- Young_Clean_After %>%
  dplyr::filter(!(.data[[id_col]] %in% Excl_EMPI_baseline))

message("---- After baseline exclusion ----")
message("Unique IDs (Table1 cleaned): ", dplyr::n_distinct(Young_Clean_After[[id_col]]))
message("Unique IDs (No baseline outcome): ", dplyr::n_distinct(Young_Clean_NoBS_After[[id_col]]))

# 5) Save outputs
dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(Indexed_Diagnoses_Combined, file.path(cfg$output_dir, "Indexed_Diagnoses_Combined_with_baseline_flags.rds"))
saveRDS(Excl_EMPI_baseline, file.path(cfg$output_dir, "Excl_IDs_baseline.rds"))
saveRDS(Wide_everything_After, file.path(cfg$output_dir, "Wide_everything_After.rds"))
saveRDS(Young_Clean_After, file.path(cfg$output_dir, "Young_Clean_After.rds"))
saveRDS(Young_Clean_NoBS_After, file.path(cfg$output_dir, "Young_Clean_NoBS_After.rds"))

message("Saved outputs to: ", cfg$output_dir)


source("R/06b_matching.R")

match_res <- run_matching(
  df = Young_Clean_NoBS_After,
  cfg = cfg,
  verbose = TRUE
)

Young_Clean_NoBS_After_matched <- match_res$matched_df
Matched_sample_for_outcome <- tibble::tibble(!!cfg$id_col := match_res$matched_ids)

# Save like your old behavior
saveRDS(Matched_sample_for_outcome, file.path(cfg$output_dir, "Matched_sample_EMPI_outcome.rds"))
saveRDS(Young_Clean_NoBS_After_matched, file.path(cfg$output_dir, "Young_Clean_NoBS_After_matched.rds"))

message("Saved matched sample + matched dataset to outputs/")