# TBI_Claude_Code

R pipeline for studying outcomes in Traumatic Brain Injury (TBI) cohorts using electronic health record (EHR) data from a large academic medical center.

---

## Project Overview

This pipeline ingests pipe-delimited EHR exports (demographics, diagnoses, encounters, radiology), builds exposure/control cohorts, assigns TBI severity, filters by encounter criteria, and runs survival analyses (Kaplan–Meier + Cox regression) with optional propensity score matching.

---

## Repository Structure

```
TBI_Claude_Code/
├── R/                              # Reusable helper functions (sourced by scripts)
│   ├── 00_packages.R               # Library loading
│   ├── 01_config.R                 # ⚙️  Central config (paths, parameters, toggles)
│   ├── 02_read_exposure_inputs.R   # Read demographics & diagnoses (Exp + Control)
│   ├── assign_disease_state_and_combine_control_exp.R  # TBI severity assignment + cohort build
│   ├── assign_SUB_status.R         # Secondary outcome (SUB/seizure) status assignment
│   ├── filter_by_encounters.R      # Encounter-based inclusion filter
│   ├── read_index_dates.R          # Build index dates from earliest qualifying dx
│   ├── read_encounters.R           # Encounter summary (before/after index)
│   ├── read_control_encounters_random_index.R  # Random index date for controls
│   ├── read_imaging.R              # Read & classify radiology (_Rdt.txt) files
│   ├── exclusion_and_table1_prep.R # Baseline exclusion + wide dataset + Table 1 cleaning
│   ├── 06b_matching.R              # Propensity score matching (binary & 3-way)
│   ├── tables_1_2.R                # Table 1 / Table 2 generation (gtsummary)
│   └── 08_survival_outcomes.R      # KM plots + Cox regression + forest plots
│
├── scripts/                        # Ordered pipeline execution scripts
│   ├── 00_force_rebuild.R          # Delete cache + re-run full pipeline
│   ├── 01_read_exp_dem_dia.R       # Step 1: Read demographics & diagnoses
│   ├── 02_index_dates_and_encounters.R  # Step 2: Build index dates & encounter summaries
│   ├── 03_control_random_index_dates.R  # Step 3: Assign random index dates to controls
│   ├── 04_filtering_encounters.R   # Step 4: Filter by encounter criteria
│   ├── 05_assign_status_severity_and_combine.R  # Step 5: Assign TBI status & combine cohorts
│   ├── 05b_assign_pte_status.R     # Step 5b: Assign secondary outcome (SUB/seizure) status
│   ├── 05c_imaging_analysis.R      # Step 5c: Imaging (CT/MRI) analysis
│   ├── 06_exclusion_and_table1_prep.R   # Step 6: Baseline exclusion + Table 1 prep + matching
│   ├── 06b_cases_output.R          # Step 6b: Incident cases detail output (Excel)
│   ├── 07_tables.R                 # Step 7: Generate Table 1, Table 1.5, Table 2
│   └── 08_survival_outcomes_script.R    # Step 8: Survival analysis (KM + Cox)
│
├── outputs/                        # ⛔ Auto-generated; gitignored
└── .gitignore
```

---

## Setup

### 1. Prerequisites

- R ≥ 4.2
- Install required packages (run once):

```r
install.packages(c(
  "dplyr", "readxl", "lubridate", "tidyr", "MatchIt",
  "gtsummary", "survival", "survminer", "flextable",
  "stringr", "openxlsx", "forestplot", "gt", "purrr",
  "tibble", "rlang"
))
```

### 2. Configure data paths

Open `R/01_config.R` and set your `data_dir` and `excel_file` paths to point to your local data.

### 3. Open as an RStudio Project

- In RStudio: *File → New Project → Existing Directory* → select the repo folder
- Set the working directory to the project root before running any scripts

---

## Running the Pipeline

Scripts are designed to be run in order from an R session with the project root as working directory. Each script sources its dependencies from `R/` and relies on objects left in the environment by the previous step.

```r
source("scripts/01_read_exp_dem_dia.R")
source("scripts/02_index_dates_and_encounters.R")
source("scripts/03_control_random_index_dates.R")   # skip if cfg$include_controls = FALSE
source("scripts/04_filtering_encounters.R")
source("scripts/05_assign_status_severity_and_combine.R")
source("scripts/05b_assign_pte_status.R")
source("scripts/05c_imaging_analysis.R")
source("scripts/06_exclusion_and_table1_prep.R")
source("scripts/06b_cases_output.R")
source("scripts/07_tables.R")
source("scripts/08_survival_outcomes_script.R")
```

To force a full rebuild (clearing all cached `.rds` files first):

```r
source("scripts/00_force_rebuild.R")
```

---

## Key Configuration Toggles (`R/01_config.R`)

| Parameter | Description |
|---|---|
| `cfg$include_controls` | `TRUE/FALSE` — enable/disable control group processing |
| `cfg$do_matching` | `TRUE/FALSE` — enable propensity score matching |
| `cfg$matching_mode` | `"binary"` or `"three_way_twostep"` |
| `cfg$outcome_dx_values` | Diagnosis labels to study as outcomes |
| `cfg$status_col` | `"TBI_Status"` or `"SUB_Status"` — which column drives grouping |
| `cfg$project_name` | Used to auto-generate output filenames |

---

## Outputs

All outputs land in `cfg$output_dir` (default: `TBI_Tumor/`) and are gitignored.

| Type | Location |
|---|---|
| Cached data (`.rds`) | `outputs/` |
| Table 1 / Table 2 (`.html`) | `outputs/tables/` |
| Survival results (`.csv`) | `outputs/survival/results/` |
| KM plots (`.pdf`) | `outputs/survival/km/` |
| Forest plot (`.pdf`) | `outputs/survival/forest/` |
| Imaging summary (`.csv`) | `outputs/` |
| Incident cases (`.xlsx`) | `outputs/` |

---

## Notes

- Raw data files (`.txt`, `.csv`, `.xlsx`) and all outputs are gitignored and will never be committed.
- The pipeline uses RDS caching — if a step has already been run, it loads from cache. Use `scripts/00_force_rebuild.R` to clear and re-run.
- `cfg$project_slug` is auto-built from key settings (ICD types, encounter thresholds, matching mode) so output filenames reflect the exact analysis configuration.
