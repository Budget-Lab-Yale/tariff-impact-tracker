# ==============================================================================
# Employment Index Calculation
# ==============================================================================
#
# Purpose: Calculate the tariff-exposed employment index using Leontief-adjusted
#          (total requirements) weights:
#
#   I_t = sum_j E_{j,t} * w_j
#
#   where w_j = sum_c tau_c * R_{c,j} * s_c  (Leontief-adjusted, primary)
#
# Where:
#   - I_t: Index at time t
#   - E_{j,t}: Employment of industry j at time t (from BLS via HAVER)
#   - tau_c: Tariff rate for commodity c (from USITC, by NAICS)
#   - R_{c,j}: Total requirements of commodity c per dollar of j's output
#              (BEA Commodity-by-Commodity Total Requirements / Leontief inverse)
#   - s_c: Direct import share of commodity c (BEA Import Matrix / Use Table)
#
# A direct-import reference index is also computed:
#   w_j^direct = (sum_c tau_c * x_{c,j}) / y_j
# where x_{c,j} = imported inputs, y_j = industry output
#
# Author: John Iselin, Yale Budget Lab
# Date: January 2026
# ==============================================================================

# ==============================================================================
# LOAD LIBRARIES
# ==============================================================================

# Haver is loaded conditionally via check_haver_available() in utils.R
# Do not use library(Haver) -- it will fail if the package is not installed
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(here)
library(stringr)

# ==============================================================================
# SECTION 1: CONFIGURATION
# ==============================================================================

here::i_am("R/employment_index.R")

# Source shared utilities
source(here("R", "utils.R"))

INPUT_DIR  <- here("input")
OUTPUT_DIR <- here("output")
LOG_DIR    <- here("logs")

# Canonical list of required input files (used by run_all.R for pre-flight checks)
employment_index_required_inputs <- function() {
  c("USITC - Customs and Duties - January 2026.xlsx",
    "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx",
    "BEA - The Use of Commodities by Industry - Summary - 2024.xlsx",
    "x_codes.csv",
    "naics_to_bea_crosswalk.csv")
}

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)

# Create log file and get logging function
EMP_IDX_LOG_FILE <- file.path(LOG_DIR, paste0("employment_index_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_msg <- create_logger(EMP_IDX_LOG_FILE)

log_msg("INFO", "Libraries loaded successfully")

# Initialize Haver direct connection
haver_available <- check_haver_available()

# Wrapper for pull_haver that uses script's haver_available and log_msg
# Automatically normalizes dates to first-of-month to avoid leap-year issues
pull_haver_local <- function(codes, frequency = "monthly", start_date, end_date = NULL) {
  result <- pull_haver(codes, frequency, start_date, end_date,
                       haver_available = haver_available, log_fn = log_msg)
  if (!is.null(result)) {
    result <- normalize_monthly_dates(result)
  }
  result
}

log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Employment Index Calculation")
log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))

# ==============================================================================
# SECTION 2: LOAD TARIFF DATA (tau_c)
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 1: Loading Tariff Data from USITC")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

usitc_file <- file.path(INPUT_DIR, "USITC - Customs and Duties - January 2026.xlsx")

# 1(a) Import data from both sheets
log_msg("INFO", paste("Reading from:", usitc_file))

customs_value <- read_excel(usitc_file, sheet = "Customs Value") %>%
  dplyr::select(
    year = Year,
    month = Month,
    naics = `NAIC Number`,
    description = Description,
    customs_value = `Customs Value`
  ) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    naics = as.character(naics),
    # Convert to numeric with logging of conversion failures
    customs_value = safe_as_numeric(customs_value, "customs_value", log_msg)
  ) %>%
  filter(!is.na(customs_value))

log_msg("INFO", paste("Read Customs Value:", nrow(customs_value), "rows"))

calculated_duties <- read_excel(usitc_file, sheet = "Calculated Duties") %>%
  dplyr::select(
    year = Year,
    month = Month,
    naics = `NAIC Number`,
    calculated_duties = `Calculated Duties`
  ) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    naics = as.character(naics),
    # Convert to numeric with logging of conversion failures
    calculated_duties = safe_as_numeric(calculated_duties, "calculated_duties", log_msg)
  ) %>%
  filter(!is.na(calculated_duties))

log_msg("INFO", paste("Read Calculated Duties:", nrow(calculated_duties), "rows"))

# 1(b) Keep 2025 data only
customs_value_2025 <- customs_value %>% filter(year == 2025)
calculated_duties_2025 <- calculated_duties %>% filter(year == 2025)

log_msg("INFO", paste("Filtered to 2025: Customs Value =", nrow(customs_value_2025),
                      "rows, Duties =", nrow(calculated_duties_2025), "rows"))

# 1(c) Merge together by year, month, and NAICS
tariff_data <- customs_value_2025 %>%
  left_join(
    calculated_duties_2025,
    by = c("year", "month", "naics")
  ) %>%
  mutate(
    calculated_duties = replace_na(calculated_duties, 0)
  )

log_msg("INFO", paste("Merged tariff data:", nrow(tariff_data), "rows"))
log_msg("INFO", paste("  Unique NAICS codes:", length(unique(tariff_data$naics))))
log_msg("INFO", paste("  Months available:", paste(sort(unique(tariff_data$month)), collapse = ", ")))

# ==============================================================================
# SECTION 3: LOAD IMPORT MATRIX (x_{c,j})
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 2: Loading Import Matrix from BEA")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

bea_import_file <- file.path(INPUT_DIR,
                              "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx")

# 2(a) Read data from A7:CQ80, replace "---" with 0
log_msg("INFO", paste("Reading from:", bea_import_file))

# Read the import matrix - data starts at row 7 (skip 6 rows for header)
# Columns A and B are commodity codes/names, row 7 has industry names
x_cj_raw <- read_excel(
  bea_import_file,
  sheet = "Table",
  range = "A7:CQ80",
  col_names = TRUE
)

log_msg("INFO", paste("Read raw import matrix:", nrow(x_cj_raw), "rows x", ncol(x_cj_raw), "cols"))

# Get column names - first two are commodity code and name
col_names <- names(x_cj_raw)
log_msg("INFO", paste("First 5 columns:", paste(col_names[1:5], collapse = ", ")))

# Rename first two columns for clarity
names(x_cj_raw)[1:2] <- c("bea_code_c", "bea_name_c")

# 2(b) Drop columns after and including "Federal general government (defense)" (column BQ)
# Find the column index for defense
defense_col_idx <- which(str_detect(names(x_cj_raw), "Federal general government \\(defense\\)|GFGD"))

if (length(defense_col_idx) > 0) {
  log_msg("INFO", paste("Found defense column at index:", defense_col_idx[1]))
  x_cj_raw <- x_cj_raw[, 1:(defense_col_idx[1] - 1)]
  log_msg("INFO", paste("After dropping defense+ columns:", ncol(x_cj_raw), "cols"))
} else {
  log_msg("WARN", "Defense column not found - keeping all columns")
}

# 2(c) Drop rows after and including row 74 (Federal general government (defense))
# Row 74 in the original corresponds to row 68 after removing header rows (74 - 6 = 68)
# But we need to find it by content
defense_row_idx <- which(str_detect(x_cj_raw$bea_code_c, "GFGD") |
                          str_detect(x_cj_raw$bea_name_c, "Federal general government \\(defense\\)"))

if (length(defense_row_idx) > 0) {
  log_msg("INFO", paste("Found defense row at index:", defense_row_idx[1]))
  x_cj_raw <- x_cj_raw[1:(defense_row_idx[1] - 1), ]
  log_msg("INFO", paste("After dropping defense+ rows:", nrow(x_cj_raw), "rows"))
} else {
  log_msg("WARN", "Defense row not found - keeping all rows")
}

# Replace "---" and any non-numeric values with 0
industry_cols <- names(x_cj_raw)[3:ncol(x_cj_raw)]

for (col in industry_cols) {
  x_cj_raw[[col]] <- as.character(x_cj_raw[[col]])
  x_cj_raw[[col]] <- ifelse(x_cj_raw[[col]] == "---" | is.na(x_cj_raw[[col]]), "0", x_cj_raw[[col]])
  x_cj_raw[[col]] <- as.numeric(x_cj_raw[[col]])
}

log_msg("INFO", "Replaced '---' with 0s")

# 2(d) Reshape to long format
# Need to create mapping from industry names to BEA codes
# The column names are industry names, we need to look up their codes

# First, let's load x_codes to get the mapping
x_codes <- read_csv(file.path(INPUT_DIR, "x_codes.csv"), show_col_types = FALSE)
log_msg("INFO", paste("Loaded x_codes:", nrow(x_codes), "BEA codes"))

# Create a lookup from description to code
industry_lookup <- x_codes %>%
  dplyr::select(bea_code_j = `Commodity Code`, bea_name_j = Description)

# Reshape to long
x_cj_long <- x_cj_raw %>%
  pivot_longer(
    cols = all_of(industry_cols),
    names_to = "bea_name_j",
    values_to = "x_cj"
  ) %>%
  filter(!is.na(x_cj))

log_msg("INFO", paste("Reshaped to long format:", nrow(x_cj_long), "rows"))

# Match industry names to codes
# Clean up names for matching
x_cj_long <- x_cj_long %>%
  mutate(
    bea_name_j_clean = str_trim(bea_name_j),
    bea_name_j_clean = str_replace_all(bea_name_j_clean, "\\s+", " ")
  )

industry_lookup <- industry_lookup %>%
  mutate(
    bea_name_j_clean = str_trim(bea_name_j),
    bea_name_j_clean = str_replace_all(bea_name_j_clean, "\\s+", " ")
  )

# Join to get industry codes
x_cj_long <- x_cj_long %>%
  left_join(
    industry_lookup %>% dplyr::select(bea_code_j, bea_name_j_clean),
    by = "bea_name_j_clean"
  ) %>%
  dplyr::select(bea_code_c, bea_name_c, bea_code_j, bea_name_j, x_cj)

# Check for unmatched industries
unmatched_industries <- x_cj_long %>%
  filter(is.na(bea_code_j)) %>%
  distinct(bea_name_j)

if (nrow(unmatched_industries) > 0) {
  log_msg("WARN", paste("Unmatched industries:", nrow(unmatched_industries)))
  log_msg("WARN", paste("  ", head(unmatched_industries$bea_name_j, 5), collapse = ", "))
}

log_msg("INFO", paste("Final x_cj dataset:", nrow(x_cj_long), "rows"))
log_msg("INFO", paste("  Unique commodities:", length(unique(x_cj_long$bea_code_c))))
log_msg("INFO", paste("  Unique industries:", length(unique(x_cj_long$bea_code_j))))

# ==============================================================================
# SECTION 3B: LOAD USE TABLE AND COMPUTE DIRECT IMPORT SHARES (s_c)
# ==============================================================================
# For the Leontief-adjusted employment weights, we need commodity-level direct
# import shares: s_c = total_imports_c / total_use_c. These are the same shares
# computed in import_price_index.R.
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 2B: Loading Use Table and Computing Import Shares")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# --- 2B(a): Get total imports per commodity from Import Matrix ---
# Re-read the full Import Matrix to access the "Imports of goods and services"
# column, which was dropped in Section 3 when we trimmed to industry-only columns.
import_full_raw <- read_excel(
  bea_import_file, sheet = "Table",
  range = "A7:CQ80", col_names = TRUE
)
names(import_full_raw)[1:2] <- c("bea_code", "bea_name")

# Replace "---" with 0 in all data columns
for (col in names(import_full_raw)[3:ncol(import_full_raw)]) {
  import_full_raw[[col]] <- as.character(import_full_raw[[col]])
  import_full_raw[[col]] <- ifelse(import_full_raw[[col]] == "---" | is.na(import_full_raw[[col]]),
                                    "0", import_full_raw[[col]])
  import_full_raw[[col]] <- as.numeric(import_full_raw[[col]])
}

# Find "Imports of goods and services" column
imports_col_idx <- which(str_detect(str_to_lower(names(import_full_raw)),
                                     "^imports of goods"))

if (length(imports_col_idx) > 0) {
  imports_col <- names(import_full_raw)[imports_col_idx[1]]
  log_msg("INFO", paste("Found imports column:", imports_col))

  import_totals_emp <- import_full_raw %>%
    dplyr::select(bea_code, bea_name) %>%
    dplyr::mutate(
      bea_code = str_trim(as.character(bea_code)),
      total_imports = abs(import_full_raw[[imports_col]])
    )
} else {
  log_msg("WARN", "Imports column not found - computing row sums as fallback")
  data_cols_full <- names(import_full_raw)[3:ncol(import_full_raw)]
  import_totals_emp <- import_full_raw %>%
    dplyr::select(bea_code, bea_name) %>%
    dplyr::mutate(
      bea_code = str_trim(as.character(bea_code)),
      total_imports = rowSums(import_full_raw[, data_cols_full], na.rm = TRUE)
    )
}

log_msg("INFO", paste("Computed total imports for", nrow(import_totals_emp), "commodities"))

# --- 2B(b): Read Use Table for Total Use of Products ---
use_table_file <- file.path(INPUT_DIR,
  "BEA - The Use of Commodities by Industry - Summary - 2024.xlsx")
log_msg("INFO", paste("Reading Use Table:", basename(use_table_file)))

use_raw <- read_excel(use_table_file, sheet = "Table",
                       range = "A6:CP89", col_names = TRUE)
log_msg("INFO", paste("Read use table:", nrow(use_raw), "rows x", ncol(use_raw), "cols"))

names(use_raw)[1:2] <- c("row_code", "row_name")

# T019 column = last column = "Total use of products"
t019_col_name <- names(use_raw)[ncol(use_raw)]
log_msg("INFO", paste("  Total use column:", t019_col_name))

use_totals_emp <- use_raw %>%
  dplyr::mutate(
    bea_code = str_trim(as.character(row_code)),
    total_use = suppressWarnings(as.numeric(as.character(.data[[t019_col_name]])))
  ) %>%
  dplyr::filter(
    !is.na(bea_code),
    !str_detect(bea_code, "^T0|^V0|^VA")
  ) %>%
  dplyr::select(bea_code, total_use)

log_msg("INFO", paste("Got total use for", nrow(use_totals_emp), "commodities"))

# --- 2B(c): Compute direct import shares ---
import_shares_emp <- import_totals_emp %>%
  dplyr::left_join(use_totals_emp, by = "bea_code") %>%
  dplyr::mutate(
    import_share = safe_divide(total_imports, total_use, default = 0),
    import_share = pmin(import_share, 1.0)  # Cap at 100%
  )

log_msg("INFO", paste("Computed import shares for", nrow(import_shares_emp), "commodities"))
log_msg("INFO", paste("  Mean import share:",
                      round(mean(import_shares_emp$import_share, na.rm = TRUE) * 100, 1), "%"))

rm(import_full_raw, use_raw)  # Free memory

# ==============================================================================
# SECTION 3C: LOAD TOTAL REQUIREMENTS MATRIX (Leontief Inverse)
# ==============================================================================
# The BEA Commodity-by-Commodity Total Requirements matrix R = (I - A)^{-1}
# gives the total amount of each commodity required (directly + indirectly) per
# dollar of another commodity's output.
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 2C: Loading Total Requirements Matrix")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

total_req_file <- file.path(INPUT_DIR,
  "BEA - Commodity-by-Commodity Total Requirements - Summary - 2024.xlsx")

total_req_available <- FALSE

if (file.exists(total_req_file)) {
  log_msg("INFO", paste("Reading:", basename(total_req_file)))

  # Read full sheet to detect layout
  tr_raw <- read_excel(total_req_file, sheet = 1, col_names = FALSE)
  log_msg("INFO", paste("  Raw sheet:", nrow(tr_raw), "rows x", ncol(tr_raw), "cols"))

  # --- Detect structure ---
  bea_code_strict <- "^[0-9]{2,3}[A-Z0-9]*$"
  bea_code_broad <- "^[A-Za-z0-9]{2,6}$"
  data_start <- NA
  for (i in 1:min(20, nrow(tr_raw))) {
    val <- str_trim(as.character(tr_raw[[1]][i]))
    if (!is.na(val) && nchar(val) >= 2 && nchar(val) <= 6 &&
        str_detect(val, bea_code_strict)) {
      data_start <- i
      break
    }
  }

  if (is.na(data_start)) {
    log_msg("ERROR", "Could not detect data rows in Total Requirements table")
    log_msg("WARN", "Leontief adjustment will not be available")
  } else {
    # Find data end
    data_end <- data_start
    for (i in (data_start + 1):min(nrow(tr_raw), data_start + 100)) {
      val <- str_trim(as.character(tr_raw[[1]][i]))
      if (is.na(val) || val == "" ||
          str_detect(val, "^(Legend|Note|Source|Total)")) break
      data_end <- i
    }

    # Find column code row
    code_row <- NA
    for (i in max(1, data_start - 5):(data_start - 1)) {
      vals <- str_trim(as.character(tr_raw[i, ]))
      n_matches <- sum(str_detect(vals[3:length(vals)], bea_code_broad), na.rm = TRUE)
      if (n_matches >= 10) {
        code_row <- i
        break
      }
    }

    log_msg("INFO", paste("  Data rows:", data_start, "to", data_end,
                           "(", data_end - data_start + 1, "rows)"))
    log_msg("INFO", paste("  Column code row:",
                           ifelse(is.na(code_row), "not found", code_row)))

    # Extract row codes
    row_codes <- str_trim(as.character(tr_raw[[1]][data_start:data_end]))

    # Determine number of data columns
    last_col <- ncol(tr_raw)
    test_row <- if (!is.na(code_row)) code_row else data_start
    for (j in ncol(tr_raw):3) {
      val <- as.character(tr_raw[[j]][test_row])
      if (!is.na(val) && str_trim(val) != "" && str_trim(val) != "---") {
        last_col <- j
        break
      }
    }

    # Extract column codes
    if (!is.na(code_row)) {
      col_codes <- str_trim(as.character(tr_raw[code_row, 3:last_col]))
    } else {
      log_msg("WARN", "Column code row not found - assuming columns match row codes")
      col_codes <- row_codes[1:min(length(row_codes), last_col - 2)]
    }

    # Remove trailing NA/empty
    valid_cols <- !is.na(col_codes) & col_codes != ""
    col_codes <- col_codes[valid_cols]
    n_data_cols <- length(col_codes)

    log_msg("INFO", paste("  Matrix:", length(row_codes), "x", n_data_cols))
    log_msg("INFO", paste("  First 5 row codes:", paste(head(row_codes, 5), collapse = ", ")))
    log_msg("INFO", paste("  First 5 col codes:", paste(head(col_codes, 5), collapse = ", ")))

    # Build numeric matrix
    R_matrix <- matrix(0, nrow = length(row_codes), ncol = n_data_cols)
    for (j in 1:n_data_cols) {
      col_idx <- which(valid_cols)[j] + 2  # offset for 2 label columns
      vals <- as.character(tr_raw[[col_idx]][data_start:data_end])
      vals[is.na(vals) | vals == "---" | vals == "..."] <- "0"
      R_matrix[, j] <- suppressWarnings(as.numeric(vals))
      R_matrix[is.na(R_matrix[, j]), j] <- 0
    }
    rownames(R_matrix) <- row_codes
    colnames(R_matrix) <- col_codes

    # Sanity check: row sums should generally be >= 1
    row_sums <- rowSums(R_matrix)
    log_msg("INFO", paste("  Row sum range:", round(min(row_sums), 2), "to",
                           round(max(row_sums), 2), "(should generally be >= 1)"))

    total_req_available <- TRUE
    log_msg("INFO", "Total Requirements matrix loaded successfully")
  }

  rm(tr_raw)  # Free memory
} else {
  log_msg("WARN", paste("Total Requirements file not found:", basename(total_req_file)))
  log_msg("WARN", "Will use direct import weights only (no Leontief adjustment)")
}

# ==============================================================================
# SECTION 4: LOAD CROSSWALK
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 3: Loading NAICS to BEA Crosswalk")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

crosswalk <- read_csv(
  file.path(INPUT_DIR, "naics_to_bea_crosswalk.csv"),
  show_col_types = FALSE
) %>%
  mutate(naics_code = as.character(naics_code))

log_msg("INFO", paste("Loaded crosswalk:", nrow(crosswalk), "mappings"))
log_msg("INFO", paste("  Match types:", paste(table(crosswalk$match_type), collapse = ", ")))

# ==============================================================================
# SECTION 5: ADJUST TARIFF/COMMODITY GROUPS FOR MERGE
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 4: Adjusting Tariff and Commodity Groups")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# 4(a) For tariff data: merge on BEA codes, drop if unmatched, collapse to BEA level
# For NAICS 336, the crosswalk maps to both 3361MV and 3364OT - this is intentional
# (many-to-many: multiple months per NAICS, and NAICS 336 maps to 2 BEA codes)

tariff_with_bea <- tariff_data %>%
  left_join(
    crosswalk %>%
      filter(match_type != "unmatched") %>%
      dplyr::select(naics = naics_code, bea_code = bea_code),
    by = "naics",
    relationship = "many-to-many"
  )

# Check for unmatched
unmatched_tariff <- tariff_with_bea %>% filter(is.na(bea_code))
matched_tariff <- tariff_with_bea %>% filter(!is.na(bea_code))

log_msg("INFO", paste("Tariff data with BEA codes:", nrow(matched_tariff), "rows matched,",
                      nrow(unmatched_tariff), "rows unmatched"))

if (nrow(unmatched_tariff) > 0) {
  log_msg("INFO", "Unmatched NAICS codes (dropping):")
  unmatched_naics <- unique(unmatched_tariff$naics)
  for (n in unmatched_naics) {
    desc <- unique(unmatched_tariff$description[unmatched_tariff$naics == n])
    log_msg("INFO", paste("  ", n, "-", desc[1]))
  }
}

# Collapse to BEA level (sum customs values and duties by BEA code and month)
tariff_by_bea <- matched_tariff %>%
  group_by(year, month, bea_code) %>%
  summarize(
    customs_value = sum(customs_value, na.rm = TRUE),
    calculated_duties = sum(calculated_duties, na.rm = TRUE),
    .groups = "drop"
  )

log_msg("INFO", paste("Collapsed tariff data:", nrow(tariff_by_bea), "rows"))
log_msg("INFO", paste("  Unique BEA codes:", length(unique(tariff_by_bea$bea_code))))

# 4(b) BEA 336 Aggregation:
# The BEA import matrix splits transportation equipment into 3361MV (Motor vehicles,
# bodies, trailers, and parts) and 3364OT (Other transportation equipment). However,
# USITC tariff data and BLS employment data use the aggregate 336 (Transportation
# equipment). We collapse 3361MV + 3364OT -> 336 in three places:
#   1. x_cj commodity dimension (here)
#   2. y_j industry output (Section 7)
#   3. x_cj industry dimension (Section 7)
x_cj_adjusted <- x_cj_long %>%
  mutate(
    bea_code_c = case_when(
      bea_code_c %in% c("3361MV", "3364OT") ~ "336",
      TRUE ~ bea_code_c
    )
  ) %>%
  group_by(bea_code_c, bea_code_j) %>%
  summarize(
    bea_name_c = first(bea_name_c),  # Keep first name
    bea_name_j = first(bea_name_j),
    x_cj = sum(x_cj, na.rm = TRUE),
    .groups = "drop"
  )

log_msg("INFO", paste("Adjusted x_cj (collapsed 336):", nrow(x_cj_adjusted), "rows"))
log_msg("INFO", paste("  Unique commodity codes:", length(unique(x_cj_adjusted$bea_code_c))))

# ==============================================================================
# SECTION 6: CALCULATE TARIFF RATES
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 5: Calculating Tariff Rates")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# 5(a) Full 2025 values: duties / customs value for all of 2025
tau_c_2025 <- tariff_by_bea %>%
  group_by(bea_code) %>%
  summarize(
    customs_value_2025 = sum(customs_value, na.rm = TRUE),
    duties_2025 = sum(calculated_duties, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tau_c_2025 = safe_divide(duties_2025, customs_value_2025, default = 0)
  )

log_msg("INFO", paste("Full 2025 tariff rates:", nrow(tau_c_2025), "BEA codes"))
log_msg("INFO", paste("  Mean rate:", round(mean(tau_c_2025$tau_c_2025) * 100, 2), "%"))
log_msg("INFO", paste("  Weighted mean:",
                      round(sum(tau_c_2025$duties_2025) / sum(tau_c_2025$customs_value_2025) * 100, 2), "%"))

# 5(b) Most recent 2025 values: duties / customs value for max month in 2025
max_month <- max(tariff_by_bea$month)
log_msg("INFO", paste("Most recent month in data:", max_month))

tau_c_recent <- tariff_by_bea %>%
  filter(month == max_month) %>%
  dplyr::select(bea_code, customs_value_recent = customs_value, duties_recent = calculated_duties) %>%
  mutate(
    tau_c_recent = safe_divide(duties_recent, customs_value_recent, default = 0)
  )

log_msg("INFO", paste("Recent month tariff rates:", nrow(tau_c_recent), "BEA codes"))
log_msg("INFO", paste("  Mean rate:", round(mean(tau_c_recent$tau_c_recent) * 100, 2), "%"))

# 5(c) June 2025 values: duties / customs value for month == 6
june_data <- tariff_by_bea %>% filter(month == 6)

if (nrow(june_data) > 0) {
  tau_c_june <- june_data %>%
    dplyr::select(bea_code, customs_value_june = customs_value, duties_june = calculated_duties) %>%
    mutate(
      tau_c_june = safe_divide(duties_june, customs_value_june, default = 0)
    )
  log_msg("INFO", paste("June tariff rates:", nrow(tau_c_june), "BEA codes"))
  log_msg("INFO", paste("  Mean rate:", round(mean(tau_c_june$tau_c_june) * 100, 2), "%"))
} else {
  log_msg("WARN", "No June 2025 data found in tariff_by_bea; tau_c_june will be NULL")
  tau_c_june <- NULL
}

# Combine all measures
tau_c <- tau_c_2025 %>%
  left_join(tau_c_recent %>% dplyr::select(bea_code, tau_c_recent), by = "bea_code")

if (!is.null(tau_c_june)) {
  tau_c <- tau_c %>%
    left_join(tau_c_june %>% dplyr::select(bea_code, customs_value_june, duties_june, tau_c_june),
              by = "bea_code")
}

log_msg("INFO", paste("Combined tariff rates:", nrow(tau_c), "BEA codes"))

# Display top tariff rates
log_msg("INFO", "Top 10 tariff rates (2025 average):")
top_tariffs <- tau_c %>% arrange(desc(tau_c_2025)) %>% head(10)
for (i in 1:nrow(top_tariffs)) {
  log_msg("INFO", paste("  ", top_tariffs$bea_code[i], ":",
                        round(top_tariffs$tau_c_2025[i] * 100, 2), "%"))
}

# ==============================================================================
# SECTION 7: MERGE TARIFF RATES ONTO COMMODITY CODES
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 6: Merging Tariff Rates onto Import Matrix")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# Merge tau_c onto x_cj by commodity code
# Assign unmatched commodities tariff of 0
x_cj_with_tariff <- x_cj_adjusted %>%
  left_join(
    tau_c %>% dplyr::select(bea_code, tau_c_2025, tau_c_recent),
    by = c("bea_code_c" = "bea_code")
  ) %>%
  mutate(
    tau_c_2025 = replace_na(tau_c_2025, 0),
    tau_c_recent = replace_na(tau_c_recent, 0)
  )

# Check coverage
matched_commodities <- x_cj_with_tariff %>%
  filter(tau_c_2025 > 0) %>%
  distinct(bea_code_c)

unmatched_commodities <- x_cj_with_tariff %>%
  filter(tau_c_2025 == 0) %>%
  distinct(bea_code_c)

log_msg("INFO", paste("Commodities with tariff data:", nrow(matched_commodities)))
log_msg("INFO", paste("Commodities with zero tariff (unmatched):", nrow(unmatched_commodities)))

# ==============================================================================
# SECTION 8: LOAD INDUSTRY OUTPUT (y_j)
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 7: Loading Industry Output (y_j) from Haver")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# Crosswalk: BEA industry -> Haver codes (from tariff_impacts_work.R lines 546-644)
y_j_crosswalk <- tribble(
  ~bea_industry, ~haver_code, ~description, ~var_name,

  # Agriculture & Mining
  "111CA", "USTIA1@USNA", "Farms", "farms",
  "113FF", "USTIA3@USNA", "Forestry, fishing, and related activities", "forestry_fishing",
  "211", "USTIB1@USNA", "Oil and gas extraction", "oil_gas_extraction",
  "212", "USTIB2@USNA", "Mining, except oil and gas", "mining_ex_oil_gas",
  "213", "USTIB3@USNA", "Support activities for mining", "support_mining",

  # Utilities & Construction
  "22", "USTIC0@USNA", "Utilities", "utilities",
  "23", "USTID0@USNA", "Construction", "construction",

  # Manufacturing - Durable
  "321", "USTIF1@USNA", "Wood products", "wood_products",
  "327", "USTIF7@USNA", "Nonmetallic mineral products", "nonmetallic_minerals",
  "331", "USTIG1@USNA", "Primary metals", "primary_metals",
  "332", "USTIG2@USNA", "Fabricated metal products", "fabricated_metals",
  "333", "USTIG3@USNA", "Machinery", "machinery",
  "334", "USTIG4@USNA", "Computer and electronic products", "computer_electronic",
  "335", "USTIG5@USNA", "Electrical equipment, appliances, and components", "electrical_equipment",
  "3361MV", "USTIG6@USNA", "Motor vehicles, bodies and trailers, and parts", "motor_vehicles",
  "3364OT", "USTIGA@USNA", "Other transportation equipment", "other_transport_equip",
  "337", "USTIG7@USNA", "Furniture and related products", "furniture",
  "339", "USTIG9@USNA", "Miscellaneous manufacturing", "misc_manufacturing",

  # Manufacturing - Nondurable
  "311FT", "USTIE1@USNA", "Food and beverage and tobacco products", "food_bev_tobacco",
  "313TT", "USTIE3@USNA", "Textile mills and textile product mills", "textiles",
  "315AL", "USTIE5@USNA", "Apparel and leather and allied products", "apparel_leather",
  "322", "USTIF2@USNA", "Paper products", "paper_products",
  "323", "USTIF3@USNA", "Printing and related support activities", "printing",
  "324", "USTIF4@USNA", "Petroleum and coal products", "petroleum_coal",
  "325", "USTIF5@USNA", "Chemical products", "chemicals",
  "326", "USTIF6@USNA", "Plastics and rubber products", "plastics_rubber",

  # Trade
  "42", "USTIH0@USNA", "Wholesale trade", "wholesale_trade",
  "441", "USTII1@USNA", "Motor vehicle and parts dealers", "auto_dealers",
  "445", "USTII5@USNA", "Food and beverage stores", "food_bev_stores",
  "452", "USTIJ2@USNA", "General merchandise stores", "general_merch",
  "4A0", "USTIOR@USNA", "Other retail", "other_retail",

  # Transportation & Warehousing
  "481", "USTIK1@USNA", "Air transportation", "air_transport",
  "482", "USTIK2@USNA", "Rail transportation", "rail_transport",
  "483", "USTIK3@USNA", "Water transportation", "water_transport",
  "484", "USTIK4@USNA", "Truck transportation", "truck_transport",
  "485", "USTIK5@USNA", "Transit and ground passenger transportation", "transit_ground",
  "486", "USTIK6@USNA", "Pipeline transportation", "pipeline_transport",
  "487OS", "USTIK7@USNA", "Other transportation and support activities", "other_transport_support",
  "493", "USTIL3@USNA", "Warehousing and storage", "warehousing",

  # Information
  "511", "USTIM1@USNA", "Publishing industries, except internet (includes software)", "publishing",
  "512", "USTIM2@USNA", "Motion picture and sound recording industries", "motion_picture",
  "513", "USTIM3@USNA", "Broadcasting and telecommunications", "broadcasting_telecom",
  "514", "USTIM4@USNA", "Data processing, internet publishing, and other info services", "data_processing",

  # Finance
  "521CI", "USTIN1@USNA", "Federal Reserve banks, credit intermediation, and related", "credit_intermediation",
  "523", "USTIN3@USNA", "Securities, commodity contracts, and investments", "securities",
  "524", "USTIN4@USNA", "Insurance carriers and related activities", "insurance",
  "525", "USTIN5@USNA", "Funds, trusts, and other financial vehicles", "funds_trusts",

  # Real Estate
  "HS", "USTIHS@USNA", "Housing", "housing",
  "ORE", "USTIOE@USNA", "Other real estate", "other_real_estate",
  "532RL", "USTIO2@USNA", "Rental and leasing services and lessors of intangible assets", "rental_leasing",

  # Professional Services
  "5411", "USTIP1@USNA", "Legal services", "legal_services",
  "5415", "USTIP5@USNA", "Computer systems design and related services", "computer_systems_design",
  "5412OP", "USTIP2@USNA", "Miscellaneous professional, scientific, and technical services", "misc_professional",

  # Management & Administrative
  "55", "USTIQ0@USNA", "Management of companies and enterprises", "management_companies",
  "561", "USTIR1@USNA", "Administrative and support services", "admin_support",
  "562", "USTIR2@USNA", "Waste management and remediation services", "waste_management",

  # Education & Health
  "61", "USTIS0@USNA", "Educational services", "education",
  "621", "USTIT1@USNA", "Ambulatory health care services", "ambulatory_health",
  "622", "USTIT2@USNA", "Hospitals", "hospitals",
  "623", "USTIT3@USNA", "Nursing and residential care facilities", "nursing_care",
  "624", "USTIT4@USNA", "Social assistance", "social_assistance",

  # Arts, Entertainment, Recreation
  "711AS", "USTIU1@USNA", "Performing arts, spectator sports, museums, and related", "arts_sports",
  "713", "USTIU3@USNA", "Amusements, gambling, and recreation industries", "amusements",

  # Accommodation & Food
  "721", "USTIV1@USNA", "Accommodation", "accommodation",
  "722", "USTIV2@USNA", "Food services and drinking places", "food_services",

  # Other Services
  "81", "USTIW0@USNA", "Other services, except government", "other_services"
)

log_msg("INFO", paste("Built output crosswalk with", nrow(y_j_crosswalk), "industries"))

# Pull output data from Haver (annually)
all_y_codes <- y_j_crosswalk$haver_code

if (haver_available && length(all_y_codes) > 0) {
  y_j_raw <- pull_haver_local(all_y_codes, "annual", "1996-01-01")

  if (!is.null(y_j_raw)) {
    log_msg("INFO", paste("Pulled output data:", nrow(y_j_raw), "rows,",
                          ncol(y_j_raw) - 1, "series"))

    # Get most recent year's data
    y_j_recent <- y_j_raw %>%
      filter(date == max(date))

    log_msg("INFO", paste("Most recent year:", format(y_j_recent$date[1], "%Y")))

    # Reshape to long format with BEA codes
    # Note: Haver returns lowercase column names, so we standardize to lowercase
    y_j_data <- y_j_recent %>%
      pivot_longer(
        cols = -date,
        names_to = "haver_code_lower",
        values_to = "y_j"
      ) %>%
      mutate(
        # Extract code portion and ensure lowercase
        haver_code_lower = str_to_lower(str_extract(haver_code_lower, "^[a-z0-9]+"))
      )

    # Join with crosswalk to get BEA codes
    # Standardize crosswalk codes to lowercase for matching
    y_j_crosswalk_lower <- y_j_crosswalk %>%
      mutate(haver_code_lower = str_to_lower(str_extract(haver_code, "^[a-zA-Z0-9]+")))

    y_j_data <- y_j_data %>%
      left_join(
        y_j_crosswalk_lower %>% dplyr::select(bea_industry, haver_code_lower, description),
        by = "haver_code_lower"
      ) %>%
      filter(!is.na(bea_industry)) %>%
      dplyr::select(bea_code_j = bea_industry, y_j, description)

    log_msg("INFO", paste("Industry output data:", nrow(y_j_data), "industries"))

    # Cache raw Haver pull for offline fallback
    tryCatch({
      write_csv(y_j_recent, file.path(OUTPUT_DIR, "y_j_output.csv"))
      log_msg("INFO", "Cached industry output to y_j_output.csv")
    }, error = function(e) {
      log_msg("WARN", paste("Could not cache y_j_output.csv:", e$message))
    })

    log_msg("INFO", "Industry output data ready")
  }
} else {
  log_msg("WARN", "Could not pull output data from Haver")

  # Try to load from existing file
  y_j_file <- file.path(OUTPUT_DIR, "y_j_output.csv")
  if (file.exists(y_j_file)) {
    # Staleness check: warn if cache is older than 90 days
    cache_age_days <- as.numeric(difftime(Sys.time(), file.mtime(y_j_file), units = "days"))
    if (cache_age_days > 90) {
      log_msg("WARN", sprintf("y_j_output.csv is %.0f days old -- consider refreshing with Haver", cache_age_days))
    }
    log_msg("INFO", sprintf("Loading from existing y_j_output.csv (%.0f days old)", cache_age_days))
    y_j_existing <- read_csv(y_j_file, show_col_types = FALSE)

    # Get most recent year and reshape
    y_j_existing <- y_j_existing %>%
      filter(date == max(date)) %>%
      pivot_longer(
        cols = -date,
        names_to = "haver_code_lower",
        values_to = "y_j"
      ) %>%
      mutate(
        haver_code_lower = str_to_lower(str_extract(haver_code_lower, "^[a-z0-9]+"))
      )

    # Join with crosswalk using haver codes (CSV columns are lowercase Haver codes)
    y_j_crosswalk_lower <- y_j_crosswalk %>%
      mutate(haver_code_lower = str_to_lower(str_extract(haver_code, "^[a-zA-Z0-9]+")))

    y_j_data <- y_j_existing %>%
      left_join(
        y_j_crosswalk_lower %>% dplyr::select(bea_industry, haver_code_lower),
        by = "haver_code_lower"
      ) %>%
      filter(!is.na(bea_industry)) %>%
      dplyr::select(bea_code_j = bea_industry, y_j)

    log_msg("INFO", paste("Loaded industry output:", nrow(y_j_data), "industries"))
  } else {
    log_msg("ERROR", "No industry output data available")
    y_j_data <- NULL
  }
}

# ==============================================================================
# SECTION 9: CALCULATE INDUSTRY ADJUSTMENT
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 8: Calculating Industry Adjustment")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# Formula: (sum_c tau_c * x_{c,j}) / y_j

if (!is.null(y_j_data)) {

  # Collapse 3361MV + 3364OT -> 336 in y_j (see Section 4b for rationale)
  y_j_adjusted <- y_j_data %>%
    mutate(
      bea_code_j = case_when(
        bea_code_j %in% c("3361MV", "3364OT") ~ "336",
        TRUE ~ bea_code_j
      )
    ) %>%
    group_by(bea_code_j) %>%
    summarize(
      y_j = sum(y_j, na.rm = TRUE),
      .groups = "drop"
    )

  log_msg("INFO", paste("Adjusted y_j (collapsed 336):", nrow(y_j_adjusted), "industries"))

  # Collapse 3361MV + 3364OT -> 336 in x_cj industry dimension (see Section 4b)
  x_cj_industry_adjusted <- x_cj_with_tariff %>%
    mutate(
      bea_code_j = case_when(
        bea_code_j %in% c("3361MV", "3364OT") ~ "336",
        TRUE ~ bea_code_j
      )
    ) %>%
    group_by(bea_code_c, bea_code_j) %>%
    summarize(
      x_cj = sum(x_cj, na.rm = TRUE),
      tau_c_2025 = first(tau_c_2025),
      tau_c_recent = first(tau_c_recent),
      .groups = "drop"
    )

  # Calculate weighted tariff exposure by industry: sum_c(tau_c * x_{c,j})
  industry_tariff_exposure <- x_cj_industry_adjusted %>%
    mutate(
      tariff_weighted_imports_2025 = tau_c_2025 * x_cj,
      tariff_weighted_imports_recent = tau_c_recent * x_cj
    ) %>%
    group_by(bea_code_j) %>%
    summarize(
      total_imports = sum(x_cj, na.rm = TRUE),
      sum_tau_x_2025 = sum(tariff_weighted_imports_2025, na.rm = TRUE),
      sum_tau_x_recent = sum(tariff_weighted_imports_recent, na.rm = TRUE),
      .groups = "drop"
    )

  log_msg("INFO", paste("Calculated tariff exposure for", nrow(industry_tariff_exposure), "industries"))

  # Merge with output data
  industry_adjustment <- industry_tariff_exposure %>%
    left_join(y_j_adjusted, by = "bea_code_j") %>%
    mutate(
      # Industry adjustment = (sum_c tau_c * x_{c,j}) / y_j
      # Use safe_divide to handle NA and zero denominators
      adjustment_2025 = safe_divide(sum_tau_x_2025, y_j, default = 0),
      adjustment_recent = safe_divide(sum_tau_x_recent, y_j, default = 0)
    )

  # Check for missing y_j values
  missing_y <- industry_adjustment %>% filter(is.na(y_j))
  if (nrow(missing_y) > 0) {
    log_msg("WARN", paste("Industries missing output data:", nrow(missing_y)))
    log_msg("WARN", paste("  ", paste(head(missing_y$bea_code_j, 10), collapse = ", ")))
  }

  # Filter to industries with valid data
  industry_adjustment_valid <- industry_adjustment %>%
    filter(!is.na(y_j) & y_j > 0)

  log_msg("INFO", paste("Valid industry adjustments:", nrow(industry_adjustment_valid), "industries"))

  # Display top adjustments
  log_msg("INFO", "Top 10 industry adjustments (2025 tariff rates):")
  top_adj <- industry_adjustment_valid %>%
    arrange(desc(adjustment_2025)) %>%
    head(10)

  for (i in 1:nrow(top_adj)) {
    log_msg("INFO", paste("  ", top_adj$bea_code_j[i], ":",
                          round(top_adj$adjustment_2025[i] * 100, 4), "%"))
  }

  log_msg("INFO", "Industry adjustment factors calculated")

} else {
  log_msg("ERROR", "Cannot calculate industry adjustment - missing output data")
  industry_adjustment <- NULL
}

# ==============================================================================
# SECTION 9B: LEONTIEF-ADJUSTED INDUSTRY WEIGHTS
# ==============================================================================
# Formula: w_j = sum_c(tau_c * R_{c,j} * s_c)
#
# Where R_{c,j} is from the Total Requirements matrix (Leontief inverse) and
# s_c is the direct import share. This captures tariff cost pressure at every
# tier of the supply chain, not just direct imported inputs.
# ==============================================================================

if (total_req_available && !is.null(y_j_data)) {

  log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
  log_msg("INFO", "Step 8B: Computing Leontief-Adjusted Industry Weights")
  log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

  # --- 7B(a): Build tau vector aligned to R_matrix rows ---
  # Tariff rates are at BEA commodity level. The TR matrix uses original BEA codes
  # including 3361MV and 3364OT (not aggregated 336).
  tau_for_leontief <- tau_c %>%
    dplyr::select(bea_code, tau_c_2025, tau_c_recent)

  # Expand 336 -> 3361MV, 3364OT
  tau_336 <- tau_for_leontief %>% filter(bea_code == "336")
  if (nrow(tau_336) > 0) {
    tau_expanded <- bind_rows(
      tau_for_leontief %>% filter(bea_code != "336"),
      tau_336 %>% mutate(bea_code = "3361MV"),
      tau_336 %>% mutate(bea_code = "3364OT")
    )
  } else {
    tau_expanded <- tau_for_leontief
  }

  # Align to R_matrix row codes
  tau_aligned <- data.frame(bea_code = rownames(R_matrix), stringsAsFactors = FALSE) %>%
    left_join(tau_expanded, by = "bea_code") %>%
    mutate(
      tau_c_2025 = replace_na(tau_c_2025, 0),
      tau_c_recent = replace_na(tau_c_recent, 0)
    )

  tau_vec_2025 <- tau_aligned$tau_c_2025
  tau_vec_recent <- tau_aligned$tau_c_recent

  log_msg("INFO", paste("  Tariff vector: matched", sum(tau_vec_2025 > 0), "of",
                         length(tau_vec_2025), "commodities"))

  # --- 7B(b): Build import share vector aligned to R_matrix rows ---
  s_aligned <- data.frame(bea_code = rownames(R_matrix), stringsAsFactors = FALSE) %>%
    left_join(
      import_shares_emp %>% dplyr::select(bea_code, import_share),
      by = "bea_code"
    ) %>%
    mutate(import_share = replace_na(import_share, 0))

  s_vec <- s_aligned$import_share

  log_msg("INFO", paste("  Import share vector: matched", sum(s_vec > 0), "of",
                         length(s_vec), "commodities"))

  # --- 7B(c): Compute Leontief-adjusted weights ---
  # w_j = sum_c(tau_c * R_{c,j} * s_c)
  # In matrix form: w = t(R) %*% (tau * s)
  w_leontief_2025 <- as.numeric(t(R_matrix) %*% (tau_vec_2025 * s_vec))
  w_leontief_recent <- as.numeric(t(R_matrix) %*% (tau_vec_recent * s_vec))

  leontief_weights_raw <- data.frame(
    bea_code_j = colnames(R_matrix),
    leontief_adj_2025 = w_leontief_2025,
    leontief_adj_recent = w_leontief_recent,
    stringsAsFactors = FALSE
  )

  log_msg("INFO", paste("  Computed Leontief weights for", nrow(leontief_weights_raw), "industries"))

  # --- 7B(d): Aggregate 3361MV + 3364OT -> 336 ---
  # Use output-weighted average since employment data is at 336 level
  y_j_for_agg <- y_j_data %>%
    filter(bea_code_j %in% c("3361MV", "3364OT")) %>%
    dplyr::select(bea_code_j, y_j)

  mv_codes <- c("3361MV", "3364OT")
  mv_weights <- leontief_weights_raw %>% filter(bea_code_j %in% mv_codes)

  if (nrow(mv_weights) == 2 && nrow(y_j_for_agg) == 2) {
    mv_merged <- mv_weights %>%
      left_join(y_j_for_agg, by = "bea_code_j")

    total_y <- sum(mv_merged$y_j, na.rm = TRUE)
    if (total_y > 0) {
      w_336_2025 <- sum(mv_merged$leontief_adj_2025 * mv_merged$y_j) / total_y
      w_336_recent <- sum(mv_merged$leontief_adj_recent * mv_merged$y_j) / total_y
    } else {
      w_336_2025 <- mean(mv_merged$leontief_adj_2025)
      w_336_recent <- mean(mv_merged$leontief_adj_recent)
    }

    leontief_weights <- leontief_weights_raw %>%
      filter(!bea_code_j %in% mv_codes) %>%
      bind_rows(data.frame(
        bea_code_j = "336",
        leontief_adj_2025 = w_336_2025,
        leontief_adj_recent = w_336_recent,
        stringsAsFactors = FALSE
      ))

    log_msg("INFO", paste("  Aggregated 3361MV + 3364OT -> 336:",
                           "w_2025 =", round(w_336_2025, 6),
                           ", w_recent =", round(w_336_recent, 6)))
  } else {
    leontief_weights <- leontief_weights_raw
    log_msg("WARN", "  Could not aggregate 3361MV + 3364OT (codes not found in both sources)")
  }

  log_msg("INFO", paste("  Final Leontief weights:", nrow(leontief_weights), "industries"))

  # Display top Leontief weights
  log_msg("INFO", "Top 10 Leontief-adjusted weights (2025 tariff rates):")
  top_leon <- leontief_weights %>%
    arrange(desc(leontief_adj_2025)) %>%
    head(10)
  for (i in 1:nrow(top_leon)) {
    log_msg("INFO", paste("  ", top_leon$bea_code_j[i], ":",
                          round(top_leon$leontief_adj_2025[i] * 100, 4), "%"))
  }

  leontief_available <- TRUE

} else {
  if (!total_req_available) {
    log_msg("WARN", "Total Requirements matrix not available - skipping Leontief adjustment")
  }
  if (is.null(y_j_data)) {
    log_msg("WARN", "Industry output data not available - skipping Leontief adjustment")
  }
  leontief_available <- FALSE
}

# ==============================================================================
# SECTION 10: SUMMARY AND OUTPUT
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Summary")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# Save tariff rates by BEA code (used by report for consumer-goods passthrough)
tryCatch(
  write_csv(tau_c, file.path(OUTPUT_DIR, "emp_idx_tau_c.csv")),
  error = function(e) log_msg("WARN", paste("Could not save emp_idx_tau_c.csv:", e$message))
)
log_msg("INFO", "Saved emp_idx_tau_c.csv")

if (!is.null(industry_adjustment)) {
  log_msg("INFO", "")
  log_msg("INFO", "Employment Index Components Ready:")
  log_msg("INFO", paste("  - Tariff rates (tau_c):", nrow(tau_c), "commodity groups"))
  log_msg("INFO", paste("  - Import matrix (x_cj):", nrow(x_cj_with_tariff), "commodity-industry pairs"))
  log_msg("INFO", paste("  - Industry output (y_j):", nrow(y_j_adjusted), "industries"))
  log_msg("INFO", paste("  - Industry adjustments:", nrow(industry_adjustment_valid), "valid industries"))
  log_msg("INFO", "")
  log_msg("INFO", "Proceeding to merge with employment data (E_jt) to calculate final index I_t")
}

# ==============================================================================
# SECTION 11: LOAD EMPLOYMENT DATA (E_jt)
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 9: Loading Employment by Industry (E_jt) from Haver")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# Employment Crosswalk: BEA industry -> Haver codes/formulas
# Some industries require formulas (additions/subtractions of multiple series)
e_jt_crosswalk <- tribble(
  ~bea_industry, ~haver_formula, ~description, ~var_name,

  # Agriculture & Mining
  "113FF", "LAA33A@LABOR", "Logging", "logging",
  "211", "LAB1A@LABOR", "Oil & Gas Extraction", "oil_gas_extraction",
  "212", "LAB2A@LABOR", "Mining, except Oil & Gas", "mining_ex_oil_gas",
  "213", "LAB3A@LABOR", "Support Activities for Mining", "support_mining",

  # Utilities & Construction
  "22", "LAUTILA@LABOR", "Utilities", "utilities",
  "23", "LACONSA@LABOR", "Construction", "construction",

  # Manufacturing - Durable
  "321", "LAF1A@LABOR", "Wood Products", "wood_products",
  "327", "LAF7A@LABOR", "Nonmetallic Mineral Products", "nonmetallic_minerals",
  "331", "LAG1A@LABOR", "Primary Metals", "primary_metals",
  "332", "LAG2A@LABOR", "Fabricated Metal Products", "fabricated_metals",
  "333", "LAG3A@LABOR", "Machinery", "machinery",
  "334", "LAG4A@LABOR", "Computer & Electronic Products", "computer_electronic",
  "335", "LAG5A@LABOR", "Electrical Equipment & Appliances", "electrical_equipment",
  "336", "LAG6A@LABOR", "Transportation Equipment", "motor_vehicles",
  "337", "LAG7A@LABOR", "Furniture & Related Products", "furniture",
  "339", "LAG9A@LABOR", "Misc Durable Goods Manufacturing", "misc_durable_mfg",

  # Manufacturing - Nondurable
  "311FT", "LAE1A@LABOR+LAE26A@LABOR", "Food + Misc Nondurable Manufacturing", "food_misc_nondurable",
  "313TT", "LAE3A@LABOR+LAE4A@LABOR", "Textile Mills + Textile Products", "textiles",
  "315AL", "LAE5A@LABOR", "Apparel", "apparel",
  "322", "LAF2A@LABOR", "Paper & Paper Products", "paper_products",
  "323", "LAF3A@LABOR", "Printing & Related Support", "printing",
  "324", "LAF4A@LABOR", "Petroleum & Coal Products", "petroleum_coal",
  "325", "LAF5A@LABOR", "Chemicals", "chemicals",
  "326", "LAF6A@LABOR", "Plastics & Rubber Products", "plastics_rubber",

  # Trade
  "42", "LAWTRDA@LABOR", "Wholesale Trade", "wholesale_trade",
  "441", "LAI1A@LABOR", "Motor Vehicle & Parts Dealers", "auto_dealers",
  "445", "LAI5A@LABOR", "Food & Beverage Stores", "food_bev_stores",
  "452", "LAJ5A@LABOR", "General Merchandise Retailers", "general_merch",
  "4A0", "LARTRDA@LABOR-LAI1A@LABOR-LAI5A@LABOR-LAJ5A@LABOR", "Other Retail", "other_retail",

  # Transportation & Warehousing
  "481", "LAK1A@LABOR", "Air Transportation", "air_transport",
  "482", "LAK2A@LABOR", "Rail Transportation", "rail_transport",
  "483", "LAK3A@LABOR", "Water Transportation", "water_transport",
  "484", "LAK4A@LABOR", "Truck Transportation", "truck_transport",
  "485", "LAK5A@LABOR", "Transit & Ground Passenger", "transit_ground",
  "486", "LAK6A@LABOR", "Pipeline Transportation", "pipeline_transport",
  "487OS", "LAK7A@LABOR+LAK8A@LABOR+LAL2A@LABOR", "Other Transport + Support + Couriers", "other_transport_support",
  "493", "LAL3A@LABOR", "Warehousing & Storage", "warehousing",

  # Information
  "511", "LAM3A@LABOR", "Publishing Industries", "publishing",
  "512", "LAM2A@LABOR", "Motion Picture & Sound Recording", "motion_picture",
  "513", "LAM6A@LABOR+LAM7A@LABOR", "Broadcasting + Telecommunications", "broadcasting_telecom",
  "514", "LAINFOA@LABOR-LAM2A@LABOR-LAM3A@LABOR-LAM6A@LABOR-LAM7A@LABOR", "Data Processing & Internet", "data_processing",

  # Finance
  "521CI", "LAN1A@LABOR", "Credit Intermediation", "credit_intermediation",
  "523", "LAN3A@LABOR-LAN399A@LABOR", "Securities & Commodities (excl Funds/Trusts)", "securities",
  "524", "LAN4A@LABOR", "Insurance Carriers & Related", "insurance",
  "525", "LAN399A@LABOR", "Funds, Trusts & Other Financial", "funds_trusts",

  # Real Estate & Rental
  # Note: HS and ORE both map to LAO1A@LABOR (total real estate employment)
  # because BLS does not split real estate employment the way BEA splits output.
  # The industry adjustment factors for HS vs ORE differ, so both are needed.
  "HS", "LAO1A@LABOR", "Real Estate (proxy for Housing)", "housing",
  "ORE", "LAO1A@LABOR", "Real Estate (Other)", "other_real_estate",
  "532RL", "LAO2A@LABOR", "Rental & Leasing Services", "rental_leasing",

  # Professional Services
  "5411", "LAP11@USECON", "Legal Services", "legal_services",
  "5415", "LAP15@LABOR", "Computer Systems Design", "computer_systems_design",
  "5412OP", "LAPBSV@LABOR-LAP11@USECON-LAP15@LABOR", "Other Professional Services", "misc_professional",

  # Management & Administrative
  "55", "LAMGSVA@LABOR", "Management of Companies", "management_companies",
  "561", "LAR1A@LABOR", "Administrative & Support Services", "admin_support",
  "562", "LAR2A@LABOR", "Waste Management & Remediation", "waste_management",

  # Education & Health
  "61", "LAEDUCA@LABOR", "Educational Services", "education",
  "621", "LAT1A@LABOR", "Ambulatory Health Care", "ambulatory_health",
  "622", "LAT2A@LABOR", "Hospitals", "hospitals",
  "623", "LAT3A@LABOR", "Nursing & Residential Care", "nursing_care",
  "624", "LAT4A@LABOR", "Social Assistance", "social_assistance",

  # Arts, Entertainment, Recreation
  "711AS", "LAU1A@LABOR+LAU2A@LABOR", "Arts, Entertainment & Recreation", "arts_sports",
  "713", "LAU3A@LABOR", "Amusements, Gambling & Recreation", "amusements",

  # Accommodation & Food
  "721", "LAV1A@LABOR", "Accommodation", "accommodation",
  "722", "LAV2A@LABOR", "Food Services & Drinking Places", "food_services",

  # Other Services
  "81", "LASRVOA@LABOR", "Other Services", "other_services"
)

log_msg("INFO", paste("Built employment crosswalk with", nrow(e_jt_crosswalk), "industries"))

# Extract all unique Haver codes needed from formulas (case-insensitive pattern)
all_e_codes <- unique(unlist(str_extract_all(
  e_jt_crosswalk$haver_formula,
  "[A-Za-z0-9]+@[A-Za-z]+"
)))

log_msg("INFO", paste("Need to pull", length(all_e_codes), "unique Haver codes for employment"))

# Pull employment data from Haver
if (haver_available && length(all_e_codes) > 0) {

  # First, validate which codes are available using shared utility
  log_msg("INFO", "Validating Haver codes...")
  validation_result <- validate_haver_codes(all_e_codes, test_start = "2024-01-01", log_fn = log_msg)
  valid_codes <- validation_result$valid
  invalid_codes <- validation_result$invalid

  # Pull only valid codes
  if (length(valid_codes) > 0) {
    e_jt_raw <- pull_haver_local(valid_codes, "monthly", "1996-01-01")
  } else {
    log_msg("ERROR", "No valid Haver codes found")
    e_jt_raw <- NULL
  }

  if (!is.null(e_jt_raw)) {
    log_msg("INFO", paste("Pulled employment data:", nrow(e_jt_raw), "rows,",
                          ncol(e_jt_raw) - 1, "series"))

    # Start with raw data
    e_jt_data <- e_jt_raw

    # Calculate each industry variable from the crosswalk formulas
    for (i in 1:nrow(e_jt_crosswalk)) {
      formula <- e_jt_crosswalk$haver_formula[i]
      var_name <- e_jt_crosswalk$var_name[i]
      bea_ind <- e_jt_crosswalk$bea_industry[i]

      # Convert Haver formula to R formula (lowercase column names, remove @DATABASE)
      # Pattern handles both upper and lower case input
      formula_r <- formula %>%
        str_replace_all("([A-Za-z0-9]+)@[A-Za-z]+", function(m) {
          str_to_lower(str_extract(m, "^[A-Za-z0-9]+"))
        })

      tryCatch({
        e_jt_data <- e_jt_data %>%
          mutate(!!var_name := eval(parse(text = formula_r)))
        log_msg("INFO", paste("  [", bea_ind, "]", var_name, "=", formula_r))
      }, error = function(e) {
        log_msg("WARN", paste("  Failed:", var_name, "-", e$message))
      })
    }

    # Keep only the calculated industry variables plus date
    industry_vars <- e_jt_crosswalk$var_name
    e_jt_final <- e_jt_data %>%
      dplyr::select(date, all_of(intersect(industry_vars, names(e_jt_data))))

    log_msg("INFO", paste("Final employment data:", nrow(e_jt_final), "rows,",
                          ncol(e_jt_final) - 1, "industries"))

    # Save employment data (used as cache for re-runs)
    tryCatch(
      write_csv(e_jt_final, file.path(OUTPUT_DIR, "e_jt_employment_index.csv")),
      error = function(e) log_msg("WARN", paste("Could not save e_jt_employment_index.csv:", e$message))
    )
    log_msg("INFO", "Saved e_jt_employment_index.csv")

  } else {
    log_msg("WARN", "Could not pull employment data from Haver")
    e_jt_final <- NULL
  }
} else {
  log_msg("WARN", "Haver not available for employment data")

  # Try to load from existing file
  e_jt_file <- file.path(OUTPUT_DIR, "e_jt_employment_index.csv")
  if (file.exists(e_jt_file)) {
    log_msg("INFO", "Loading from existing e_jt_employment_index.csv")
    e_jt_final <- read_csv(e_jt_file, show_col_types = FALSE)
    log_msg("INFO", paste("Loaded employment data:", nrow(e_jt_final), "rows"))
  } else {
    log_msg("ERROR", "No employment data available")
    e_jt_final <- NULL
  }
}

# ==============================================================================
# SECTION 12: CALCULATE FINAL INDEX I_t
# ==============================================================================

log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Step 10: Calculating Final Employment Index I_t")
log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

# Primary (Leontief): I_t = sum_j E_{j,t} * w_j  where w_j = sum_c(tau_c * R_{c,j} * s_c)
# Reference (direct):  I_t = sum_j E_{j,t} * (sum_c tau_c * x_{c,j}) / y_j

if (!is.null(e_jt_final) && !is.null(industry_adjustment)) {

  # Reshape employment data to long format for merging
  e_jt_long <- e_jt_final %>%
    pivot_longer(
      cols = -date,
      names_to = "var_name",
      values_to = "employment"
    )

  # Join with crosswalk to get BEA codes
  e_jt_long <- e_jt_long %>%
    left_join(
      e_jt_crosswalk %>% dplyr::select(bea_industry, var_name),
      by = "var_name"
    ) %>%
    filter(!is.na(bea_industry))

  log_msg("INFO", paste("Employment data (long):", nrow(e_jt_long), "rows"))

  # Rename bea_industry to bea_code_j for consistency
  e_jt_adjusted <- e_jt_long %>%
    rename(bea_code_j = bea_industry)

  log_msg("INFO", paste("Employment data:", nrow(e_jt_adjusted), "date-industry pairs"))

  # Get the direct industry adjustments (from Section 9)
  adj_direct <- industry_adjustment_valid %>%
    dplyr::select(bea_code_j, adjustment_2025, adjustment_recent)

  # Merge employment with direct industry adjustments
  index_components <- e_jt_adjusted %>%
    left_join(adj_direct, by = "bea_code_j")

  # If Leontief weights are available, merge them too
  if (leontief_available) {
    index_components <- index_components %>%
      left_join(
        leontief_weights %>% dplyr::select(bea_code_j, leontief_adj_2025, leontief_adj_recent),
        by = "bea_code_j"
      )
  }

  # Direct contributions (reference)
  index_components <- index_components %>%
    mutate(
      contribution_2025_direct = employment * adjustment_2025,
      contribution_recent_direct = employment * adjustment_recent
    )

  # Leontief contributions (primary) or fall back to direct
  if (leontief_available) {
    index_components <- index_components %>%
      mutate(
        contribution_2025 = employment * replace_na(leontief_adj_2025, 0),
        contribution_recent = employment * replace_na(leontief_adj_recent, 0)
      )
  } else {
    index_components <- index_components %>%
      mutate(
        contribution_2025 = contribution_2025_direct,
        contribution_recent = contribution_recent_direct
      )
  }

  # Check for unmatched industries
  unmatched_emp <- index_components %>%
    filter(is.na(adjustment_2025)) %>%
    distinct(bea_code_j)

  if (nrow(unmatched_emp) > 0) {
    log_msg("WARN", paste("Industries without adjustment factors:", nrow(unmatched_emp)))
    log_msg("WARN", paste("  ", paste(unmatched_emp$bea_code_j, collapse = ", ")))
  }

  # Filter to matched industries only
  index_components_valid <- index_components %>%
    filter(!is.na(adjustment_2025))

  log_msg("INFO", paste("Valid index components:", length(unique(index_components_valid$bea_code_j)), "industries"))

  # Calculate final index I_t = sum_j(E_jt * w_j)
  # Primary columns use Leontief weights; _direct columns use direct weights
  employment_index <- index_components_valid %>%
    group_by(date) %>%
    summarize(
      total_employment = sum(employment, na.rm = TRUE),
      I_t_2025 = sum(contribution_2025, na.rm = TRUE),
      I_t_recent = sum(contribution_recent, na.rm = TRUE),
      I_t_2025_direct = sum(contribution_2025_direct, na.rm = TRUE),
      I_t_recent_direct = sum(contribution_recent_direct, na.rm = TRUE),
      n_industries = n(),
      .groups = "drop"
    ) %>%
    mutate(
      year = year(date),
      month = month(date),
      I_t_2025_pct = I_t_2025 / total_employment * 100,
      I_t_recent_pct = I_t_recent / total_employment * 100,
      I_t_2025_direct_pct = I_t_2025_direct / total_employment * 100,
      I_t_recent_direct_pct = I_t_recent_direct / total_employment * 100
    )

  log_msg("INFO", paste("Calculated employment index for", nrow(employment_index), "months"))

  # Display recent index values
  log_msg("INFO", "")
  log_msg("INFO", "Employment Index I_t (most recent 12 months):")
  log_msg("INFO", "  Using 2025 average tariff rates:")
  recent_idx <- employment_index %>%
    arrange(desc(date)) %>%
    head(12)

  for (i in 1:min(6, nrow(recent_idx))) {
    log_msg("INFO", paste("  ", format(recent_idx$date[i], "%Y-%m"), ":",
                          format(round(recent_idx$I_t_2025[i], 0), big.mark = ","),
                          "(", round(recent_idx$I_t_2025_pct[i], 3), "% of total emp)"))
  }

  # Save index data
  tryCatch(
    write_csv(employment_index, file.path(OUTPUT_DIR, "employment_index_I_t.csv")),
    error = function(e) log_msg("WARN", paste("Could not save employment_index_I_t.csv:", e$message))
  )
  log_msg("INFO", "")
  log_msg("INFO", "Saved employment_index_I_t.csv")

  # Summary statistics
  log_msg("INFO", "")
  log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))
  log_msg("INFO", "EMPLOYMENT INDEX SUMMARY")
  log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))

  latest <- employment_index %>% filter(date == max(date))
  log_msg("INFO", paste("Latest date:", format(latest$date, "%Y-%m")))
  log_msg("INFO", paste("Total employment covered:", format(round(latest$total_employment, 0), big.mark = ",")))
  log_msg("INFO", paste("Industries in index:", latest$n_industries))
  log_msg("INFO", "")
  log_msg("INFO", "Index values (tariff-weighted employment):")
  log_msg("INFO", paste("  I_t (2025 avg rates):", format(round(latest$I_t_2025, 0), big.mark = ","),
                        "=", round(latest$I_t_2025_pct, 3), "% of covered employment"))
  log_msg("INFO", paste("  I_t (recent rates):  ", format(round(latest$I_t_recent, 0), big.mark = ","),
                        "=", round(latest$I_t_recent_pct, 3), "% of covered employment"))

  # ============================================================================
  # SECTION 12B: CALCULATE MANUFACTURING-ONLY INDEX
  # ============================================================================

  log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))
  log_msg("INFO", "Step 11: Calculating Manufacturing-Only Employment Index")
  log_msg("INFO", "-" %>% rep(60) %>% paste(collapse = ""))

  # Define manufacturing BEA codes
  mfg_codes <- c(
    # Durable goods manufacturing
    "321",    # Wood Products
    "327",    # Nonmetallic Mineral Products
    "331",    # Primary Metals
    "332",    # Fabricated Metal Products
    "333",    # Machinery
    "334",    # Computer & Electronic Products
    "335",    # Electrical Equipment & Appliances
    "336",    # Transportation Equipment
    "337",    # Furniture & Related Products
    "339",    # Misc Durable Goods Manufacturing
    # Nondurable goods manufacturing
    "311FT",  # Food + Misc Nondurable Manufacturing
    "313TT",  # Textile Mills + Textile Products
    "315AL",  # Apparel
    "322",    # Paper & Paper Products
    "323",    # Printing & Related Support
    "324",    # Petroleum & Coal Products
    "325",    # Chemicals
    "326"     # Plastics & Rubber Products
  )

  # Filter to manufacturing industries only
  index_components_mfg <- index_components_valid %>%
    filter(bea_code_j %in% mfg_codes)

  log_msg("INFO", paste("Manufacturing industries:", length(unique(index_components_mfg$bea_code_j))))

  # Calculate manufacturing index (Leontief primary, direct reference)
  employment_index_mfg <- index_components_mfg %>%
    group_by(date) %>%
    summarize(
      total_employment = sum(employment, na.rm = TRUE),
      I_t_2025 = sum(contribution_2025, na.rm = TRUE),
      I_t_recent = sum(contribution_recent, na.rm = TRUE),
      I_t_2025_direct = sum(contribution_2025_direct, na.rm = TRUE),
      I_t_recent_direct = sum(contribution_recent_direct, na.rm = TRUE),
      n_industries = n(),
      .groups = "drop"
    ) %>%
    mutate(
      year = year(date),
      month = month(date),
      I_t_2025_pct = I_t_2025 / total_employment * 100,
      I_t_recent_pct = I_t_recent / total_employment * 100,
      I_t_2025_direct_pct = I_t_2025_direct / total_employment * 100,
      I_t_recent_direct_pct = I_t_recent_direct / total_employment * 100
    )

  log_msg("INFO", paste("Calculated manufacturing index for", nrow(employment_index_mfg), "months"))

  # Display manufacturing summary
  latest_mfg <- employment_index_mfg %>% filter(date == max(date))
  log_msg("INFO", "")
  log_msg("INFO", "Manufacturing Index Summary:")
  log_msg("INFO", paste("  Manufacturing employment:", format(round(latest_mfg$total_employment, 0), big.mark = ",")))
  log_msg("INFO", paste("  I_t (2025 avg rates):", format(round(latest_mfg$I_t_2025, 0), big.mark = ","),
                        "=", round(latest_mfg$I_t_2025_pct, 3), "% of mfg employment"))
  log_msg("INFO", paste("  I_t (recent rates):  ", format(round(latest_mfg$I_t_recent, 0), big.mark = ","),
                        "=", round(latest_mfg$I_t_recent_pct, 3), "% of mfg employment"))

  # Save manufacturing index
  tryCatch(
    write_csv(employment_index_mfg, file.path(OUTPUT_DIR, "employment_index_I_t_manufacturing.csv")),
    error = function(e) log_msg("WARN", paste("Could not save employment_index_I_t_manufacturing.csv:", e$message))
  )
  log_msg("INFO", "Saved employment_index_I_t_manufacturing.csv")

} else {
  log_msg("ERROR", "Cannot calculate employment index - missing employment or adjustment data")
  employment_index <- NULL
}

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

log_msg("INFO", "")
log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "OUTPUT FILES CREATED")
log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "")
log_msg("INFO", "Output files:")
log_msg("INFO", "  - emp_idx_tau_c.csv (tariff rates by BEA commodity)")
log_msg("INFO", "  - e_jt_employment_index.csv (employment by industry)")
log_msg("INFO", "  - employment_index_I_t.csv (final index time series)")
log_msg("INFO", "  - employment_index_I_t_manufacturing.csv (manufacturing-only index)")
log_msg("INFO", "")
log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "Employment Index Script Complete")
log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))
