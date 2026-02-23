# ==============================================================================
# Imported PCE Goods Price Index
# ==============================================================================
#
# Purpose: Construct import-content-weighted PCE goods price indices
#
# Formula:
#   I_t = sum_k w_{k,t} * P_{k,t}
#
# Where:
#   - P_{k,t}: BEA chain-type price index for PCE category k
#   - w_{k,t}: Time-varying weight based on import content and spending
#   - w_{k,t} = s_k * E_{k,t} / sum_j(s_j * E_{j,t})
#   - s_k: Import content share for PCE category k
#   - E_{k,t}: Nominal PCE expenditure on category k
#
# Import content shares (s_k) are computed from:
#   1. BEA Import Matrix (imports by commodity x industry, 2024)
#   2. BEA Use Table (total commodity use, 2024)
#   3. BEA PCE Bridge Table (commodity -> PCE category mapping, 2024)
#
# Three variants: All goods, Core goods (ex food/energy), Durables only
#
# Author: John Iselin, Yale Budget Lab
# Date: February 2026
# ==============================================================================

library(Haver)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(here)
library(stringr)

here::i_am("R/import_price_index.R")

# Source shared utilities
source(here("R", "utils.R"))

INPUT_DIR  <- here("input")
OUTPUT_DIR <- here("output")
LOG_DIR    <- here("logs")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)

# Create log file and get logging function
IPI_LOG_FILE <- file.path(LOG_DIR, paste0("import_price_index_",
                                           format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_msg <- create_logger(IPI_LOG_FILE)

log_msg("INFO", "Libraries loaded successfully")

# Initialize Haver connection
haver_available <- check_haver_available()

# Wrapper for pull_haver
pull_haver_local <- function(codes, frequency = "monthly", start_date, end_date = NULL) {
  result <- pull_haver(codes, frequency, start_date, end_date,
                       haver_available = haver_available, log_fn = log_msg)
  if (!is.null(result)) {
    result <- normalize_monthly_dates(result)
  }
  result
}

# safe_divide() is defined in utils.R (sourced above)

log_msg("INFO", paste(rep("=", 60), collapse = ""))
log_msg("INFO", "Imported PCE Goods Price Index")
log_msg("INFO", paste(rep("=", 60), collapse = ""))

# ==============================================================================
# SECTION 1: DEFINE 32 PCE GOODS CATEGORIES
# ==============================================================================

log_msg("INFO", paste(rep("-", 60), collapse = ""))
log_msg("INFO", "Step 1: Defining PCE Goods Categories")
log_msg("INFO", paste(rep("-", 60), collapse = ""))

# 32 PCE goods categories with Haver codes, matching NIPA Table 2.4.5U
pce_goods <- tribble(
  ~haver_exp,      ~haver_price,      ~category,                                                             ~type,        ~is_food_energy,
  "CDMVNM@USNA",  "JCDMVNM@USNA",   "New motor vehicles",                                                  "durable",    FALSE,
  "CDMVUM@USNA",  "JCDMVUM@USNA",   "Net purchases of used motor vehicles",                                 "durable",    FALSE,
  "CDMTM@USNA",   "JCDMTM@USNA",    "Motor vehicle parts and accessories",                                  "durable",    FALSE,
  "CDFFM@USNA",   "JCDFFM@USNA",    "Furniture and furnishings",                                            "durable",    FALSE,
  "CDFKM@USNA",   "JCDFKM@USNA",    "Household appliances",                                                "durable",    FALSE,
  "CDFGM@USNA",   "JCDFGM@USNA",    "Glassware, tableware, and household utensils",                         "durable",    FALSE,
  "CDFOSM@USNA",  "JCDFOSM@USNA",   "Tools and hardware for house and garden",                              "durable",    FALSE,
  "CDRGTM@USNA",  "JCDRGTM@USNA",   "Video, audio, photographic, and information processing equipment",     "durable",    FALSE,
  "CDRSM@USNA",   "JCDRSM@USNA",    "Sporting equipment, supplies, guns, and ammunition",                   "durable",    FALSE,
  "CDRSVM@USNA",  "JCDRSVM@USNA",   "Sports and recreational vehicles",                                     "durable",    FALSE,
  "CDRBM@USNA",   "JCDRBM@USNA",    "Recreational books",                                                   "durable",    FALSE,
  "CDFTIM@USNA",  "JCDFTIM@USNA",   "Musical instruments",                                                  "durable",    FALSE,
  "CDOJM@USNA",   "JCDOJM@USNA",    "Jewelry and watches",                                                  "durable",    FALSE,
  "CDOOM@USNA",   "JCDOOM@USNA",    "Therapeutic appliances and equipment",                                  "durable",    FALSE,
  "CDEBM@USNA",   "JCDEBM@USNA",    "Educational books",                                                    "durable",    FALSE,
  "CDOLM@USNA",   "JCDOLM@USNA",    "Luggage and similar personal items",                                   "durable",    FALSE,
  "CDOTM@USNA",   "JCDOTM@USNA",    "Telephone and related communication equipment",                        "durable",    FALSE,
  "CNFOBM@USNA",  "JCNFOBM@USNA",  "Food and nonalcoholic beverages purchased for off-premises consumption","nondurable",  TRUE,
  "CNFOLM@USNA",  "JCNFOLM@USNA",  "Alcoholic beverages purchased for off-premises consumption",            "nondurable",  TRUE,
  "CNFEFM@USNA",  "JCNFEFM@USNA",  "Food produced and consumed on farms",                                   "nondurable",  TRUE,
  "CNLFFM@USNA",  "JCNLFFM@USNA",  "Women's and girls' clothing",                                          "nondurable",  FALSE,
  "CNLMFM@USNA",  "JCNLMFM@USNA",  "Men's and boys' clothing",                                             "nondurable",  FALSE,
  "CNLFIM@USNA",  "JCNLFIM@USNA",   "Children's and infants' clothing",                                     "nondurable",  FALSE,
  "CNLOM@USNA",   "JCNLOM@USNA",    "Other clothing materials and footwear",                                "nondurable",  FALSE,
  "CNGM@USNA",    "JCNGM@USNA",     "Motor vehicle fuels, lubricants, and fluids",                          "nondurable",  TRUE,
  "CNOFM@USNA",   "JCNOFM@USNA",    "Fuel oil and other fuels",                                            "nondurable",  TRUE,
  "CNODM@USNA",   "JCNODM@USNA",    "Pharmaceutical and other medical products",                            "nondurable",  FALSE,
  "CNRM@USNA",    "JCNRM@USNA",     "Recreational items",                                                   "nondurable",  FALSE,
  "CNOLM@USNA",   "JCNOLM@USNA",    "Household supplies",                                                   "nondurable",  FALSE,
  "CNOPM@USNA",   "JCNOPM@USNA",    "Personal care products",                                               "nondurable",  FALSE,
  "CNOTM@USNA",   "JCNOTM@USNA",    "Tobacco",                                                              "nondurable",  FALSE,
  "CNMOM@USNA",   "JCNMOM@USNA",    "Magazines, newspapers, and stationery",                                "nondurable",  FALSE
)

# Aggregate price indices for comparison
aggregate_price_codes <- c("JCTGM@USNA", "JCGXFEM@USNA", "JCDM@USNA")

log_msg("INFO", paste("Defined", nrow(pce_goods), "PCE goods categories"))
log_msg("INFO", paste("  Durables:", sum(pce_goods$type == "durable")))
log_msg("INFO", paste("  Nondurables:", sum(pce_goods$type == "nondurable")))
log_msg("INFO", paste("  Food/energy:", sum(pce_goods$is_food_energy)))

# ==============================================================================
# SECTION 2: COMPUTE COMMODITY-LEVEL IMPORT SHARES
# ==============================================================================

log_msg("INFO", paste(rep("-", 60), collapse = ""))
log_msg("INFO", "Step 2: Computing Commodity-Level Import Shares")
log_msg("INFO", paste(rep("-", 60), collapse = ""))

# --- 2a: Read Import Matrix (2024) ---
import_matrix_file <- file.path(INPUT_DIR,
  "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx")
log_msg("INFO", paste("Reading Import Matrix:", basename(import_matrix_file)))

# Read the import matrix - data starts at row 7 (skip 6 rows for header)
# Columns A and B are commodity codes/names, row 7 has industry names
import_raw <- read_excel(import_matrix_file, sheet = "Table",
                          range = "A7:CQ80", col_names = TRUE)

log_msg("INFO", paste("Read import matrix:", nrow(import_raw), "rows x", ncol(import_raw), "cols"))

# Validate dimensions (BEA Summary import matrix should have ~73 rows x ~95 cols)
if (nrow(import_raw) < 70) log_msg("WARN", paste("Fewer rows than expected:", nrow(import_raw), "vs ~73"))
if (ncol(import_raw) < 90) log_msg("WARN", paste("Fewer cols than expected:", ncol(import_raw), "vs ~95"))

# Get column names - first two are commodity code and name
col_names <- names(import_raw)
log_msg("INFO", paste("First 5 columns:", paste(col_names[1:5], collapse = ", ")))

# Rename first two columns for clarity
names(import_raw)[1:2] <- c("bea_code", "bea_name")

# Replace "---" and any non-numeric values with 0
data_cols <- names(import_raw)[3:ncol(import_raw)]
for (col in data_cols) {
  import_raw[[col]] <- as.character(import_raw[[col]])
  import_raw[[col]] <- ifelse(import_raw[[col]] == "---" | is.na(import_raw[[col]]), "0", import_raw[[col]])
  import_raw[[col]] <- as.numeric(import_raw[[col]])
}

log_msg("INFO", "Replaced '---' with 0s")

# Total imports per commodity: use "Imports of goods and services" column (col 82).
# In the BEA Import Matrix, this column shows total imports as a negative value
# (imports are subtracted in the GDP accounting identity). Take the absolute value.
# Note: Total Intermediate + Total Final Uses = 0 by construction, so those
# subtotal columns cannot be summed to get total imports.
imports_col_idx <- which(str_detect(str_to_lower(names(import_raw)),
                                     "^imports of goods"))

if (length(imports_col_idx) > 0) {
  imports_col <- names(import_raw)[imports_col_idx[1]]
  log_msg("INFO", paste("  Imports column:", imports_col, "(index", imports_col_idx[1], ")"))

  import_totals <- import_raw %>%
    dplyr::select(bea_code, bea_name) %>%
    dplyr::mutate(
      total_imports = abs(import_raw[[imports_col]])
    )
} else {
  # Fallback: sum Total Intermediate + individual final demand columns (excl. imports col)
  log_msg("WARN", "Could not find 'Imports of goods and services' column - using row sums")
  total_int_idx <- which(str_detect(str_to_lower(names(import_raw)),
                                     "total.*intermediate"))
  total_final_idx <- which(str_detect(str_to_lower(names(import_raw)),
                                       "total.*final"))
  if (length(total_int_idx) > 0 && length(total_final_idx) > 0) {
    # Sum individual final demand columns (between Total Intermediate and Total Final Uses),
    # excluding the imports column
    fd_cols <- names(import_raw)[(total_int_idx[1] + 1):(total_final_idx[1] - 1)]
    fd_cols <- fd_cols[!str_detect(str_to_lower(fd_cols), "^imports")]
    import_totals <- import_raw %>%
      dplyr::select(bea_code, bea_name) %>%
      dplyr::mutate(
        total_imports = import_raw[[names(import_raw)[total_int_idx[1]]]] +
          rowSums(import_raw[, fd_cols], na.rm = TRUE)
      )
  } else {
    log_msg("WARN", "Could not identify subtotal columns - computing row sums")
    import_totals <- import_raw %>%
      dplyr::select(bea_code, bea_name) %>%
      dplyr::mutate(
        total_imports = rowSums(import_raw[, data_cols], na.rm = TRUE)
      )
  }
}

import_totals <- import_totals %>%
  dplyr::mutate(bea_code = str_trim(as.character(bea_code)))

log_msg("INFO", paste("Computed total imports for", nrow(import_totals), "commodities"))

# --- 2b: Read Use Table (2024) for Total Use of Products ---
use_table_file <- file.path(INPUT_DIR,
  "BEA - The Use of Commodities by Industry - Summary - 2024.xlsx")
log_msg("INFO", paste("Reading Use Table:", basename(use_table_file)))

# Row 6 = headers (industry names), Rows 7-89 = data
use_raw <- read_excel(use_table_file, sheet = "Table",
                       range = "A6:CP89", col_names = TRUE)

log_msg("INFO", paste("Read use table:", nrow(use_raw), "rows x", ncol(use_raw), "cols"))

# Validate dimensions (BEA Summary use table should have ~83 rows x ~94 cols)
if (nrow(use_raw) < 75) log_msg("WARN", paste("Fewer rows than expected:", nrow(use_raw), "vs ~83"))
if (ncol(use_raw) < 85) log_msg("WARN", paste("Fewer cols than expected:", ncol(use_raw), "vs ~94"))

# Rename first two columns
names(use_raw)[1:2] <- c("row_code", "row_name")

# T019 column = last column = "Total use of products"
t019_col_name <- names(use_raw)[ncol(use_raw)]
log_msg("INFO", paste("  Total use column:", t019_col_name))

# Extract commodity rows (exclude summary rows T005, V001, T018, etc.)
use_totals <- use_raw %>%
  dplyr::mutate(
    bea_code = str_trim(as.character(row_code)),
    total_use = suppressWarnings(as.numeric(as.character(.data[[t019_col_name]])))
  ) %>%
  dplyr::filter(
    !is.na(bea_code),
    !str_detect(bea_code, "^T0|^V0|^VA")  # Exclude summary/value-added rows
  ) %>%
  dplyr::select(bea_code, total_use)

log_msg("INFO", paste("Got total use for", nrow(use_totals), "commodities"))

# --- 2c: Compute import shares ---
import_shares <- import_totals %>%
  dplyr::left_join(use_totals, by = "bea_code") %>%
  dplyr::mutate(
    import_share = safe_divide(total_imports, total_use, default = 0)
  )

# Log any commodities with import share > 100% before capping
over_100 <- import_shares %>% dplyr::filter(import_share > 1)
if (nrow(over_100) > 0) {
  log_msg("WARN", paste("Found", nrow(over_100), "commodities with import share > 100%:"))
  for (i in 1:nrow(over_100)) {
    log_msg("WARN", paste("  ", over_100$bea_code[i], "-",
                           str_trunc(over_100$bea_name[i], 40), ":",
                           round(over_100$import_share[i] * 100, 1), "%"))
  }
}
import_shares <- import_shares %>%
  dplyr::mutate(import_share = pmin(import_share, 1))

log_msg("INFO", "Commodity-level import shares computed:")
log_msg("INFO", paste("  Matched:", sum(!is.na(import_shares$total_use)), "of", nrow(import_shares)))
log_msg("INFO", paste("  Mean import share:", round(mean(import_shares$import_share, na.rm = TRUE) * 100, 1), "%"))

# Log top import shares
top_imports <- import_shares %>%
  dplyr::arrange(desc(import_share)) %>%
  dplyr::filter(import_share > 0) %>%
  head(10)

for (i in 1:min(10, nrow(top_imports))) {
  log_msg("INFO", paste("  ", top_imports$bea_code[i], "-",
                         str_trunc(top_imports$bea_name[i], 40), ":",
                         round(top_imports$import_share[i] * 100, 1), "%"))
}

log_msg("INFO", "Commodity import shares calculated")

# ==============================================================================
# SECTION 2b: COMPUTE TOTAL IMPORT CONTENT VIA LEONTIEF INVERSE
# ==============================================================================
#
# The direct import share (s_c = imports_c / total_use_c) captures only the
# direct import content of commodity c. However, domestically-produced
# intermediate inputs also contain imported content. The BEA's Commodity-by-
# Commodity Total Requirements matrix (the Leontief inverse, R) captures all
# rounds of intermediate production.
#
# Total import content of commodity c:
#   total_import_c = Σ_j R(c,j) × m_j
#
# where m_j is the direct import share of commodity j. This follows the
# methodology of Hale et al. (SF Fed Economic Letter, May 2025).
# ==============================================================================

log_msg("INFO", paste(rep("-", 60), collapse = ""))
log_msg("INFO", "Step 2b: Computing Total Import Content via Leontief Inverse")
log_msg("INFO", paste(rep("-", 60), collapse = ""))

total_req_file <- file.path(INPUT_DIR,
  "BEA - Commodity-by-Commodity Total Requirements - Summary - 2024.xlsx")

if (file.exists(total_req_file)) {
  log_msg("INFO", paste("Reading Total Requirements matrix:", basename(total_req_file)))

  # Read full sheet without fixed range to detect layout
  tr_raw <- read_excel(total_req_file, sheet = 1, col_names = FALSE)
  log_msg("INFO", paste("  Raw sheet:", nrow(tr_raw), "rows x", ncol(tr_raw), "cols"))

  # --- Detect structure ---
  # Find first data row: contains a BEA commodity code in column 1.
  # BEA Summary codes include numeric-prefix codes (111CA, 3361MV, 5412OP),
  # alpha-prefix codes (4A0, HS, ORE), and government codes (GFGD, GFE, GSLG).
  # Use a strict pattern for initial detection (first row is always numeric-prefix).
  bea_code_strict <- "^[0-9]{2,3}[A-Z0-9]*$"
  # Broad pattern for all BEA commodity codes (2+ alphanumeric chars)
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
    for (i in 1:min(10, nrow(tr_raw))) {
      log_msg("ERROR", paste("  Row", i, "col A:", as.character(tr_raw[[1]][i])))
    }
    log_msg("WARN", "Falling back to direct import shares only")
    import_shares <- import_shares %>%
      dplyr::mutate(total_import_content = import_share)
    total_req_available <- FALSE
  } else {
    # Find data end: scan forward until an empty row or footer text.
    # BEA codes include non-numeric prefixes (4A0, HS, ORE, GFGD, Used, Other),
    # so we cannot rely on the strict numeric regex for boundary detection.
    data_end <- data_start
    for (i in (data_start + 1):min(nrow(tr_raw), data_start + 100)) {
      val <- str_trim(as.character(tr_raw[[1]][i]))
      if (is.na(val) || val == "" ||
          str_detect(val, "^(Legend|Note|Source|Total)")) break
      data_end <- i
    }

    # Find column code row: row above data_start with many short alphanumeric
    # values in cols 3+ (these are the BEA commodity codes for the columns)
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

    # Extract row codes and names
    row_codes <- str_trim(as.character(tr_raw[[1]][data_start:data_end]))
    row_names <- str_trim(as.character(tr_raw[[2]][data_start:data_end]))

    # Determine number of data columns from code row or first data row
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

    # Remove any trailing NA/empty column codes
    valid_cols <- !is.na(col_codes) & col_codes != ""
    col_codes <- col_codes[valid_cols]
    n_data_cols <- length(col_codes)

    log_msg("INFO", paste("  Matrix:", length(row_codes), "x", n_data_cols))
    log_msg("INFO", paste("  First 5 row codes:", paste(head(row_codes, 5), collapse = ", ")))
    log_msg("INFO", paste("  First 5 col codes:", paste(head(col_codes, 5), collapse = ", ")))

    # Build numeric matrix R (Total Requirements / Leontief inverse)
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

    # Dimensional check: row/column sums should generally be >= 1
    row_sums <- rowSums(R_matrix)
    log_msg("INFO", paste("  Row sum range:", round(min(row_sums), 2), "to",
                           round(max(row_sums), 2),
                           "(should generally be >= 1)"))

    # Build direct import share vector aligned to column ordering of R
    m_aligned <- data.frame(bea_code = col_codes, stringsAsFactors = FALSE) %>%
      dplyr::left_join(
        import_shares %>% dplyr::select(bea_code, import_share),
        by = "bea_code"
      ) %>%
      dplyr::mutate(import_share = replace_na(import_share, 0))

    m_vec <- m_aligned$import_share
    log_msg("INFO", paste("  Direct import share vector: matched",
                           sum(m_vec > 0), "of", length(m_vec), "commodities"))

    # Compute total import content: total_import = R %*% m
    total_import_vec <- as.numeric(R_matrix %*% m_vec)

    # Cap at 1.0 (same as direct share capping)
    n_over_1 <- sum(total_import_vec > 1, na.rm = TRUE)
    if (n_over_1 > 0) {
      log_msg("WARN", paste(n_over_1, "commodities with total import content > 100% (capped)"))
    }
    total_import_vec <- pmin(total_import_vec, 1.0)

    # Create data frame with total import content per commodity
    total_import_df <- data.frame(
      bea_code = row_codes,
      total_import_content = total_import_vec,
      stringsAsFactors = FALSE
    )

    # Add to import_shares for downstream use
    import_shares <- import_shares %>%
      dplyr::left_join(total_import_df, by = "bea_code") %>%
      dplyr::mutate(
        # For commodities not in the Total Requirements matrix, fall back to direct share
        total_import_content = ifelse(is.na(total_import_content),
                                       import_share, total_import_content)
      )

    # Log comparison: direct vs total
    log_msg("INFO", "Direct vs Total import content (top 10 by total):")
    top_total <- import_shares %>%
      dplyr::arrange(desc(total_import_content)) %>%
      dplyr::filter(total_import_content > 0) %>%
      head(10)

    for (i in 1:nrow(top_total)) {
      log_msg("INFO", paste("  ", top_total$bea_code[i], "-",
                             str_trunc(top_total$bea_name[i], 35), ":",
                             "direct =", round(top_total$import_share[i] * 100, 1), "%,",
                             "total =", round(top_total$total_import_content[i] * 100, 1), "%"))
    }

    # Sanity check: total should generally be >= direct
    n_lower <- sum(import_shares$total_import_content < import_shares$import_share - 0.001,
                    na.rm = TRUE)
    if (n_lower > 0) {
      log_msg("WARN", paste(n_lower, "commodities where total < direct (unexpected)"))
    }

    total_req_available <- TRUE
  }
} else {
  log_msg("WARN", paste("Total Requirements file not found:", basename(total_req_file)))
  log_msg("WARN", "Using direct import shares only (no Leontief inverse adjustment)")

  # Fallback: use direct import shares as total import content
  import_shares <- import_shares %>%
    dplyr::mutate(total_import_content = import_share)

  total_req_available <- FALSE
}

log_msg("INFO", paste("Total import content computed. Leontief available:",
                       total_req_available))

# ==============================================================================
# SECTION 3: MAP TO PCE CATEGORIES VIA BRIDGE TABLE
# ==============================================================================

log_msg("INFO", paste(rep("-", 60), collapse = ""))
log_msg("INFO", "Step 3: Mapping Import Shares to PCE Categories")
log_msg("INFO", paste(rep("-", 60), collapse = ""))

# --- 3a: Read PCE Bridge Table (2024) ---
bridge_file <- file.path(INPUT_DIR, "PCEBridge_Summary.xlsx")
log_msg("INFO", paste("Reading PCE Bridge:", basename(bridge_file)))

bridge_raw <- read_excel(bridge_file, sheet = "2024")
log_msg("INFO", paste("Read bridge table:", nrow(bridge_raw), "rows x", ncol(bridge_raw), "cols"))
log_msg("INFO", paste("Column names:", paste(names(bridge_raw), collapse = ", ")))

# Standardize column names - detect by partial matching
col_lower <- str_to_lower(names(bridge_raw))

find_col <- function(patterns, cols_lower, cols_orig) {
  for (p in patterns) {
    idx <- which(str_detect(cols_lower, p))
    if (length(idx) > 0) return(cols_orig[idx[1]])
  }
  return(NA_character_)
}

nipa_col     <- find_col(c("nipa", "line_?n"), col_lower, names(bridge_raw))
pce_cat_col  <- find_col(c("pce.*cat", "category"), col_lower, names(bridge_raw))
comm_code_col <- find_col(c("commodity.*code", "comm.*code"), col_lower, names(bridge_raw))
purch_val_col <- find_col(c("purchaser"), col_lower, names(bridge_raw))

# Position-based fallback if detection fails
if (is.na(nipa_col))     nipa_col     <- names(bridge_raw)[1]
if (is.na(pce_cat_col))  pce_cat_col  <- names(bridge_raw)[2]
if (is.na(comm_code_col)) comm_code_col <- names(bridge_raw)[3]
if (is.na(purch_val_col)) purch_val_col <- names(bridge_raw)[9]

log_msg("INFO", paste("  NIPA line col:", nipa_col))
log_msg("INFO", paste("  PCE category col:", pce_cat_col))
log_msg("INFO", paste("  Commodity code col:", comm_code_col))
log_msg("INFO", paste("  Purchasers' value col:", purch_val_col))

bridge_clean <- bridge_raw %>%
  dplyr::transmute(
    nipa_line = as.character(.data[[nipa_col]]),
    pce_category = str_trim(as.character(.data[[pce_cat_col]])),
    commodity_code = str_trim(as.character(.data[[comm_code_col]])),
    purchasers_value = suppressWarnings(as.numeric(.data[[purch_val_col]]))
  ) %>%
  dplyr::filter(!is.na(purchasers_value), purchasers_value != 0)

log_msg("INFO", paste("Bridge table after cleaning:", nrow(bridge_clean), "rows"))

# --- 3b: Get unique PCE categories from bridge ---
bridge_categories <- bridge_clean %>%
  dplyr::distinct(nipa_line, pce_category) %>%
  dplyr::arrange(as.numeric(nipa_line))

log_msg("INFO", paste("Unique PCE categories in bridge:", nrow(bridge_categories)))
for (i in 1:nrow(bridge_categories)) {
  log_msg("INFO", paste("  Line", bridge_categories$nipa_line[i], ":",
                         bridge_categories$pce_category[i]))
}

# --- 3c: Match Haver categories to bridge categories ---
# Use case-insensitive matching on category names
pce_goods <- pce_goods %>%
  dplyr::mutate(category_lower = str_to_lower(str_trim(category)))

bridge_categories <- bridge_categories %>%
  dplyr::mutate(cat_lower = str_to_lower(str_trim(pce_category)))

# Exact match first
matched <- pce_goods %>%
  dplyr::left_join(
    bridge_categories %>% dplyr::select(nipa_line, cat_lower, bridge_category = pce_category),
    by = c("category_lower" = "cat_lower")
  )

# Check for unmatched categories
unmatched <- matched %>% dplyr::filter(is.na(nipa_line))
if (nrow(unmatched) > 0) {
  log_msg("WARN", paste("Unmatched categories:", nrow(unmatched), "- trying partial match"))

  for (i in which(is.na(matched$nipa_line))) {
    cat_lower <- matched$category_lower[i]

    # Try partial contains matching
    partial <- which(str_detect(bridge_categories$cat_lower, fixed(cat_lower)) |
                       str_detect(cat_lower, fixed(bridge_categories$cat_lower)))
    if (length(partial) > 0) {
      matched$nipa_line[i] <- bridge_categories$nipa_line[partial[1]]
      matched$bridge_category[i] <- bridge_categories$pce_category[partial[1]]
      log_msg("INFO", paste("  Partial match:", matched$category[i], "->",
                             bridge_categories$pce_category[partial[1]]))
    } else {
      # Try matching first significant words
      cat_words <- str_split(cat_lower, "\\s+")[[1]]
      for (j in 1:nrow(bridge_categories)) {
        bridge_words <- str_split(bridge_categories$cat_lower[j], "\\s+")[[1]]
        overlap <- length(intersect(cat_words, bridge_words))
        if (overlap >= 2) {
          matched$nipa_line[i] <- bridge_categories$nipa_line[j]
          matched$bridge_category[i] <- bridge_categories$pce_category[j]
          log_msg("INFO", paste("  Word match:", matched$category[i], "->",
                                 bridge_categories$pce_category[j]))
          break
        }
      }
    }
  }
}

# Log final matching status
still_unmatched <- matched %>% dplyr::filter(is.na(nipa_line))
if (nrow(still_unmatched) > 0) {
  log_msg("WARN", paste("Still unmatched after partial matching:", nrow(still_unmatched)))
  for (cat in still_unmatched$category) {
    log_msg("WARN", paste("  ", cat))
  }
}

pce_goods <- matched %>%
  dplyr::select(-category_lower, -bridge_category)

log_msg("INFO", paste("Matched", sum(!is.na(pce_goods$nipa_line)), "of", nrow(pce_goods), "categories"))

# --- 3d: Join import shares to bridge and compute PCE-level shares (direct + total) ---
# Join commodity-level import shares to bridge by commodity code
bridge_with_shares <- bridge_clean %>%
  dplyr::left_join(
    import_shares %>% dplyr::select(bea_code, import_share, total_import_content),
    by = c("commodity_code" = "bea_code")
  ) %>%
  dplyr::mutate(
    # Commodities not in the import matrix (e.g., "Used") get share = 0
    import_share = replace_na(import_share, 0),
    total_import_content = replace_na(total_import_content, 0)
  )

# Compute purchasers'-value-weighted average import share for each PCE category
pce_import_shares <- bridge_with_shares %>%
  dplyr::group_by(nipa_line, pce_category) %>%
  dplyr::summarize(
    # Note: abs() is defensive - purchasers_value should be positive but some
    # BEA bridge entries (e.g. "Less: Expenditures by nonresidents") are negative
    s_k = safe_divide(
      sum(import_share * abs(purchasers_value), na.rm = TRUE),
      sum(abs(purchasers_value), na.rm = TRUE),
      default = 0
    ),
    # Total import content via Leontief inverse (direct + indirect)
    s_k_total = safe_divide(
      sum(total_import_content * abs(purchasers_value), na.rm = TRUE),
      sum(abs(purchasers_value), na.rm = TRUE),
      default = 0
    ),
    total_purchasers_value = sum(purchasers_value, na.rm = TRUE),
    n_commodities = n(),
    .groups = "drop"
  )

log_msg("INFO", "PCE-level import content shares (direct / total):")
for (i in 1:nrow(pce_import_shares)) {
  log_msg("INFO", paste("  Line", pce_import_shares$nipa_line[i], "-",
                         str_trunc(pce_import_shares$pce_category[i], 40), ":",
                         round(pce_import_shares$s_k[i] * 100, 1), "% /",
                         round(pce_import_shares$s_k_total[i] * 100, 1), "%"))
}

# Merge s_k and s_k_total back onto the 32 Haver categories
pce_goods <- pce_goods %>%
  dplyr::left_join(
    pce_import_shares %>% dplyr::select(nipa_line, s_k, s_k_total),
    by = "nipa_line"
  ) %>%
  dplyr::mutate(
    s_k = replace_na(s_k, 0),
    s_k_total = replace_na(s_k_total, 0)
  )

log_msg("INFO", paste("Categories with positive import share:", sum(pce_goods$s_k > 0)))

log_msg("INFO", "PCE import shares mapped")

# ==============================================================================
# SECTION 4: PULL HAVER DATA
# ==============================================================================

log_msg("INFO", paste(rep("-", 60), collapse = ""))
log_msg("INFO", "Step 4: Pulling Haver Price and Expenditure Data")
log_msg("INFO", paste(rep("-", 60), collapse = ""))

# --- 4a: Pull expenditure data (32 series) ---
exp_codes <- pce_goods$haver_exp
log_msg("INFO", paste("Pulling", length(exp_codes), "expenditure series"))

expenditure_data <- pull_haver_local(exp_codes, "monthly", "1996-01-01")

if (!is.null(expenditure_data)) {
  log_msg("INFO", paste("Expenditure data:", nrow(expenditure_data), "rows x",
                         ncol(expenditure_data) - 1, "series"))
  write_csv(expenditure_data, file.path(OUTPUT_DIR, "ipi_expenditure_data.csv"))
} else {
  # Try loading from existing file
  exp_file <- file.path(OUTPUT_DIR, "ipi_expenditure_data.csv")
  if (file.exists(exp_file)) {
    log_msg("INFO", "Loading expenditure data from existing CSV")
    expenditure_data <- read_csv(exp_file, show_col_types = FALSE) %>%
      dplyr::mutate(date = as.Date(date))
  } else {
    log_msg("ERROR", "No expenditure data available")
  }
}

# --- 4b: Pull price index data (32 series + 3 aggregates) ---
price_codes <- c(pce_goods$haver_price, aggregate_price_codes)
log_msg("INFO", paste("Pulling", length(price_codes), "price index series"))

price_data <- pull_haver_local(price_codes, "monthly", "1996-01-01")

if (!is.null(price_data)) {
  log_msg("INFO", paste("Price data:", nrow(price_data), "rows x",
                         ncol(price_data) - 1, "series"))
  write_csv(price_data, file.path(OUTPUT_DIR, "ipi_price_data.csv"))
} else {
  price_file <- file.path(OUTPUT_DIR, "ipi_price_data.csv")
  if (file.exists(price_file)) {
    log_msg("INFO", "Loading price data from existing CSV")
    price_data <- read_csv(price_file, show_col_types = FALSE) %>%
      dplyr::mutate(date = as.Date(date))
  } else {
    log_msg("ERROR", "No price data available")
  }
}

# ==============================================================================
# SECTION 5: CONSTRUCT IMPORT-WEIGHTED PRICE INDICES
# ==============================================================================

log_msg("INFO", paste(rep("-", 60), collapse = ""))
log_msg("INFO", "Step 5: Constructing Import-Weighted Price Indices")
log_msg("INFO", paste(rep("-", 60), collapse = ""))

if (!is.null(expenditure_data) && !is.null(price_data)) {

  # --- 5a: Reshape expenditure data to long format ---
  exp_long <- expenditure_data %>%
    pivot_longer(cols = -date, names_to = "haver_code", values_to = "expenditure") %>%
    dplyr::mutate(haver_code = str_to_lower(haver_code))

  # --- 5b: Join with import shares ---
  # Standardize Haver codes for matching (lowercase, strip @database)
  pce_goods_lookup <- pce_goods %>%
    dplyr::mutate(
      exp_code_lower = str_to_lower(str_extract(haver_exp, "^[A-Za-z0-9]+"))
    )

  exp_with_shares <- exp_long %>%
    dplyr::left_join(
      pce_goods_lookup %>% dplyr::select(exp_code_lower, s_k, s_k_total, type, is_food_energy, category),
      by = c("haver_code" = "exp_code_lower")
    ) %>%
    dplyr::filter(!is.na(s_k))

  log_msg("INFO", paste("Expenditure-shares join:", length(unique(exp_with_shares$haver_code)),
                         "series matched"))

  # --- 5c: Compute import-weighted expenditures ---
  exp_with_shares <- exp_with_shares %>%
    dplyr::mutate(import_weighted_exp = s_k * expenditure)

  # Apply 2-month moving average
  exp_with_shares <- exp_with_shares %>%
    dplyr::arrange(haver_code, date) %>%
    dplyr::group_by(haver_code) %>%
    dplyr::mutate(
      import_weighted_exp_ma = (import_weighted_exp + lag(import_weighted_exp, 1)) / 2
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(import_weighted_exp_ma))

  # --- 5d: Compute weights for three index variants ---

  compute_index <- function(exp_df, price_df, categories_filter, index_name) {

    log_msg("INFO", paste("  Computing", index_name, "index"))

    # Filter to selected categories
    exp_filtered <- exp_df %>%
      dplyr::filter(haver_code %in% categories_filter)

    # Compute normalized weights
    weights <- exp_filtered %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(
        w_k = import_weighted_exp_ma / sum(import_weighted_exp_ma, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    # Reshape price data to long format
    price_long <- price_df %>%
      pivot_longer(cols = -date, names_to = "haver_code", values_to = "price_index") %>%
      dplyr::mutate(haver_code = str_to_lower(haver_code))

    # Map expenditure codes to price codes
    price_map <- pce_goods_lookup %>%
      dplyr::mutate(
        price_code_lower = str_to_lower(str_extract(haver_price, "^[A-Za-z0-9]+"))
      ) %>%
      dplyr::select(exp_code_lower, price_code_lower)

    weights <- weights %>%
      dplyr::left_join(price_map, by = c("haver_code" = "exp_code_lower"))

    # Join with price data
    weights <- weights %>%
      dplyr::left_join(
        price_long %>% dplyr::select(date, haver_code, price_index),
        by = c("date" = "date", "price_code_lower" = "haver_code")
      )

    # Compute weighted price index
    index <- weights %>%
      dplyr::filter(!is.na(price_index) & !is.na(w_k)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        raw_index = sum(w_k * price_index, na.rm = TRUE),
        n_categories = sum(!is.na(w_k) & !is.na(price_index)),
        .groups = "drop"
      )

    # Rebase to December 2024 = 100
    dec_2024 <- index %>%
      dplyr::filter(date == as.Date("2024-12-01") |
                      (year(date) == 2024 & month(date) == 12))

    if (nrow(dec_2024) == 0) {
      # Use closest available date before end of 2024
      pre_2025 <- index$date[index$date <= as.Date("2024-12-31")]
      if (length(pre_2025) > 0) {
        dec_2024 <- index %>% dplyr::filter(date == max(pre_2025))
      }
    }

    if (nrow(dec_2024) > 0) {
      base_val <- dec_2024$raw_index[1]
      index <- index %>%
        dplyr::mutate(index_value = raw_index / base_val * 100)
      log_msg("INFO", paste("    Rebased to Dec 2024 = 100 (base value:", round(base_val, 2), ")"))
    } else {
      index <- index %>% dplyr::mutate(index_value = raw_index)
      log_msg("WARN", "    Could not find Dec 2024 for rebasing")
    }

    index %>% dplyr::select(date, index_value, n_categories)
  }

  # Get lowercase expenditure codes for each variant
  all_exp_codes <- str_to_lower(str_extract(pce_goods$haver_exp, "^[A-Za-z0-9]+"))
  core_exp_codes <- str_to_lower(str_extract(
    pce_goods$haver_exp[!pce_goods$is_food_energy], "^[A-Za-z0-9]+"))
  dur_exp_codes <- str_to_lower(str_extract(
    pce_goods$haver_exp[pce_goods$type == "durable"], "^[A-Za-z0-9]+"))

  # Compute three index variants
  idx_all_goods <- compute_index(exp_with_shares, price_data, all_exp_codes, "All Goods")
  idx_core_goods <- compute_index(exp_with_shares, price_data, core_exp_codes, "Core Goods")
  idx_durables <- compute_index(exp_with_shares, price_data, dur_exp_codes, "Durables")

  # --- 5e: Combine with aggregate indices for comparison ---
  # Get aggregate price indices, rebased to Dec 2024 = 100
  agg_long <- price_data %>%
    dplyr::select(date, jctgm, jcgxfem, jcdm) %>%
    dplyr::filter(!is.na(jctgm))

  dec_2024_agg <- agg_long %>%
    dplyr::filter(date == as.Date("2024-12-01") |
                    (year(date) == 2024 & month(date) == 12))

  if (nrow(dec_2024_agg) > 0) {
    agg_rebased <- agg_long %>%
      dplyr::mutate(
        pce_goods_idx = jctgm / dec_2024_agg$jctgm[1] * 100,
        pce_core_goods_idx = jcgxfem / dec_2024_agg$jcgxfem[1] * 100,
        pce_durables_idx = jcdm / dec_2024_agg$jcdm[1] * 100
      ) %>%
      dplyr::select(date, pce_goods_idx, pce_core_goods_idx, pce_durables_idx)
  } else {
    agg_rebased <- agg_long %>%
      dplyr::mutate(
        pce_goods_idx = jctgm, pce_core_goods_idx = jcgxfem, pce_durables_idx = jcdm
      ) %>%
      dplyr::select(date, pce_goods_idx, pce_core_goods_idx, pce_durables_idx)
  }

  # Combine all indices into one dataset
  import_price_index <- idx_all_goods %>%
    dplyr::rename(imported_goods = index_value) %>%
    dplyr::select(date, imported_goods) %>%
    dplyr::left_join(
      idx_core_goods %>% dplyr::rename(imported_core_goods = index_value) %>%
        dplyr::select(date, imported_core_goods),
      by = "date"
    ) %>%
    dplyr::left_join(
      idx_durables %>% dplyr::rename(imported_durables = index_value) %>%
        dplyr::select(date, imported_durables),
      by = "date"
    ) %>%
    dplyr::left_join(agg_rebased, by = "date")

  log_msg("INFO", paste("Final index dataset:", nrow(import_price_index), "months"))

  # Display recent values
  recent <- import_price_index %>% dplyr::filter(date >= as.Date("2024-10-01"))
  log_msg("INFO", "")
  log_msg("INFO", "Recent index values (Dec 2024 = 100):")
  for (i in 1:min(6, nrow(recent))) {
    log_msg("INFO", paste("  ", format(recent$date[i], "%Y-%m"),
                           "| Imported Core:", round(recent$imported_core_goods[i], 2),
                           "| PCE Core:", round(recent$pce_core_goods_idx[i], 2),
                           "| Imported Dur:", round(recent$imported_durables[i], 2),
                           "| PCE Dur:", round(recent$pce_durables_idx[i], 2)))
  }

  # ==============================================================================
  # SECTION 6: SAVE WEIGHTS FOR DEBUGGING
  # ==============================================================================

  # Save the time-varying weights for the latest month
  latest_weights <- exp_with_shares %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::group_by(haver_code, category) %>%
    dplyr::summarize(
      s_k = first(s_k),
      s_k_total = first(s_k_total),
      expenditure = first(expenditure),
      import_weighted_exp = first(import_weighted_exp),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      w_all = import_weighted_exp / sum(import_weighted_exp, na.rm = TRUE)
    ) %>%
    dplyr::arrange(desc(w_all))

  log_msg("INFO", "Latest weights calculated")

  log_msg("INFO", "")
  log_msg("INFO", "Top 10 weights (all goods, latest month):")
  for (i in 1:min(10, nrow(latest_weights))) {
    log_msg("INFO", paste("  ", latest_weights$category[i], ":",
                           round(latest_weights$w_all[i] * 100, 1), "%",
                           "(import share:", round(latest_weights$s_k[i] * 100, 1), "%)"))
  }

  # ==============================================================================
  # SECTION 6b: COMPUTE IMPORT CONTENT SHARES FOR PASSTHROUGH
  # ==============================================================================
  # Two types of import content shares:
  #
  # 1. Expenditure-weighted total import content (primary, for passthrough tables):
  #    For each variant, the fraction of consumer spending that ultimately goes to
  #    imports, accounting for both direct and indirect (supply-chain) import
  #    content via the Leontief inverse.
  #      import_share_variant = Σ_k (E_k / Σ_j E_j) × s_k_total
  #
  # 2. Direct import share (for comparison):
  #    Same computation but using only direct import shares.
  #      direct_share_variant = Σ_k (E_k / Σ_j E_j) × s_k

  # Join type/food_energy info back to latest weights
  pce_goods_type <- pce_goods %>%
    dplyr::mutate(exp_code = str_to_lower(str_extract(haver_exp, "^[A-Za-z0-9]+"))) %>%
    dplyr::select(exp_code, type, is_food_energy)

  weights_with_type <- latest_weights %>%
    dplyr::left_join(pce_goods_type, by = c("haver_code" = "exp_code"))

  # Core goods: exclude food/energy
  core_subset <- weights_with_type %>% dplyr::filter(!is_food_energy)
  core_exp_w <- core_subset$expenditure / sum(core_subset$expenditure, na.rm = TRUE)
  total_share_core <- sum(core_exp_w * core_subset$s_k_total, na.rm = TRUE)
  direct_share_core <- sum(core_exp_w * core_subset$s_k, na.rm = TRUE)

  # Durables
  dur_subset <- weights_with_type %>% dplyr::filter(type == "durable")
  dur_exp_w <- dur_subset$expenditure / sum(dur_subset$expenditure, na.rm = TRUE)
  total_share_dur <- sum(dur_exp_w * dur_subset$s_k_total, na.rm = TRUE)
  direct_share_dur <- sum(dur_exp_w * dur_subset$s_k, na.rm = TRUE)

  # All goods
  all_exp_w <- latest_weights$expenditure / sum(latest_weights$expenditure, na.rm = TRUE)
  total_share_all <- sum(all_exp_w * latest_weights$s_k_total, na.rm = TRUE)
  direct_share_all <- sum(all_exp_w * latest_weights$s_k, na.rm = TRUE)

  ipi_effective_shares <- tibble::tibble(
    variant = c("all_goods", "core_goods", "durables"),
    effective_import_share = c(total_share_all, total_share_core, total_share_dur),
    direct_import_share = c(direct_share_all, direct_share_core, direct_share_dur)
  )

  write_csv(ipi_effective_shares, file.path(OUTPUT_DIR, "ipi_effective_shares.csv"))
  log_msg("INFO", "Saved ipi_effective_shares.csv")
  log_msg("INFO", "  Import content shares (total / direct):")
  log_msg("INFO", paste("    All goods:",
                         round(total_share_all * 100, 1), "% /",
                         round(direct_share_all * 100, 1), "%"))
  log_msg("INFO", paste("    Core goods:",
                         round(total_share_core * 100, 1), "% /",
                         round(direct_share_core * 100, 1), "%"))
  log_msg("INFO", paste("    Durables:",
                         round(total_share_dur * 100, 1), "% /",
                         round(direct_share_dur * 100, 1), "%"))

  # Save per-category detail for auditing
  ipi_shares_detail <- pce_import_shares %>%
    dplyr::left_join(
      pce_goods %>%
        dplyr::distinct(nipa_line, type, is_food_energy),
      by = "nipa_line"
    ) %>%
    dplyr::select(nipa_line, pce_category, type, is_food_energy,
                  direct_share = s_k, total_share = s_k_total)
  write_csv(ipi_shares_detail, file.path(OUTPUT_DIR, "ipi_import_shares_detail.csv"))
  log_msg("INFO", "Saved ipi_import_shares_detail.csv")

} else {
  log_msg("ERROR", "Cannot construct index - missing price or expenditure data")
  import_price_index <- NULL
}

# ==============================================================================
# SECTION 7: SAVE FINAL OUTPUT
# ==============================================================================

log_msg("INFO", paste(rep("-", 60), collapse = ""))
log_msg("INFO", "Step 6: Saving Output")
log_msg("INFO", paste(rep("-", 60), collapse = ""))

if (!is.null(import_price_index)) {
  write_csv(import_price_index, file.path(OUTPUT_DIR, "import_price_index.csv"))
  log_msg("INFO", "Saved import_price_index.csv")

  log_msg("INFO", "Import price index saved")
}

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

log_msg("INFO", "")
log_msg("INFO", paste(rep("=", 60), collapse = ""))
log_msg("INFO", "IMPORT PRICE INDEX SUMMARY")
log_msg("INFO", paste(rep("=", 60), collapse = ""))
log_msg("INFO", "")
log_msg("INFO", "Output files:")
log_msg("INFO", "  - import_price_index.csv (final index time series)")
log_msg("INFO", "  - ipi_effective_shares.csv (import content shares for passthrough)")
log_msg("INFO", "  - ipi_import_shares_detail.csv (per-category direct/total shares)")
log_msg("INFO", "  - ipi_expenditure_data.csv (Haver expenditure pulls)")
log_msg("INFO", "  - ipi_price_data.csv (Haver price index pulls)")
log_msg("INFO", "")
log_msg("INFO", paste(rep("=", 60), collapse = ""))
log_msg("INFO", "Import Price Index Script Complete")
log_msg("INFO", paste(rep("=", 60), collapse = ""))
