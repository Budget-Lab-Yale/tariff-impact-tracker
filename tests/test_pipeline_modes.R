# ==============================================================================
# Pipeline Mode Verification Tests
# ==============================================================================
#
# Purpose: Verify that run_all.R handles offline, missing-input, and
#          publication modes correctly without actually running the full
#          pipeline. These are lightweight pre-flight checks.
#
# Usage:
#   Rscript tests/test_pipeline_modes.R
#
# Author: John Iselin, Yale Budget Lab
# Date: March 2026
# ==============================================================================

library(here)
here::i_am("tests/test_pipeline_modes.R")

passed <- 0
failed <- 0

test <- function(description, expr) {
  result <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(result)) {
    cat(sprintf("  PASS: %s\n", description))
    passed <<- passed + 1
  } else {
    cat(sprintf("  FAIL: %s\n", description))
    failed <<- failed + 1
  }
}

cat("\n")
cat(strrep("=", 60), "\n")
cat("Pipeline Mode Verification Tests\n")
cat(rep("=", 60), "\n\n")

# ==============================================================================
# TEST 1: Haver offline, caches present
# ==============================================================================
cat("--- Test 1: Offline mode with cached CSVs ---\n")

REQUIRED_OUTPUT_CSVS <- c(
  "tariff_revenue.csv", "import_shares.csv", "pce_prices.csv",
  "import_prices.csv", "employment.csv", "industrial_production.csv",
  "trade_flows.csv", "fed_policy.csv", "exchange_rates.csv",
  "twi_long.csv", "cpi_data.csv"
)

output_dir <- here("output")
existing_csvs <- list.files(output_dir, pattern = "\\.csv$")

test("output/ directory exists", dir.exists(output_dir))

for (f in REQUIRED_OUTPUT_CSVS) {
  test(paste("cached CSV exists:", f), f %in% existing_csvs)
}

# Check non-empty
for (f in REQUIRED_OUTPUT_CSVS) {
  fpath <- here("output", f)
  test(paste("cached CSV non-empty:", f), {
    file.exists(fpath) && file.size(fpath) >= 10
  })
}

cat("\n")

# ==============================================================================
# TEST 2: Employment index input readiness
# ==============================================================================
cat("--- Test 2: Employment index input files ---\n")

# Source the canonical input list from the script
source(here("R", "utils.R"))
tryCatch({
  # Parse just the function definition without running the script
  emp_inputs <- c(
    "USITC - Customs and Duties - January 2026.xlsx",
    "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx",
    "BEA - The Use of Commodities by Industry - Summary - 2024.xlsx",
    "x_codes.csv",
    "naics_to_bea_crosswalk.csv"
  )

  for (f in emp_inputs) {
    test(paste("employment input exists:", f), file.exists(here("input", f)))
  }
}, error = function(e) {
  cat(sprintf("  SKIP: could not check employment inputs: %s\n", e$message))
})

cat("\n")

# ==============================================================================
# TEST 3: Import price index input readiness
# ==============================================================================
cat("--- Test 3: Import price index input files ---\n")

ipi_inputs <- c(
  "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx",
  "BEA - The Use of Commodities by Industry - Summary - 2024.xlsx",
  "PCEBridge_Summary.xlsx"
)

for (f in ipi_inputs) {
  test(paste("IPI input exists:", f), file.exists(here("input", f)))
}

cat("\n")

# ==============================================================================
# TEST 4: Publication mode safety
# ==============================================================================
cat("--- Test 4: Publication mode safety ---\n")

test("PUBLICATION_RUN env var defaults to FALSE or unset", {
  val <- Sys.getenv("PUBLICATION_RUN", unset = "FALSE")
  val %in% c("FALSE", "false", "")
})

test("output/publication/ directory exists", {
  dir.exists(here("output", "publication"))
})

test("website/ directory exists", {
  dir.exists(here("website"))
})

test("website/csv/ directory exists", {
  dir.exists(here("website", "csv"))
})

test("website/html/ directory exists", {
  dir.exists(here("website", "html"))
})

cat("\n")

# ==============================================================================
# TEST 5: y_j_output.csv cache health
# ==============================================================================
cat("--- Test 5: y_j_output.csv cache ---\n")

y_j_file <- here("output", "y_j_output.csv")

test("y_j_output.csv exists", file.exists(y_j_file))

if (file.exists(y_j_file)) {
  test("y_j_output.csv is non-empty", file.size(y_j_file) >= 10)

  cache_age <- as.numeric(difftime(Sys.time(), file.mtime(y_j_file), units = "days"))
  test(sprintf("y_j_output.csv age (%.0f days) <= 90 days", cache_age), cache_age <= 90)

  # Check it's valid CSV
  test("y_j_output.csv is parseable CSV", {
    df <- tryCatch(read.csv(y_j_file, nrows = 5), error = function(e) NULL)
    !is.null(df) && nrow(df) > 0
  })
}

cat("\n")

# ==============================================================================
# TEST 6: Haver conditional loading
# ==============================================================================
cat("--- Test 6: Haver is not hard-required ---\n")

test("utils.R check_haver_available() uses requireNamespace", {
  fn_body <- paste(deparse(check_haver_available), collapse = "\n")
  grepl("requireNamespace", fn_body)
})

# Verify no library(Haver) in standalone scripts
for (script in c("R/tariff_impacts_work.R", "R/employment_index.R", "R/import_price_index.R")) {
  lines <- readLines(here(script), warn = FALSE)
  test(paste("no library(Haver) in", script), {
    !any(grepl("^library\\(Haver\\)", lines))
  })
}

cat("\n")

# ==============================================================================
# SUMMARY
# ==============================================================================
cat(strrep("=", 60), "\n")
total <- passed + failed
cat(sprintf("Results: %d/%d passed", passed, total))
if (failed > 0) {
  cat(sprintf(", %d FAILED", failed))
}
cat("\n")
cat(rep("=", 60), "\n\n")

if (failed > 0) {
  quit(status = 1, save = "no")
}
