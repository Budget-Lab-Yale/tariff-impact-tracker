# ==============================================================================
# Tariff Impacts - Pipeline Run Script
# ==============================================================================
#
# Purpose: Execute the full tariff impacts analysis pipeline
#
# Usage:
#   source("run_all.R")
#
# Or from command line:
#   Rscript run_all.R
#
# This script:
#   1. Checks for required packages and installs missing ones
#   2. Validates the environment (Haver connection, directories)
#   3. Runs the data processing script
#   4. Generates the HTML and Word reports
#   5. Logs execution status
#
# Author: John Iselin, Yale Budget Lab
# Date: February 2026
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("TARIFF IMPACTS ANALYSIS PIPELINE\n")
cat(rep("=", 70), "\n", sep = "")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# LOGGING SETUP
# ==============================================================================

# Create logs directory if it doesn't exist
LOG_DIR <- file.path(getwd(), "logs")
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)

# Create log file with timestamp
LOG_FILE <- file.path(LOG_DIR, paste0("run_all_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

# Initialize log file
cat(paste0("Tariff Impacts Pipeline Log\n"),
    paste0("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"),
    paste0(rep("=", 70), "\n"),
    file = LOG_FILE, sep = "")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Publication flag: controls whether final outputs are copied to output/publication/
# (which is tracked by git) and whether website assets are exported to website/.
# Default: FALSE (local/development run). Set to TRUE for publication, or override
# via environment variable: PUBLICATION_RUN=TRUE Rscript run_all.R
PUBLICATION_RUN <- as.logical(Sys.getenv("PUBLICATION_RUN", unset = "FALSE"))
if (is.na(PUBLICATION_RUN)) PUBLICATION_RUN <- FALSE
cat("Run mode:", if (PUBLICATION_RUN) "PUBLICATION (outputs copied to publication/ and website/)" else "LOCAL (development only)", "\n\n")

# Required packages (Haver is optional -- loaded conditionally via check_haver_available())
REQUIRED_PACKAGES <- c(
  "dplyr",       # Data manipulation
  "tidyr",       # Data reshaping
  "lubridate",   # Date handling
  "readr",       # CSV reading/writing
  "readxl",      # Excel file reading
  "openxlsx",    # Excel file writing (for results export)
  "here",        # Project-relative paths
  "stringr",     # String manipulation
  "ggplot2",     # Plotting
  "patchwork",   # Multi-panel figures
  "scales",      # Plot scales
  "knitr",       # Report generation
  "kableExtra",  # Table formatting (HTML)
  "flextable",   # Table formatting (Word)
  "rmarkdown",   # R Markdown rendering
  "sandwich",    # Newey-West HAC standard errors
  "lmtest",      # Robust coefficient testing
  "MSwM",        # Markov-switching models for local projection trend
  "depmixS4"     # Hidden Markov models (fallback for MSwM)
)

# Required input files for each standalone script.
# Canonical source: employment_index_required_inputs() in R/employment_index.R
#                   import_price_index_required_inputs() in R/import_price_index.R
# Keep these in sync -- each script validates its own inputs at runtime.
EMP_INDEX_INPUTS <- c(
  "USITC - Customs and Duties - January 2026.xlsx",
  "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx",
  "BEA - The Use of Commodities by Industry - Summary - 2024.xlsx",
  "x_codes.csv",
  "naics_to_bea_crosswalk.csv"
)

IPI_INPUTS <- c(
  "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx",
  "BEA - The Use of Commodities by Industry - Summary - 2024.xlsx",
  "PCEBridge_Summary.xlsx"
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Simple log_msg for early use (before utils.R is sourced)
log_msg <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- paste0("[", timestamp, "][", level, "] ", message, "\n")
  # Print to console
  cat(log_line)
  # Append to log file
  if (exists("LOG_FILE") && file.exists(dirname(LOG_FILE))) {
    cat(log_line, file = LOG_FILE, append = TRUE)
  }
}

install_missing_packages <- function(packages) {
  missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    log_msg("INFO", paste("Installing missing packages:", paste(missing, collapse = ", ")))
    install.packages(missing)
  }
}

check_packages_available <- function(packages) {
  missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    log_msg("WARN", paste("Missing packages:", paste(missing, collapse = ", ")))
    return(FALSE)
  }
  return(TRUE)
}

# ==============================================================================
# STEP 1: CHECK AND INSTALL PACKAGES
# ==============================================================================

log_msg("INFO", "Step 1: Checking required packages...")

if (!check_packages_available(REQUIRED_PACKAGES)) {
  log_msg("INFO", "Attempting to install missing packages...")
  install_missing_packages(REQUIRED_PACKAGES)

  # Verify installation
  if (!check_packages_available(REQUIRED_PACKAGES)) {
    stop("Failed to install required packages. Please install manually.")
  }
}

log_msg("INFO", "All required packages available")

# ==============================================================================
# STEP 2: VALIDATE ENVIRONMENT
# ==============================================================================

log_msg("INFO", "Step 2: Validating environment...")

# Load here package for path management
library(here)

# Check if we're in the right project directory
if (!file.exists(here("R", "tariff_impacts_work.R"))) {
  stop("Cannot find R/tariff_impacts_work.R - please run from project root directory")
}

# Create required directories
dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("input"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("website", "csv"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("website", "html"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("website", "vintages"), showWarnings = FALSE, recursive = TRUE)

log_msg("INFO", paste("Working directory:", here()))

# Check Haver availability (requireNamespace, not library -- Haver is optional)
haver_available <- tryCatch({
  if (!requireNamespace("Haver", quietly = TRUE)) stop("not installed")
  Haver::haver.direct("on")
  TRUE
}, error = function(e) FALSE)

if (haver_available) {
  log_msg("INFO", "Haver Analytics connection: AVAILABLE")
} else {
  log_msg("WARN", "Haver Analytics connection: UNAVAILABLE -- will use cached CSV files")
}

# --- Validate cached output CSVs when Haver is unavailable ---
# These are the CSVs produced by tariff_impacts_work.R that the report needs
REQUIRED_OUTPUT_CSVS <- c(
  "tariff_revenue.csv", "import_shares.csv", "pce_prices.csv",
  "import_prices.csv", "employment.csv", "industrial_production.csv",
  "trade_flows.csv", "fed_policy.csv", "exchange_rates.csv",
  "twi_long.csv", "cpi_data.csv"
)

output_csvs <- list.files(here("output"), pattern = "\\.csv$")
if (length(output_csvs) > 0) {
  log_msg("INFO", paste("Found", length(output_csvs), "existing output CSV files"))
}

if (!haver_available) {
  missing_csvs <- REQUIRED_OUTPUT_CSVS[!REQUIRED_OUTPUT_CSVS %in% output_csvs]
  if (length(missing_csvs) > 0) {
    log_msg("ERROR", paste("Offline mode but missing required cached CSVs:"))
    for (f in missing_csvs) log_msg("ERROR", paste("  Missing:", f))
    stop("Pipeline cannot proceed: Haver unavailable and cached outputs incomplete. ",
         "Missing: ", paste(missing_csvs, collapse = ", "))
  }
  # Check that cached CSVs are non-empty
  empty_csvs <- REQUIRED_OUTPUT_CSVS[sapply(REQUIRED_OUTPUT_CSVS, function(f) {
    fpath <- here("output", f)
    file.exists(fpath) && file.size(fpath) < 10  # less than 10 bytes = effectively empty
  })]
  if (length(empty_csvs) > 0) {
    log_msg("ERROR", paste("Cached CSVs exist but are empty:"))
    for (f in empty_csvs) log_msg("ERROR", paste("  Empty:", f))
    stop("Pipeline cannot proceed: cached CSVs are empty. ",
         "Empty: ", paste(empty_csvs, collapse = ", "))
  }
  log_msg("INFO", "All required cached CSVs present and non-empty -- offline mode OK")
}

# --- Check input files for employment index ---
check_inputs <- function(input_files, label = "step") {
  missing <- input_files[!sapply(input_files, function(f) file.exists(here("input", f)))]
  if (length(missing) > 0) {
    log_msg("SKIP", paste0(label, ": missing input files:"))
    for (f in missing) log_msg("SKIP", paste("  Missing:", f))
  }
  length(missing) == 0
}

emp_idx_ready <- check_inputs(EMP_INDEX_INPUTS, label = "Employment index")
ipi_ready <- check_inputs(IPI_INPUTS, label = "Import price index")

# ==============================================================================
# STEP 3: RUN DATA PROCESSING
# ==============================================================================

log_msg("INFO", "Step 3: Running data processing (tariff_impacts_work.R)...")

# Start timer
start_time <- Sys.time()

# Run the main work script
tryCatch({
  source(here("R", "tariff_impacts_work.R"))
  log_msg("OK", "Step 3 completed: data processing finished")
}, error = function(e) {
  log_msg("FAIL", paste("Step 3 failed: data processing --", e$message))
  stop("Pipeline cannot proceed: data processing step failed")
})

# ==============================================================================
# STEP 4: RUN EMPLOYMENT INDEX CALCULATION
# ==============================================================================

log_msg("INFO", "Step 4: Running employment index calculation...")

if (emp_idx_ready) {
  tryCatch({
    source(here("R", "employment_index.R"))
    log_msg("OK", "Step 4 completed: employment index calculated")
  }, error = function(e) {
    log_msg("FAIL", paste("Step 4 failed: employment index --", e$message))
    log_msg("INFO", "Pipeline continues without employment index")
  })
} else {
  log_msg("SKIP", "Step 4 skipped: employment index (missing input files, see above)")
}

# ==============================================================================
# STEP 5: RUN IMPORT PRICE INDEX CALCULATION
# ==============================================================================

log_msg("INFO", "Step 5: Running import price index calculation...")

if (ipi_ready) {
  tryCatch({
    source(here("R", "import_price_index.R"))
    log_msg("OK", "Step 5 completed: import price index calculated")
  }, error = function(e) {
    log_msg("FAIL", paste("Step 5 failed: import price index --", e$message))
    log_msg("INFO", "Pipeline continues without import price index")
  })
} else {
  log_msg("SKIP", "Step 5 skipped: import price index (missing input files, see above)")
}

# ==============================================================================
# STEP 6: GENERATE REPORTS, EXPORTS, AND DRUPAL HTML
# ==============================================================================

log_msg("INFO", "Step 6: Generating reports and exports...")

# Pass publication flag to Rmd/script environment
Sys.setenv(PUBLICATION_RUN = as.character(PUBLICATION_RUN))

# 6a: HTML report (review document with ggplot figures)
report_file <- here("R", "tariff_impacts_report.Rmd")
if (!file.exists(report_file)) {
  log_msg("SKIP", "Step 6a: report template not found -- skipping")
} else {
  tryCatch({
    html_file <- rmarkdown::render(
      input = report_file,
      output_format = "html_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("OK", paste("6a: HTML report generated:", basename(html_file)))
  }, error = function(e) {
    log_msg("FAIL", paste("6a: HTML report failed:", e$message))
  })

  # 6b: Word report
  tryCatch({
    word_file <- rmarkdown::render(
      input = report_file,
      output_format = "word_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("OK", paste("6b: Word report generated:", basename(word_file)))
  }, error = function(e) {
    log_msg("FAIL", paste("6b: Word report failed:", e$message))
  })
}

# 6c: Export website data (Excel workbook + CSVs)
export_script <- here("R", "export_website_data.R")
if (!file.exists(export_script)) {
  log_msg("SKIP", "Step 6c: export_website_data.R not found -- skipping")
} else {
  tryCatch({
    source(export_script)
    log_msg("OK", "6c: Website data exported (Excel + CSVs)")
  }, error = function(e) {
    log_msg("FAIL", paste("6c: Website data export failed:", e$message))
  })
}

# 6d: Drupal HTML fragment (publication runs only)
drupal_file <- here("R", "tariff_impacts_drupal.Rmd")
if (PUBLICATION_RUN && file.exists(drupal_file)) {
  tryCatch({
    drupal_html <- rmarkdown::render(
      input = drupal_file,
      output_format = "html_fragment",
      output_dir = here("website", "html"),
      output_file = "tariff_impacts_report_drupal.html",
      quiet = TRUE
    )
    log_msg("OK", paste("6d: Drupal HTML generated:", basename(drupal_html)))
    # Copy to vintage
    vintage_dir <- file.path(here("website", "vintages"), format(Sys.Date(), "%Y%m%d"), "html")
    dir.create(vintage_dir, showWarnings = FALSE, recursive = TRUE)
    file.copy(drupal_html, file.path(vintage_dir, "tariff_impacts_report_drupal.html"),
              overwrite = TRUE)
  }, error = function(e) {
    log_msg("FAIL", paste("6d: Drupal HTML failed:", e$message))
  })
} else if (!PUBLICATION_RUN) {
  log_msg("SKIP", "Step 6d: Drupal HTML skipped (not a publication run)")
} else {
  log_msg("SKIP", "Step 6d: tariff_impacts_drupal.Rmd not found -- skipping")
}

# 6e-6f: Methodology document
methodology_file <- here("R", "methodology.Rmd")
if (!file.exists(methodology_file)) {
  log_msg("SKIP", "Methodology template not found -- skipping")
} else {
  tryCatch({
    meth_html <- rmarkdown::render(
      input = methodology_file,
      output_format = "html_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("OK", paste("6e: Methodology HTML generated:", basename(meth_html)))
  }, error = function(e) {
    log_msg("FAIL", paste("6e: Methodology HTML failed:", e$message))
  })

  tryCatch({
    meth_word <- rmarkdown::render(
      input = methodology_file,
      output_format = "word_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("OK", paste("6f: Methodology Word generated:", basename(meth_word)))
  }, error = function(e) {
    log_msg("FAIL", paste("6f: Methodology Word failed:", e$message))
  })
}

# ==============================================================================
# STEP 7: SUMMARY
# ==============================================================================

end_time <- Sys.time()
elapsed <- round(difftime(end_time, start_time, units = "mins"), 1)

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("PIPELINE COMPLETE\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n")
cat("Elapsed time:", elapsed, "minutes\n")
cat("\n")
cat("Output files in:", here("output"), "\n")
cat("\n")

# List output files
output_files <- list.files(here("output"), pattern = "\\.(csv|html|docx)$")
cat("Generated files:\n")
for (f in output_files) {
  size <- file.size(here("output", f))
  size_str <- if (size > 1e6) {
    sprintf("%.1f MB", size / 1e6)
  } else {
    sprintf("%.1f KB", size / 1e3)
  }
  cat(sprintf("  - %-40s %s\n", f, size_str))
}

cat("\n")
cat("Reports:\n")
cat("  HTML: ", here("output", "tariff_impacts_report.html"), "\n")
cat("  Word: ", here("output", "tariff_impacts_report.docx"), "\n")
cat("\n")

# ==============================================================================
# STEP 8: PUBLICATION COPY (conditional)
# ==============================================================================

if (PUBLICATION_RUN) {
  log_msg("INFO", "Step 8: Publication mode -- copying final outputs...")

  pub_dir <- here("output", "publication")
  dir.create(pub_dir, showWarnings = FALSE, recursive = TRUE)

  # Gather final output files (xlsx, docx, html)
  pub_files <- list.files(here("output"), pattern = "\\.(xlsx|docx|html)$",
                          full.names = TRUE)
  # Exclude anything already in publication/
  pub_files <- pub_files[!grepl("publication", pub_files)]

  if (length(pub_files) > 0) {
    today_filename <- paste0("tariff_impacts_results_", format(Sys.Date(), "%Y%m%d"), ".xlsx")

    # Remove prior dated results files from output/ so they don't get copied
    old_output_results <- list.files(here("output"),
                                     pattern = "^tariff_impacts_results_\\d{8}\\.xlsx$",
                                     full.names = TRUE)
    old_output_results <- old_output_results[!grepl("publication", old_output_results)]
    old_output_results <- old_output_results[basename(old_output_results) != today_filename]
    if (length(old_output_results) > 0) {
      file.remove(old_output_results)
      log_msg("INFO", paste("Removed", length(old_output_results), "old results file(s) from output/"))
      # Refresh the file list after cleanup
      pub_files <- list.files(here("output"), pattern = "\\.(xlsx|docx|html)$",
                              full.names = TRUE)
      pub_files <- pub_files[!grepl("publication", pub_files)]
    }

    # Remove prior dated results files from publication/ before copying
    old_pub_results <- list.files(pub_dir, pattern = "^tariff_impacts_results_\\d{8}\\.xlsx$",
                                  full.names = TRUE)
    old_pub_results <- old_pub_results[basename(old_pub_results) != today_filename]
    if (length(old_pub_results) > 0) {
      file.remove(old_pub_results)
      log_msg("INFO", paste("Removed", length(old_pub_results), "old results file(s) from publication/"))
    }

    file.copy(pub_files, pub_dir, overwrite = TRUE)
    log_msg("INFO", paste("Copied", length(pub_files), "files to output/publication/"))
    for (f in basename(pub_files)) {
      log_msg("INFO", paste("  ", f))
    }
  } else {
    log_msg("SKIP", "Step 8: no output files found to copy for publication")
  }
} else {
  log_msg("SKIP", "Step 8 skipped: not a publication run (set PUBLICATION_RUN=TRUE to enable)")
}

# Log completion to file
log_msg("OK", "Pipeline completed successfully")
log_msg("INFO", paste("Log file saved to:", LOG_FILE))

cat("Log file saved to:\n")
cat("  ", LOG_FILE, "\n")
cat("\n")
cat(rep("=", 70), "\n", sep = "")
