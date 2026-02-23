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

# Publication flag: set to TRUE for a final/publication run.
# When TRUE, final outputs (Excel, HTML, Word) are copied to output/publication/
# which is tracked by git. Regular (FALSE) runs stay in output/ and are gitignored.
PUBLICATION_RUN <- TRUE

# Required packages
REQUIRED_PACKAGES <- c(
  "Haver",       # Haver Analytics data access

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

# Required input files for employment index (USITC tariffs + BEA import matrix)
EMP_INDEX_INPUTS <- c(
  "USITC - Customs and Duties - January 2026.xlsx",
  "BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx"
)

# Required input files for import price index
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

    # Haver package needs special handling
    if ("Haver" %in% missing) {
      haver_install_result <- tryCatch({
        install.packages("Haver", repos = "http://www.haver.com/r/")
        TRUE
      }, error = function(e) {
        log_msg("WARN", "Could not install Haver package - Haver connection will not be available")
        FALSE
      })
      # Update missing list (without using <<-)
      missing <- missing[missing != "Haver"]
    }

    # Install remaining packages from CRAN
    if (length(missing) > 0) {
      install.packages(missing)
    }
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
  if (!check_packages_available(REQUIRED_PACKAGES[REQUIRED_PACKAGES != "Haver"])) {
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

log_msg("INFO", paste("Working directory:", here()))

# Check Haver availability
haver_available <- tryCatch({
  library(Haver)
  haver.direct("on")
  TRUE
}, error = function(e) {
  FALSE
})

if (haver_available) {
  log_msg("INFO", "Haver Analytics connection available")
} else {
  log_msg("WARN", "Haver Analytics not available - will use existing CSV files if present")
}

# Check for existing output files
output_files <- list.files(here("output"), pattern = "\\.csv$")
if (length(output_files) > 0) {
  log_msg("INFO", paste("Found", length(output_files), "existing output files"))
} else {
  if (!haver_available) {
    log_msg("WARN", "No existing output files and Haver not available - some analyses may fail")
  }
}

# Check for input files (for employment index)
input_available <- sapply(EMP_INDEX_INPUTS, function(f) file.exists(here("input", f)))
if (all(input_available)) {
  log_msg("INFO", "All input files for employment index available")
} else {
  missing_inputs <- EMP_INDEX_INPUTS[!input_available]
  log_msg("WARN", paste("Missing input files:", paste(missing_inputs, collapse = ", ")))
  log_msg("WARN", "Tariff employment index will be skipped")
}

# ==============================================================================
# STEP 3: RUN DATA PROCESSING
# ==============================================================================

log_msg("INFO", "Step 3: Running data processing...")

# Start timer
start_time <- Sys.time()

# Run the main work script
tryCatch({
  source(here("R", "tariff_impacts_work.R"))
  log_msg("INFO", "Data processing completed successfully")
}, error = function(e) {
  log_msg("ERROR", paste("Data processing failed:", e$message))
  stop("Pipeline failed at data processing step")
})

# ==============================================================================
# STEP 4: RUN EMPLOYMENT INDEX CALCULATION
# ==============================================================================

log_msg("INFO", "Step 4: Running employment index calculation...")

emp_idx_crosswalk <- here("input", "naics_to_bea_crosswalk.csv")

emp_idx_ready <- all(sapply(EMP_INDEX_INPUTS, function(f) file.exists(here("input", f)))) &&
                 file.exists(emp_idx_crosswalk)

if (emp_idx_ready) {
  tryCatch({
    source(here("R", "employment_index.R"))
    log_msg("INFO", "Employment index calculation completed successfully")
  }, error = function(e) {
    log_msg("WARN", paste("Employment index calculation failed:", e$message))
    log_msg("WARN", "Continuing without employment index - other outputs are still available")
  })
} else {
  log_msg("WARN", "Missing input files for employment index - skipping")
  if (!file.exists(emp_idx_crosswalk)) {
    log_msg("WARN", "  Missing: input/naics_to_bea_crosswalk.csv")
  }
  for (f in EMP_INDEX_INPUTS) {
    if (!file.exists(here("input", f))) {
      log_msg("WARN", paste("  Missing:", f))
    }
  }
}

# ==============================================================================
# STEP 5: RUN IMPORT PRICE INDEX CALCULATION
# ==============================================================================

log_msg("INFO", "Step 5: Running import price index calculation...")

ipi_ready <- all(sapply(IPI_INPUTS, function(f) file.exists(here("input", f))))

if (ipi_ready) {
  tryCatch({
    source(here("R", "import_price_index.R"))
    log_msg("INFO", "Import price index calculation completed successfully")
  }, error = function(e) {
    log_msg("WARN", paste("Import price index calculation failed:", e$message))
    log_msg("WARN", "Continuing without import price index - other outputs are still available")
  })
} else {
  log_msg("WARN", "Missing input files for import price index - skipping")
  for (f in IPI_INPUTS) {
    if (!file.exists(here("input", f))) {
      log_msg("WARN", paste("  Missing:", f))
    }
  }
}

# ==============================================================================
# STEP 6: GENERATE REPORTS (HTML + Word)
# ==============================================================================

log_msg("INFO", "Step 6: Generating reports...")

# Check for report file
report_file <- here("R", "tariff_impacts_report.Rmd")
if (!file.exists(report_file)) {
  log_msg("WARN", "Report template not found - skipping report generation")
} else {
  # 6a: HTML report
  tryCatch({
    html_file <- rmarkdown::render(
      input = report_file,
      output_format = "html_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("INFO", paste("HTML report generated:", html_file))
  }, error = function(e) {
    log_msg("ERROR", paste("HTML report generation failed:", e$message))
    log_msg("WARN", "Continuing without HTML report - data files are still available")
  })

  # 6b: Word report
  tryCatch({
    word_file <- rmarkdown::render(
      input = report_file,
      output_format = "word_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("INFO", paste("Word report generated:", word_file))
  }, error = function(e) {
    log_msg("WARN", paste("Word report generation failed:", e$message))
    log_msg("WARN", "HTML report is still available")
  })
}

# Methodology document
methodology_file <- here("R", "methodology.Rmd")
if (!file.exists(methodology_file)) {
  log_msg("WARN", "Methodology template not found - skipping")
} else {
  # 6c: Methodology HTML
  tryCatch({
    meth_html <- rmarkdown::render(
      input = methodology_file,
      output_format = "html_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("INFO", paste("Methodology HTML generated:", meth_html))
  }, error = function(e) {
    log_msg("WARN", paste("Methodology HTML generation failed:", e$message))
  })

  # 6d: Methodology Word
  tryCatch({
    meth_word <- rmarkdown::render(
      input = methodology_file,
      output_format = "word_document",
      output_dir = here("output"),
      quiet = TRUE
    )
    log_msg("INFO", paste("Methodology Word generated:", meth_word))
  }, error = function(e) {
    log_msg("WARN", paste("Methodology Word generation failed:", e$message))
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
  log_msg("INFO", "Step 8: Publication run -- copying final outputs...")

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
    log_msg("WARN", "No output files found to copy for publication")
  }
} else {
  log_msg("INFO", "Not a publication run -- outputs in output/ only (gitignored)")
}

# Log completion to file
log_msg("INFO", "Pipeline completed successfully")
log_msg("INFO", paste("Log file saved to:", LOG_FILE))

cat("Log file saved to:\n")
cat("  ", LOG_FILE, "\n")
cat("\n")
cat(rep("=", 70), "\n", sep = "")
