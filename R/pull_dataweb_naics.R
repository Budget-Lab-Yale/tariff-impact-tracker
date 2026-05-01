#!/usr/bin/env Rscript
# =============================================================================
# Pull NAICS-coded customs value and calculated duties from USITC DataWeb API
# =============================================================================
#
# Replaces the manual download of "USITC - Customs and Duties - <vintage>.xlsx".
# Queries the v2 runReport endpoint and writes a CSV matching the schema the
# employment_index.R script previously consumed from the Excel file.
#
# Output: input/customs_duties_by_naics.csv with columns
#   year, month, naics, description, customs_value, calculated_duties
#
# Usage:
#   Rscript R/pull_dataweb_naics.R                  # default years (2025-current)
#   Rscript R/pull_dataweb_naics.R --years 2025,2026
#   Rscript R/pull_dataweb_naics.R --output some/path.csv
#
# Auth:
#   Reads DATAWEB_API_TOKEN from .env. Looks first in this repo, then in
#   ../tariff-rate-tracker/.env (sibling repo, where the token already lives).
#   Get a free token from https://dataweb.usitc.gov/ (API tab; requires login).
#
# Failure mode:
#   If the API call fails or the response looks corrupt (e.g., measure columns
#   appear swapped), the script stops without touching an existing output CSV.
#   Downstream scripts fall back to the cached file.
#
# Non-obvious DataWeb API quirks (discovered via UI cURL capture, 2026-05):
#   1. classificationSystem must be "NAIC" (singular, no trailing S).
#      "NAICS" returns HTTP 503 with an HTML maintenance page from the
#      dispatcher -- a routing-level rejection that looks like an outage.
#   2. Measure codes carry a CONS_ prefix: "CONS_CUSTOMS_VALUE" and
#      "CONS_CALC_DUTY" (NOT "CALC_DUTY", "CALCULATED_DUTY", etc.).
#      A third related measure, "CONS_CUSTOMS_VALUE_SUB_DUTY", is available
#      but currently unused.
#   3. Multi-measure requests return one DTO table per measure, in the order
#      passed to dataToReport. The parser depends on this positional alignment
#      (validated post-parse by validate_measure_assignment).
#   4. The body shape was reverse-engineered from a captured browser request;
#      several fields (savedQueryType, manualConversions, deletedXxxUserGroups)
#      may look extraneous but are required by DataWeb's deserializer.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(here)
})

# --- Constants ---
DATAWEB_BASE <- "https://datawebws.usitc.gov/dataweb"
RUNREPORT_PATH <- "/api/v2/report2/runReport"
MEASURES <- c("CONS_CUSTOMS_VALUE", "CONS_CALC_DUTY")
GRANULARITY <- "3"   # NAICS-3 subsector, matches naics_to_bea_crosswalk.csv

# Sanity-check thresholds for validate_measure_assignment(). Customs value
# strictly dominates calculated duty in every reasonable economy; if more
# than 5% of cells violate this, the table-to-measure positional alignment
# is suspect and we refuse to write the CSV.
INVERSION_FAIL_THRESHOLD <- 0.05

# If the new pull is more than 5% smaller than the existing CSV, log a WARN
# rather than overwrite silently. Catches partial-response or schema-shift
# regressions that would otherwise pass through unnoticed.
MIN_ROW_RETENTION_RATIO <- 0.95

INPUT_DIR <- here("input")
LOG_DIR   <- here("logs")
dir.create(INPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG_DIR,   showWarnings = FALSE, recursive = TRUE)

# --- CLI args ---
cli_args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  if (!(name %in% cli_args)) return(default)
  i <- which(cli_args == name)[1] + 1
  if (i > length(cli_args)) return(default)
  cli_args[i]
}
default_years <- paste(2025:as.integer(format(Sys.Date(), "%Y")), collapse = ",")
years_arg <- get_arg("--years", default_years)
YEARS <- sort(unique(as.integer(strsplit(years_arg, ",", fixed = TRUE)[[1]])))
if (any(is.na(YEARS)) || any(YEARS < 1990) || any(YEARS > 2100)) {
  stop("--years must be comma-separated integers (e.g., 2025,2026); got: ", years_arg)
}
OUTPUT_CSV <- get_arg("--output", file.path(INPUT_DIR, "customs_duties_by_naics.csv"))

# --- Logger ---
LOG_FILE <- file.path(LOG_DIR,
                      paste0("pull_dataweb_naics_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_msg <- function(level, msg) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 "][", level, "] ", msg, "\n")
  cat(line)
  cat(line, file = LOG_FILE, append = TRUE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Token loader ---
strip_wrapping_quotes <- function(x) {
  x <- trimws(x)
  if (grepl("^['\"].*['\"]$", x)) x <- substring(x, 2, nchar(x) - 1)
  x
}

load_token <- function() {
  candidates <- c(
    here(".env"),
    file.path(dirname(here()), "tariff-rate-tracker", ".env")
  )
  env_file <- candidates[file.exists(candidates)][1]
  if (is.na(env_file)) {
    stop("No .env with DATAWEB_API_TOKEN found.\n",
         "Looked in:\n  ", paste(candidates, collapse = "\n  "),
         "\nGet a free token from https://dataweb.usitc.gov/ (API tab).",
         call. = FALSE)
  }
  log_msg("INFO", paste("Token source:", env_file))
  lines <- readLines(env_file, warn = FALSE)
  token_line <- grep("^DATAWEB_API_TOKEN=", lines, value = TRUE)[1]
  if (is.na(token_line)) {
    stop("DATAWEB_API_TOKEN not found in ", env_file, call. = FALSE)
  }
  tok <- strip_wrapping_quotes(sub("^DATAWEB_API_TOKEN=", "", token_line))
  if (!nzchar(tok)) stop("DATAWEB_API_TOKEN is empty in ", env_file, call. = FALSE)
  tok
}

# --- Build a single-year monthly query ---
# All NAICS-3 subsectors, all countries aggregated, dual-measure (customs + duties).
build_query <- function(year) {
  list(
    savedQueryType = "",
    isOwner = TRUE,
    unitConversion = "0",
    manualConversions = list(),
    reportOptions = list(
      tradeType = "Import",
      classificationSystem = "NAIC"           # singular, per DataWeb UI
    ),
    searchOptions = list(
      MiscGroup = list(
        districts = list(
          aggregation = "Aggregate District",
          districtGroups = list(),
          districts = list(),
          districtsExpanded = list(list(name = "All Districts", value = "all")),
          districtsSelectType = "all"
        ),
        importPrograms = list(
          aggregation = NULL,
          importPrograms = list(),
          programsSelectType = "all"
        ),
        extImportPrograms = list(
          aggregation = "Aggregate CSC",
          extImportPrograms = list(),
          extImportProgramsExpanded = list(),
          programsSelectType = "all"
        ),
        provisionCodes = list(
          aggregation = "Aggregate RPCODE",
          provisionCodesSelectType = "all",
          rateProvisionCodes = list(),
          rateProvisionCodesExpanded = list(),
          rateProvisionGroups = list(systemGroups = list())
        )
      ),
      commodities = list(
        aggregation = "Break Out Commodities",
        codeDisplayFormat = "YES",
        commodities = list(),
        commoditiesExpanded = list(),
        commoditiesManual = "",
        commodityGroups = list(systemGroups = list(), userGroups = list()),
        commoditySelectType = "all",
        granularity = GRANULARITY,
        groupGranularity = NULL,
        searchGranularity = NULL,
        showHTSValidDetails = ""
      ),
      componentSettings = list(
        dataToReport = as.list(MEASURES),
        scale = "1",
        timeframeSelectType = "fullYears",
        years = list(as.character(year)),
        startDate = NULL,
        endDate = NULL,
        startMonth = NULL,
        endMonth = NULL,
        yearsTimeline = "Monthly"
      ),
      countries = list(
        aggregation = "Aggregate Countries",
        countries = list(),
        countriesExpanded = list(),
        countriesSelectType = "all",
        countryGroups = list(systemGroups = list(), userGroups = list())
      )
    ),
    sortingAndDataFormat = list(
      DataSort = list(columnOrder = list(), fullColumnOrder = list(),
                       sortOrder = list()),
      reportCustomizations = list(
        exportCombineTables = FALSE,
        totalRecords = "20000",
        exportRawData = FALSE
      )
    ),
    deletedCountryUserGroups = list(),
    deletedCommodityUserGroups = list(),
    deletedDistrictUserGroups = list()
  )
}

# --- HTTP transport with retry on transient failures ---
is_retryable_transport <- function(msg) {
  msg <- tolower(msg)
  patterns <- c("timed out", "timeout", "could not resolve host",
                "couldn't resolve host", "could not connect",
                "couldn't connect", "failed to connect",
                "connection reset", "connection was reset",
                "failure when receiving data", "recv failure", "send failure",
                "empty reply from server", "ssl connect error", "schannel",
                "network is unreachable", "http/2 stream")
  any(vapply(patterns, function(p) grepl(p, msg, fixed = TRUE), logical(1)))
}

is_retryable_http <- function(code) code %in% c(429L, 500L, 502L, 503L, 504L)

post_runreport <- function(query, token, max_retries = 4, base_wait = 5,
                            timeout_sec = 180) {
  url <- paste0(DATAWEB_BASE, RUNREPORT_PATH)
  body <- toJSON(query, auto_unbox = TRUE, null = "null", na = "null")

  for (attempt in seq_len(max_retries + 1L)) {
    resp <- tryCatch(
      POST(url,
           add_headers("Content-Type" = "application/json; charset=utf-8",
                       "Authorization" = paste("Bearer", token)),
           timeout(timeout_sec),
           user_agent("tariff-impact-tracker-dataweb"),
           body = body, encode = "raw"),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      msg <- conditionMessage(resp)
      if (attempt <= max_retries && is_retryable_transport(msg)) {
        wait <- base_wait * 2^(attempt - 1L)
        log_msg("WARN", paste0("Transport error (", strsplit(msg, "\n", fixed = TRUE)[[1]][1],
                                "); retrying in ", wait, "s [", attempt, "/", max_retries, "]"))
        Sys.sleep(wait); next
      }
      stop("DataWeb POST failed: ", msg, call. = FALSE)
    }

    code <- status_code(resp)
    if (is_retryable_http(code) && attempt <= max_retries) {
      wait <- base_wait * 2^(attempt - 1L)
      hint <- if (code == 503L) " (DataWeb maintenance: Wed 5:30-8:30 PM ET?)"
              else if (code == 429L) " (rate-limited)" else ""
      log_msg("WARN", paste0("HTTP ", code, hint,
                              "; retrying in ", wait, "s [",
                              attempt, "/", max_retries, "]"))
      Sys.sleep(wait); next
    }
    return(resp)
  }
}

# --- Parse multi-measure DTO into long format -------------------------------
# DataWeb returns one table per measure, in the order of `dataToReport`.
# Each table has:
#   column_groups[[1]]$columns -> dimension labels (NAIC Number, Year, ...)
#   column_groups[[2]]$columns -> month labels (January, February, ...)
#   row_groups[[1]]$rowsNew    -> rows; each rowEntries has n_dim + n_time entries

MONTH_NUM <- c(January = 1L, February = 2L, March = 3L, April = 4L,
               May = 5L, June = 6L, July = 7L, August = 8L,
               September = 9L, October = 10L, November = 11L, December = 12L)

clean_value <- function(s) {
  if (is.null(s) || length(s) != 1L || is.na(s) || s == "") return(NA_real_)
  as.numeric(gsub(",", "", s, fixed = TRUE))
}

parse_one_table <- function(tbl, measure_name, default_year) {
  rows <- tbl$row_groups[[1]]$rowsNew %||% list()
  if (length(rows) == 0) return(tibble())

  cgs <- tbl$column_groups
  dim_labels  <- vapply(cgs[[1]]$columns,
                        function(c) c$label %||% NA_character_, character(1))
  time_labels <- if (length(cgs) >= 2) {
    vapply(cgs[[2]]$columns, function(c) c$label %||% NA_character_, character(1))
  } else character(0)

  naic_idx <- which(grepl("^NAIC", dim_labels, ignore.case = TRUE))[1]
  year_idx <- which(dim_labels == "Year")[1]
  desc_idx <- which(grepl("^Desc", dim_labels, ignore.case = TRUE))[1]
  if (is.na(naic_idx)) {
    stop("DataWeb table has no NAIC dimension column. Dim labels: ",
         paste(dim_labels, collapse = " | "), call. = FALSE)
  }

  n_dims <- length(dim_labels)
  n_times <- length(time_labels)
  month_nums <- MONTH_NUM[time_labels]   # NA for non-month labels

  out <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    entries <- rows[[i]]$rowEntries
    naic <- entries[[naic_idx]]$value %||% NA_character_
    yr <- if (!is.na(year_idx)) {
      suppressWarnings(as.integer(entries[[year_idx]]$value %||% NA_character_))
    } else default_year
    desc <- if (!is.na(desc_idx)) entries[[desc_idx]]$value %||% NA_character_ else NA_character_
    if (is.na(naic) || nchar(naic) == 0) next

    # Time-column values follow the dimension columns. Guard against the
    # backwards-sequence trap when length(entries) <= n_dims (no time cols).
    if (length(entries) <= n_dims) next
    val_entries <- entries[(n_dims + 1L):length(entries)]
    if (length(val_entries) == 0L) next

    # Pad month_nums to match val_entries length so positional alignment holds
    # even when DataWeb returns extra trailing columns we don't recognize.
    n_vals <- length(val_entries)
    months_padded <- month_nums[seq_len(min(n_times, n_vals))]
    vals <- vapply(val_entries[seq_along(months_padded)],
                   function(e) clean_value(e$value), numeric(1))

    out[[i]] <- tibble(
      year = yr,
      month = months_padded,
      naics = naic,
      description = desc,
      measure = measure_name,
      value = vals
    ) |> filter(!is.na(month), !is.na(value))
  }
  bind_rows(Filter(Negate(is.null), out))
}

parse_dto <- function(dto, measures, default_year) {
  if (length(dto$errors) > 0) {
    stop("DataWeb DTO error: ",
         paste(unlist(dto$errors), collapse = "; "), call. = FALSE)
  }
  tables <- dto$tables
  if (length(tables) == 0) return(tibble())
  if (length(tables) < length(measures)) {
    stop("DataWeb returned ", length(tables), " tables but ", length(measures),
         " measures were requested. ",
         "Cannot establish positional measure-to-table alignment; refusing to ",
         "produce a partial output.", call. = FALSE)
  }
  if (length(tables) > length(measures)) {
    log_msg("WARN", paste0("DataWeb returned ", length(tables),
                            " tables but only ", length(measures),
                            " measures requested; using first ",
                            length(measures), " positionally"))
  }
  parts <- lapply(seq_along(measures), function(i) {
    parse_one_table(tables[[i]], measures[i], default_year)
  })
  bind_rows(parts)
}

# --- Hard-validate measure-to-table alignment -------------------------------
# DataWeb returns one DTO table per measure in the order of dataToReport, and
# the parser relies on that positional alignment. If the order ever shifts,
# customs_value and calculated_duties get silently swapped in the output --
# downstream tau_c rates would be ~95% instead of ~10%. Refuse to write the
# CSV when the heuristic detects inversion.
#
# Heuristic: at the row level, customs value strictly dominates calculated
# duty in every reasonable economy. If duty > customs_value in more than 5%
# of nonzero cells, something is wrong (either swapped tables or genuinely
# corrupt data) -- in either case we should stop, not warn.
validate_measure_assignment <- function(long_df) {
  required <- c("CONS_CUSTOMS_VALUE", "CONS_CALC_DUTY")
  present  <- unique(long_df$measure)
  missing  <- setdiff(required, present)
  if (length(missing) > 0) {
    stop("Long-form data is missing required measures: ",
         paste(missing, collapse = ", "),
         "\n  Present: ", paste(present, collapse = ", "), call. = FALSE)
  }

  wide <- long_df |>
    pivot_wider(names_from = measure, values_from = value, values_fill = 0)
  ok_cells <- wide |>
    filter(.data$CONS_CUSTOMS_VALUE > 0 | .data$CONS_CALC_DUTY > 0)
  if (nrow(ok_cells) == 0) {
    stop("No nonzero cells in DataWeb response -- API likely degraded.",
         call. = FALSE)
  }
  inverted <- mean(ok_cells$CONS_CALC_DUTY > ok_cells$CONS_CUSTOMS_VALUE,
                   na.rm = TRUE)
  log_msg("INFO", sprintf("  Sanity: cells with duty > customs_value: %.2f%%",
                           inverted * 100))
  if (inverted > INVERSION_FAIL_THRESHOLD) {
    stop(sprintf(
      paste0("Measure-to-table assignment failed sanity check: duty > customs_value ",
             "in %.1f%% of nonzero cells (threshold: %.1f%%). DataWeb may have reordered ",
             "its multi-measure response, or the data is corrupt. Refusing to write ",
             "the CSV.\n  Investigate parse_one_table() ordering or the dataToReport ",
             "field in build_query()."),
      inverted * 100, INVERSION_FAIL_THRESHOLD * 100), call. = FALSE)
  }
}

# --- Main pull loop ---------------------------------------------------------
log_msg("INFO", strrep("=", 60))
log_msg("INFO", "DataWeb NAICS pull")
log_msg("INFO", strrep("=", 60))
log_msg("INFO", paste("Years:    ", paste(YEARS, collapse = ", ")))
log_msg("INFO", paste("Output:   ", OUTPUT_CSV))
log_msg("INFO", paste("Measures: ", paste(MEASURES, collapse = ", ")))

token <- load_token()
log_msg("INFO", paste("Token: ", if (nzchar(token)) "loaded" else "missing"))

all_long <- list()
for (i in seq_along(YEARS)) {
  yr <- YEARS[i]
  log_msg("INFO", paste0("Pulling ", yr, " (monthly, all NAICS-",
                          GRANULARITY, ", all countries)..."))
  q <- build_query(yr)
  t0 <- Sys.time()
  resp <- post_runreport(q, token)
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  code <- status_code(resp)

  if (code != 200) {
    body_txt <- tryCatch(content(resp, as = "text", encoding = "UTF-8"),
                         error = function(e) "")
    snippet <- if (grepl("<!DOCTYPE html>", body_txt, fixed = TRUE)) "<HTML 503/maintenance>"
               else substr(gsub("\\s+", " ", body_txt), 1, 200)
    log_msg("ERROR", paste0("HTTP ", code, " for year ", yr, ": ", snippet))
    stop("DataWeb returned HTTP ", code, " for year ", yr,
         "; not overwriting existing CSV.", call. = FALSE)
  }

  parsed <- content(resp, as = "parsed", simplifyVector = FALSE)
  long_yr <- parse_dto(parsed$dto, MEASURES, default_year = yr)
  log_msg("INFO", sprintf("  %d (%.1fs): %d long-form rows",
                           yr, dt, nrow(long_yr)))
  all_long[[as.character(yr)]] <- long_yr
  if (i < length(YEARS)) Sys.sleep(0.5)   # polite, but not after the last year
}

long_df <- bind_rows(all_long)
if (nrow(long_df) == 0) {
  stop("DataWeb returned no data across requested years (", paste(YEARS, collapse = ", "),
       "). Not overwriting existing CSV.", call. = FALSE)
}

validate_measure_assignment(long_df)

# Carry one description per NAICS code (collapse across measures, where the
# description is duplicated). DataWeb sometimes returns blank descriptions for
# certain codes; pick the first non-empty one we see.
descriptions <- long_df |>
  filter(!is.na(description), nchar(description) > 0) |>
  distinct(naics, description) |>
  group_by(naics) |>
  summarise(description = first(description), .groups = "drop")

# Pivot wide on measure -> final schema
wide <- long_df |>
  select(-description) |>
  pivot_wider(names_from = measure, values_from = value, values_fill = 0) |>
  left_join(descriptions, by = "naics") |>
  rename(customs_value = CONS_CUSTOMS_VALUE,
         calculated_duties = CONS_CALC_DUTY) |>
  arrange(year, month, naics) |>
  select(year, month, naics, description, customs_value, calculated_duties)

# Drop fully-empty rows (NAIC × month with no trade in either measure)
wide <- wide |> filter(customs_value > 0 | calculated_duties > 0)

# Regression check: if an existing CSV is present, refuse to silently shrink it.
# A genuine schema change (e.g., a NAICS code disappearing from the universe)
# is rare; usually a smaller output means the new pull missed something.
if (file.exists(OUTPUT_CSV)) {
  prior_rows <- tryCatch(
    nrow(readr::read_csv(OUTPUT_CSV, show_col_types = FALSE,
                          progress = FALSE)),
    error = function(e) NA_integer_
  )
  if (!is.na(prior_rows) && nrow(wide) < prior_rows * MIN_ROW_RETENTION_RATIO) {
    log_msg("WARN", sprintf(
      "New output (%d rows) is more than %.0f%% smaller than existing CSV (%d rows).",
      nrow(wide), (1 - MIN_ROW_RETENTION_RATIO) * 100, prior_rows
    ))
    log_msg("WARN", "Inspect the diff before treating this run as authoritative.")
  }
}

log_msg("INFO", "Output summary:")
log_msg("INFO", sprintf("  Rows:        %d", nrow(wide)))
log_msg("INFO", sprintf("  NAICS codes: %d", length(unique(wide$naics))))
ym <- sort(unique(paste0(wide$year, "-", sprintf("%02d", wide$month))))
log_msg("INFO", sprintf("  Year-months: %s", paste(ym, collapse = ", ")))
log_msg("INFO", sprintf("  Total customs value: $%.1fB",
                         sum(wide$customs_value, na.rm = TRUE) / 1e9))
log_msg("INFO", sprintf("  Total calc duties:   $%.1fB",
                         sum(wide$calculated_duties, na.rm = TRUE) / 1e9))

write_csv(wide, OUTPUT_CSV)
log_msg("INFO", paste("Wrote:", OUTPUT_CSV))
log_msg("INFO", "Done.")
