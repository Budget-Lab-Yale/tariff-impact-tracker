# ==============================================================================
# Tariff Impacts - Report Setup (Shared Data Loading & Computation)
# ==============================================================================
#
# Purpose: Shared setup for report, Drupal output, and data export scripts.
#          Loads all data, computes trends, summary stats, and passthrough tables.
#
# Usage:
#   source(here("R", "report_setup.R"))
#
# Consumers:
#   - R/tariff_impacts_report.Rmd  (review document)
#   - R/tariff_impacts_drupal.Rmd  (Drupal HTML fragment)
#   - R/export_website_data.R      (Excel + CSV exports)
#
# Author: John Iselin, Yale Budget Lab
# Date: March 2026
# ==============================================================================

# Guard against double-sourcing
if (exists(".REPORT_SETUP_LOADED") && isTRUE(.REPORT_SETUP_LOADED)) {
  message("report_setup.R already loaded, skipping.")
  return(invisible(NULL))
}

# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(scales)
library(knitr)
library(kableExtra)
library(flextable)
library(here)
library(sandwich)  # Newey-West standard errors
library(lmtest)    # Coefficient testing with robust SEs
library(patchwork) # Multi-panel figures

# ==============================================================================
# CROSS-DOCUMENT LINKS
# ==============================================================================

meth_text <- "accompanying Methodology page"
meth_link <- "https://budgetlab.yale.edu/research/methodological-appendix-tracking-economic-effects-tariffs"

# ==============================================================================
# FORMAT-AWARE TABLE RENDERER
# ==============================================================================

render_table <- function(df, caption, align = NULL, header_above = NULL) {
  is_word <- !is.null(knitr::pandoc_to()) && knitr::pandoc_to() == "docx"
  if (is_word) {
    ft <- flextable(df) %>%
      set_caption(caption) %>%
      autofit() %>%
      theme_vanilla() %>%
      fontsize(size = 9, part = "all") %>%
      bold(part = "header")
    if (!is.null(header_above)) {
      col_keys <- colnames(df)
      mapping <- character(length(col_keys))
      pos <- 1
      for (i in seq_along(header_above)) {
        span <- header_above[i]
        grp_name <- names(header_above)[i]
        if (is.null(grp_name) || grp_name == " " || grp_name == "") grp_name <- ""
        for (j in seq_len(span)) {
          mapping[pos] <- grp_name
          pos <- pos + 1
        }
      }
      header_vals <- setNames(as.list(mapping), col_keys)
      ft <- ft %>%
        add_header_row(values = header_vals, top = TRUE) %>%
        merge_h(part = "header") %>%
        align(align = "center", part = "header") %>%
        hline(i = 1, part = "header", border = officer::fp_border(width = 0))
    }
    ft
  } else {
    k <- kable(df, caption = caption, align = align)
    k <- k %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
    if (!is.null(header_above)) {
      k <- k %>% add_header_above(header_above)
    }
    k
  }
}

# ==============================================================================
# GGPLOT THEME & COLORS
# ==============================================================================

theme_set(theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ))

tbl_gray <- "#6C757D"
color_single <- "black"
color_series1 <- "#001f3f"  # Navy
color_series2 <- "#FF851B"  # Orange
tbl_colors <- c("#0055A4", "#C8102E", "#28A745", "#FFC107", "#6C757D")

# ==============================================================================
# FIGURE/TABLE METADATA REGISTRY
# ==============================================================================

FIGURE_METADATA <- list(
  F1 = list(
    label    = "Figure 1",
    title    = "Daily Effective Tariff Rate (Import-Weighted)",
    subtitle = "Overall import-weighted effective tariff rate by calendar day",
    source   = "The Budget Lab Tariff Rate Tracker (USITC HTS data, Census 2024 import weights)",
    notes    = "Vertical lines indicate major policy changes. Rates change only at policy revision boundaries.",
    y_label  = "Percent"
  ),
  F2 = list(
    label    = "Figure 2",
    title    = "Effective Tariff Rate",
    subtitle = "Customs duties as a share of import value",
    source   = "U.S. Treasury, Census Bureau via Haver Analytics",
    notes    = "",
    y_label  = "Percent"
  ),
  F3 = list(
    label    = "Figure 3",
    title    = "Customs Duty Revenue (Inflation-Adjusted)",
    subtitle = "Monthly federal customs duty revenue in 2025 dollars, deflated by CPI-U",
    source   = "U.S. Treasury via Haver Analytics, BLS CPI",
    notes    = "October 2025 CPI interpolated (government shutdown delayed BLS release).",
    y_label  = "Billions of Dollars (Real)"
  ),
  F4 = list(
    label    = "Figure 4",
    title    = "PCE Core Goods & Durables Prices",
    subtitle = "Index (Dec 2024 = 100) versus local projection with 90% CIs",
    source   = "Bureau of Economic Analysis via Haver Analytics and The Budget Lab analysis",
    notes    = "",
    y_label  = "Index"
  ),
  F5 = list(
    label    = "Figure 5",
    title    = "PCE Goods Prices: Deviation from Trend",
    subtitle = "Percent difference from trend (local projection)",
    source   = "Bureau of Economic Analysis via Haver Analytics and The Budget Lab analysis",
    notes    = "",
    y_label  = "Percent vs. Trend"
  ),
  F6 = list(
    label    = "Figure 6",
    title    = "Imported PCE Core Goods & Durables Prices",
    subtitle = "Import-content-weighted index (Dec 2024 = 100) versus local projection with 90% CIs",
    source   = "BEA PCE components via Haver Analytics, BEA I-O Tables, and The Budget Lab analysis",
    notes    = "",
    y_label  = "Index"
  ),
  F7 = list(
    label    = "Figure 7",
    title    = "Imported PCE Goods Prices: Deviation from Trend",
    subtitle = "Percent difference from trend (import-content-weighted index)",
    source   = "BEA PCE components via Haver Analytics, BEA I-O Tables, and The Budget Lab analysis",
    notes    = "",
    y_label  = "Percent vs. Trend"
  ),
  T1a = list(
    label    = "Table 1a",
    title    = "Implied Passthrough -- Imported PCE Core and Durable Goods Prices (June 2025)",
    subtitle = "Passthrough calculation components, estimated price change, and estimated passthrough rates",
    source   = "Haver Analytics, USITC, BEA, and The Budget Lab analysis",
    notes    = paste0(
      "LP (Local Projection): Price deviation from trend estimated using local projections ",
      "with recession/pandemic controls (1996-2024 estimation period). ",
      "Log-Linear: Price deviation from simple log-linear trend fit to 2023-2024 data. ",
      "Simple: Raw price change with no trend adjustment.")
  ),
  T1b = list(
    label    = "Table 1b",
    title    = "Implied Passthrough -- Imported PCE Core and Durable Goods Prices",
    subtitle = "Passthrough calculation components, estimated price change, and estimated passthrough rates",
    source   = "Haver Analytics, USITC, BEA, and The Budget Lab analysis",
    notes    = paste0(
      "LP (Local Projection): Price deviation from trend estimated using local projections ",
      "with recession/pandemic controls (1996-2024 estimation period). ",
      "Log-Linear: Price deviation from simple log-linear trend fit to 2023-2024 data. ",
      "Simple: Raw price change with no trend adjustment.")
  ),
  F8 = list(
    label    = "Figure 8",
    title    = "Non-Petroleum Import Prices",
    subtitle = "Index (Dec 2024 = 100) versus simple linear projection",
    source   = "Bureau of Labor Statistics via Haver Analytics",
    notes    = "October 2025 import prices interpolated (government shutdown delayed BLS release). Simple linear projection based on 2023-2024 data.",
    y_label  = "Index"
  ),
  F9 = list(
    label    = "Figure 9",
    title    = "Tariff-Exposed Employment Index",
    subtitle = "Leontief-adjusted total import content weighting, index (Dec 2024 = 100)",
    source   = "BLS via Haver, USITC DataWeb, BEA I-O Tables (including Total Requirements), and The Budget Lab analysis",
    notes    = "",
    y_label  = "Index"
  ),
  F10 = list(
    label    = "Figure 10",
    title    = "Manufacturing Tariff-Exposed Employment Index",
    subtitle = "Leontief-adjusted total import content weighting, index (Dec 2024 = 100)",
    source   = "BLS via Haver, USITC DataWeb, BEA I-O Tables (including Total Requirements), and The Budget Lab analysis",
    notes    = "",
    y_label  = "Index"
  ),
  T_Emp = list(
    label    = "Table",
    title    = "Employment Index Summary",
    subtitle = "Tariff-exposed employment index statistics (most recent tariff rates)",
    source   = "BLS via Haver, USITC, BEA I-O Tables",
    notes    = ""
  ),
  F11 = list(
    label    = "Figure 11",
    title    = "Manufacturing Industrial Production",
    subtitle = "Index (Dec 2024 = 100), simple linear trend fit to 2023-2024",
    source   = "Federal Reserve via Haver Analytics",
    notes    = "",
    y_label  = "Index"
  ),
  F12 = list(
    label    = "Figure 12",
    title    = "Nominal Effective Exchange Rates (Daily)",
    subtitle = "BIS broad NEER index (Dec 2024 average = 100), increase = appreciation",
    source   = "Haver Analytics (INTDAILY)",
    notes    = "",
    y_label  = "Index"
  ),
  F13 = list(
    label    = "Figure 13",
    title    = "Real US Imports and Exports",
    subtitle = "Billions of 2025 USD, simple linear trend (2023-2024)",
    source   = "Census Bureau via Haver Analytics",
    notes    = "",
    y_label  = "Billions USD"
  ),
  F14 = list(
    label    = "Figure 14",
    title    = "Trade Deviation from Trend",
    subtitle = "Percent difference from pre-2025 trend",
    source   = "Census Bureau via Haver Analytics",
    notes    = "",
    y_label  = "Percent vs. Trend"
  ),
  F15 = list(
    label    = "Figure 15",
    title    = "Cumulative Import Gap vs. Trend",
    subtitle = "Running total of monthly import gap since Dec 2024 (billions 2025 USD)",
    source   = "Census Bureau via Haver Analytics",
    notes    = "",
    y_label  = "Billions USD"
  ),
  TA1 = list(
    label    = "Table A1",
    title    = "Implied Consumer Passthrough -- All PCE Goods (June 2025)",
    subtitle = "Economy-wide effective tariff rate",
    source   = "Haver Analytics, USITC, The Budget Lab analysis",
    notes    = ""
  ),
  TA2 = list(
    label    = "Table A2",
    title    = "Implied Consumer Passthrough -- All PCE Goods",
    subtitle = "Economy-wide effective tariff rate",
    source   = "Haver Analytics, USITC, The Budget Lab analysis",
    notes    = ""
  ),
  FA1 = list(
    label    = "Figure A1",
    title    = "Daily Effective Tariff Rate by Authority",
    subtitle = "Import-weighted ETR decomposed by tariff authority",
    source   = "The Budget Lab Tariff Rate Tracker (USITC HTS data, Census 2024 import weights)",
    notes    = "Authorities: Section 232 (steel/aluminum/autos), Section 301 (China), IEEPA (reciprocal/fentanyl), Section 122 (post-SCOTUS). Section 201 omitted (negligible).",
    y_label  = "Percent"
  ),
  FA2 = list(
    label    = "Figure A2",
    title    = "PCE Core Goods & Durables Prices (Log-Linear Trend)",
    subtitle = "Index (Dec 2024 = 100), log-linear trend (2023-2024 estimation) with 90% CIs",
    source   = "Bureau of Economic Analysis via Haver Analytics and The Budget Lab analysis",
    notes    = "",
    y_label  = "Index"
  ),
  FA3 = list(
    label    = "Figure A3",
    title    = "PCE Goods Prices: Deviation from Log-Linear Trend",
    subtitle = "Percent difference from trend (log-linear, 2023-2024 estimation)",
    source   = "Bureau of Economic Analysis via Haver Analytics and The Budget Lab analysis",
    notes    = "",
    y_label  = "Percent vs. Trend"
  )
)

# Helper: retrieve a single metadata field
fig_meta <- function(id, field) {
  val <- FIGURE_METADATA[[id]][[field]]
  if (is.null(val)) "" else val
}

# Helper: build "Figure N. Title" for Excel sheet headers
fig_excel_title <- function(id) {
  paste0(fig_meta(id, "label"), ". ", fig_meta(id, "title"))
}

# Helper: build ggplot caption from source + notes
fig_caption <- function(id) {
  src <- fig_meta(id, "source")
  notes <- fig_meta(id, "notes")
  cap <- paste0("Source: ", src)
  if (nzchar(notes)) cap <- paste0(cap, "\nNote: ", notes)
  cap
}

# Helper: full passthrough table notes (static methodology + dynamic tariff rates)
# Called lazily -- dynamic variables (tau_core_recent, etc.) must exist at call time
passthrough_notes <- function(id) {
  base <- fig_meta(id, "notes")
  rate_info <- tryCatch({
    sprintf(
      paste0("Tariff rates are category-specific effective rates from USITC trade data: ",
             "core goods %.1f%%, durables %.1f%% (vs %.1f%% baseline). ",
             "Economy-wide effective rate as of %s: %.1f%%."),
      tau_core_recent, tau_dur_recent, BASELINE_TARIFF_RATE * 100,
      data_month, latest_tariff)
  }, error = function(e) "")
  if (nzchar(rate_info)) paste0(base, " ", rate_info) else base
}

# ==============================================================================
# DYNAMIC DATE HELPERS
# ==============================================================================

get_base_date <- function(df, date_col = "date") {
  max_date <- max(df[[date_col]], na.rm = TRUE)
  target_year <- year(max_date) - 1
  target_date <- as.Date(paste0(target_year, "-12-01"))
  available_dates <- df[[date_col]][!is.na(df[[date_col]])]
  closest_idx <- which.min(abs(available_dates - target_date))
  available_dates[closest_idx]
}

get_base_value <- function(df, value_col, base_date, date_col = "date") {
  df_valid <- df[!is.na(df[[value_col]]), ]
  if (nrow(df_valid) == 0) return(NA)
  date_diffs <- abs(as.numeric(df_valid[[date_col]] - as.Date(base_date)))
  closest_idx <- which.min(date_diffs)
  df_valid[[value_col]][closest_idx]
}

# ==============================================================================
# DATA LOADING
# ==============================================================================

OUTPUT_DIR <- here("output")

# Source shared utility functions (includes safe_divide, local projection, and trend functions)
source(here("R", "utils.R"))

# Core data files - normalize all dates to first-of-month
tariff_revenue <- read_csv(file.path(OUTPUT_DIR, "tariff_revenue.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
import_shares <- read_csv(file.path(OUTPUT_DIR, "import_shares.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
pce_prices <- read_csv(file.path(OUTPUT_DIR, "pce_prices.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
import_prices <- read_csv(file.path(OUTPUT_DIR, "import_prices.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
employment <- read_csv(file.path(OUTPUT_DIR, "employment.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
industrial_production <- read_csv(file.path(OUTPUT_DIR, "industrial_production.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
trade_flows <- read_csv(file.path(OUTPUT_DIR, "trade_flows.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
fed_policy <- read_csv(file.path(OUTPUT_DIR, "fed_policy.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
exchange_rates <- read_csv(file.path(OUTPUT_DIR, "exchange_rates.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
twi_long <- read_csv(file.path(OUTPUT_DIR, "twi_long.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()
cpi_data <- read_csv(file.path(OUTPUT_DIR, "cpi_data.csv"), show_col_types = FALSE) %>% normalize_monthly_dates()

# Interpolate missing months (Oct 2025 CPI unavailable due to government shutdown)
cpi_cols <- c("cpi_all", "cpi_durables", "cpi_core_goods", "cpi_services_less_energy")
for (col in cpi_cols) {
  na_idx <- which(is.na(cpi_data[[col]]))
  for (i in na_idx) {
    if (i > 1 && i < nrow(cpi_data) &&
        !is.na(cpi_data[[col]][i - 1]) && !is.na(cpi_data[[col]][i + 1])) {
      cpi_data[[col]][i] <- (cpi_data[[col]][i - 1] + cpi_data[[col]][i + 1]) / 2
    }
  }
}

# Similarly interpolate missing import price data (Oct 2025, government shutdown)
ip_cols <- c("import_price_nonpetroleum", "import_price_all")
for (col in ip_cols) {
  na_idx <- which(is.na(import_prices[[col]]))
  for (i in na_idx) {
    if (i > 1 && i < nrow(import_prices) &&
        !is.na(import_prices[[col]][i - 1]) && !is.na(import_prices[[col]][i + 1])) {
      import_prices[[col]][i] <- (import_prices[[col]][i - 1] + import_prices[[col]][i + 1]) / 2
    }
  }
}

# Daily exchange rates (from INTDAILY)
fx_daily_file <- file.path(OUTPUT_DIR, "exchange_rates_daily.csv")
fx_daily_available <- file.exists(fx_daily_file)
if (fx_daily_available) {
  fx_daily <- read_csv(fx_daily_file, show_col_types = FALSE) %>%
    dplyr::mutate(date = as.Date(date))
}

# Optional: Daily ETR series (from tariff-rate-tracker)
TARIFF_TRACKER_DIR <- file.path(dirname(here()), "tariff-rate-tracker", "output", "daily")
daily_etr_file <- file.path(TARIFF_TRACKER_DIR, "daily_overall.csv")
daily_auth_file <- file.path(TARIFF_TRACKER_DIR, "daily_by_authority.csv")
daily_etr_available <- file.exists(daily_etr_file) && file.exists(daily_auth_file)
if (daily_etr_available) {
  daily_etr <- read_csv(daily_etr_file, show_col_types = FALSE) %>%
    dplyr::mutate(date = as.Date(date))
  daily_auth <- read_csv(daily_auth_file, show_col_types = FALSE) %>%
    dplyr::mutate(date = as.Date(date))

  POLICY_EVENTS <- tribble(
    ~date,              ~label,
    "2025-02-04",       "Fentanyl",
    "2025-03-12",       "232 Autos",
    "2025-04-02",       "Liberation Day",
    "2025-04-09",       "Phase 1 Pause",
    "2025-07-01",       "Phase 2",
    "2025-08-07",       "Phase 2 Recip.",
    "2026-02-24",       "SCOTUS / S.122"
  ) %>% dplyr::mutate(date = as.Date(date))
}

# Latest statutory tariff rate for prose
latest_etr_pct <- if (daily_etr_available) {
  daily_etr %>% filter(date <= Sys.Date()) %>% filter(date == max(date)) %>% pull(weighted_etr) * 100
} else {
  NA_real_
}

# Optional: Tariff employment index (from employment_index.R)
tariff_emp_file <- file.path(OUTPUT_DIR, "employment_index_I_t.csv")
tariff_emp_available <- file.exists(tariff_emp_file)
if (tariff_emp_available) {
  tariff_emp <- read_csv(tariff_emp_file, show_col_types = FALSE) %>% normalize_monthly_dates()
  if (nrow(tariff_emp) == 0) tariff_emp_available <- FALSE
}

# Optional: Manufacturing-only employment index
tariff_emp_mfg_file <- file.path(OUTPUT_DIR, "employment_index_I_t_manufacturing.csv")
tariff_emp_mfg_available <- file.exists(tariff_emp_mfg_file)
if (tariff_emp_mfg_available) {
  tariff_emp_mfg <- read_csv(tariff_emp_mfg_file, show_col_types = FALSE) %>% normalize_monthly_dates()
  if (nrow(tariff_emp_mfg) == 0) tariff_emp_mfg_available <- FALSE
}

# Optional: Imported PCE Goods Price Index (from import_price_index.R)
ipi_file <- file.path(OUTPUT_DIR, "import_price_index.csv")
ipi_shares_file <- file.path(OUTPUT_DIR, "ipi_effective_shares.csv")
ipi_available <- file.exists(ipi_file) && file.exists(ipi_shares_file)
if (ipi_available) {
  ipi_data <- read_csv(ipi_file, show_col_types = FALSE) %>% normalize_monthly_dates()
  ipi_eff_shares <- read_csv(ipi_shares_file, show_col_types = FALSE)
  IPI_EFF_SHARE_CORE <- ipi_eff_shares$effective_import_share[ipi_eff_shares$variant == "core_goods"]
  IPI_EFF_SHARE_DUR <- ipi_eff_shares$effective_import_share[ipi_eff_shares$variant == "durables"]
}

# Import content shares for passthrough calculations
if (ipi_available) {
  IMPORT_SHARE_CORE_GOODS <- ipi_eff_shares$effective_import_share[ipi_eff_shares$variant == "core_goods"]
  IMPORT_SHARE_DURABLES <- ipi_eff_shares$effective_import_share[ipi_eff_shares$variant == "durables"]
} else {
  IMPORT_SHARE_CORE_GOODS <- 0.25
  IMPORT_SHARE_DURABLES <- 0.30
}

# ==============================================================================
# BASELINE & TREND FUNCTIONS
# ==============================================================================

BASELINE_TARIFF_RATE <- tariff_revenue %>%
  filter(year >= 2022, year <= 2024) %>%
  summarize(rate = mean(effective_rate, na.rm = TRUE) / 100) %>%
  pull(rate)

calc_trend <- function(df, value_col, trend_start = "2023-01-01", trend_end = "2024-12-01") {
  trend_data <- df %>%
    filter(date >= as.Date(trend_start) & date <= as.Date(trend_end))
  if (nrow(trend_data) < 2) return(df)

  trend_data$time_idx <- as.numeric(trend_data$date - min(trend_data$date))
  fit <- lm(as.formula(paste(value_col, "~ time_idx")), data = trend_data)

  df %>%
    mutate(
      time_idx = as.numeric(date - as.Date(trend_start)),
      trend = predict(fit, newdata = data.frame(time_idx = time_idx)),
      vs_trend = (.data[[value_col]] / trend - 1) * 100
    )
}

# ==============================================================================
# FORMATTING HELPERS
# ==============================================================================

safe_fmt <- function(x, fmt = "%.1f", default = "N/A") {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
  sprintf(fmt, x[1])
}

format_date_long <- function(d = Sys.Date()) {
  day <- as.integer(format(d, "%d"))
  suffix <- if (day %% 100 %in% 11:13) "th"
            else switch(as.character(day %% 10),
                        "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(format(d, "%A, %B "), day, suffix, ", ", format(d, "%Y"))
}

fmt_pct <- function(x) {
  if (length(x) == 0 || is.na(x)) return("N/A")
  sprintf("%.1f%%", x)
}

fmt_pt <- function(x) {
  if (length(x) == 0 || is.na(x)) return("N/A")
  sprintf("%.0f%%", x)
}

# Format monthly dates for CSV
format_csv_date <- function(dates) {
  m <- format(dates, "%b")
  y <- format(dates, "%Y")
  show_year <- seq_along(dates) == 1 | month(dates) == 1
  ifelse(show_year, paste(m, y), m)
}

format_csv_date_line <- function(dates) {
  format(dates, "%Y-%m")
}

# ==============================================================================
# SUMMARY STATISTICS FOR INLINE TEXT
# ==============================================================================

# Latest CPI for inflation adjustment
latest_cpi_val <- cpi_data %>%
  filter(!is.na(cpi_all)) %>%
  filter(date == max(date)) %>%
  pull(cpi_all)

# Revenue statistics (consolidated -- single computation of revenue_real)
revenue_with_cpi <- tariff_revenue %>%
  left_join(cpi_data %>% dplyr::select(date, cpi_all), by = "date") %>%
  filter(!is.na(cpi_all)) %>%
  mutate(customs_real = customs_duties * (latest_cpi_val / cpi_all))

pre_2025_tariff_rate <- BASELINE_TARIFF_RATE * 100
pre_2025_monthly_rev <- revenue_with_cpi %>%
  filter(date >= as.Date("2022-01-01") & date < as.Date("2025-01-01")) %>%
  summarize(avg = mean(customs_real, na.rm = TRUE)) %>%
  pull(avg) / 1000

revenue_2025_data <- revenue_with_cpi %>%
  filter(date >= as.Date("2025-01-01"))

total_excess_rev <- sum(revenue_2025_data$customs_real - (pre_2025_monthly_rev * 1000), na.rm = TRUE) / 1000
months_of_2025 <- nrow(revenue_2025_data)

excess_rev_2025_only <- revenue_2025_data %>%
  filter(date < as.Date("2026-01-01")) %>%
  summarize(excess = sum(customs_real - (pre_2025_monthly_rev * 1000), na.rm = TRUE)) %>%
  pull(excess) / 1000

latest_rev_row <- revenue_2025_data %>% filter(date == max(date))
latest_month_rev <- (latest_rev_row$customs_real - pre_2025_monthly_rev * 1000) / 1000
latest_month_name <- format(latest_rev_row$date, "%B %Y")

latest_tariff_row <- revenue_2025_data %>%
  filter(!is.na(effective_rate)) %>%
  filter(date == max(date))
latest_tariff_rate <- latest_tariff_row$effective_rate
latest_tariff_month <- format(latest_tariff_row$date, "%B %Y")

recent_2mo <- revenue_2025_data %>%
  arrange(desc(date)) %>%
  slice(1:2)
avg_monthly_2mo_rev <- mean(recent_2mo$customs_real, na.rm = TRUE) / 1000
annualized_rev <- avg_monthly_2mo_rev * 12
annualized_rev_months <- paste(format(min(recent_2mo$date), "%B"), "and", format(max(recent_2mo$date), "%B"))

latest_month_annualized <- latest_rev_row$customs_real / 1000 * 12
latest_rev_date <- format(max(revenue_2025_data$date), "%B %Y")

data_month_name <- format(max(pce_prices$date), "%B")

# PCE price statistics
pce_2025 <- pce_prices %>%
  filter(date >= as.Date("2025-01-01")) %>%
  filter(!is.na(pce_core_goods))

pce_dec_2024 <- pce_prices %>%
  filter(year(date) == 2024, month(date) == 12) %>%
  slice(1)

if (nrow(pce_dec_2024) == 0) {
  pce_dec_2024 <- pce_prices %>%
    filter(date < as.Date("2025-01-01")) %>%
    filter(date == max(date))
}

months_since_2025 <- nrow(pce_2025)

latest_pce <- pce_2025 %>% filter(date == max(date))

core_ytd_change <- if (nrow(pce_dec_2024) > 0 && nrow(latest_pce) > 0) {
  (latest_pce$pce_core_goods[1] / pce_dec_2024$pce_core_goods[1] - 1) * 100
} else { NA }

dur_ytd_change <- if (nrow(pce_dec_2024) > 0 && nrow(latest_pce) > 0) {
  (latest_pce$pce_durables[1] / pce_dec_2024$pce_durables[1] - 1) * 100
} else { NA }

comparison_month <- if (nrow(pce_2025) > 0) month(max(pce_2025$date)) else 6

pce_dec_2022 <- pce_prices %>%
  filter(year(date) == 2022, month(date) == 12) %>%
  slice(1)

pce_2023_same <- pce_prices %>%
  filter(year(date) == 2023, month(date) == comparison_month) %>%
  slice(1)

core_2023_change <- if (nrow(pce_dec_2022) > 0 && nrow(pce_2023_same) > 0) {
  (pce_2023_same$pce_core_goods[1] / pce_dec_2022$pce_core_goods[1] - 1) * 100
} else { NA }

dur_2023_change <- if (nrow(pce_dec_2022) > 0 && nrow(pce_2023_same) > 0) {
  (pce_2023_same$pce_durables[1] / pce_dec_2022$pce_durables[1] - 1) * 100
} else { NA }

# Passthrough estimates for Key Takeaways (simple + LP methods)
takeaway_tariff_increase <- latest_tariff_rate / 100 - BASELINE_TARIFF_RATE

tk_expected_core <- takeaway_tariff_increase * IMPORT_SHARE_CORE_GOODS * 100
tk_expected_dur  <- takeaway_tariff_increase * IMPORT_SHARE_DURABLES * 100

tk_pt_core_simple <- safe_divide(core_ytd_change, tk_expected_core) * 100
tk_pt_dur_simple  <- safe_divide(dur_ytd_change, tk_expected_dur) * 100

# Method 2: LP trend passthrough
pce_for_takeaway <- pce_prices %>% filter(date >= "1996-01-01")

tk_core_lp <- local_projection_trend_plain(
  pce_for_takeaway, "pce_core_goods",
  model_start = "1996-04-01", model_end = "2024-12-01",
  base_date = "2024-12-01", forecast_start = "2025-01-01",
  horizon = 24, n_lags = 12, conf_level = 0.90, indicators = TRUE
)
tk_dur_lp <- local_projection_trend_plain(
  pce_for_takeaway, "pce_durables",
  model_start = "1996-04-01", model_end = "2024-12-01",
  base_date = "2024-12-01", forecast_start = "2025-01-01",
  horizon = 24, n_lags = 12, conf_level = 0.90, indicators = TRUE
)

tk_core_lp_dev <- tk_core_lp %>% filter(date == max(date)) %>% pull(vs_trend)
tk_dur_lp_dev  <- tk_dur_lp %>% filter(date == max(date)) %>% pull(vs_trend)

tk_pt_core_lp <- safe_divide(tk_core_lp_dev, tk_expected_core) * 100
tk_pt_dur_lp  <- safe_divide(tk_dur_lp_dev, tk_expected_dur) * 100

tk_core_low  <- min(tk_pt_core_simple, tk_pt_core_lp, na.rm = TRUE)
tk_core_high <- max(tk_pt_core_simple, tk_pt_core_lp, na.rm = TRUE)
tk_dur_low   <- min(tk_pt_dur_simple, tk_pt_dur_lp, na.rm = TRUE)
tk_dur_high  <- max(tk_pt_dur_simple, tk_pt_dur_lp, na.rm = TRUE)

# IPI-based takeaway values
tk_tau_c_data <- tryCatch(
  read_csv(file.path(here("output"), "emp_idx_tau_c.csv"), show_col_types = FALSE),
  error = function(e) NULL
)

if (!is.null(tk_tau_c_data)) {
  tk_dur_bea <- c("321", "327", "331", "332", "333", "334", "335", "3361MV",
                   "3364OT", "337", "339")
  tk_food_energy_bea <- c("111CA", "311FT", "211", "324")

  tk_core_tau <- tk_tau_c_data %>% filter(!(bea_code %in% tk_food_energy_bea))
  tk_dur_tau  <- tk_tau_c_data %>% filter(bea_code %in% tk_dur_bea)

  tk_tariff_increase_core <- sum(tk_core_tau$tau_c_recent * tk_core_tau$customs_value_2025) /
    sum(tk_core_tau$customs_value_2025) - BASELINE_TARIFF_RATE
  tk_tariff_increase_dur  <- sum(tk_dur_tau$tau_c_recent * tk_dur_tau$customs_value_2025) /
    sum(tk_dur_tau$customs_value_2025) - BASELINE_TARIFF_RATE

  tk_ipi_expected_core <- tk_tariff_increase_core * IMPORT_SHARE_CORE_GOODS * 100
  tk_ipi_expected_dur  <- tk_tariff_increase_dur * IMPORT_SHARE_DURABLES * 100
} else {
  tk_ipi_expected_core <- tk_expected_core
  tk_ipi_expected_dur  <- tk_expected_dur
}

if (ipi_available) {
  tk_ipi_base_date <- get_base_date(ipi_data, "date")
  tk_ipi_dec_core <- get_base_value(ipi_data, "imported_core_goods", tk_ipi_base_date)
  tk_ipi_dec_dur <- get_base_value(ipi_data, "imported_durables", tk_ipi_base_date)
  tk_ipi_latest <- ipi_data %>% filter(date == max(date))

  tk_ipi_ytd_core <- safe_divide(
    (tk_ipi_latest$imported_core_goods - tk_ipi_dec_core), tk_ipi_dec_core) * 100
  tk_ipi_ytd_dur <- safe_divide(
    (tk_ipi_latest$imported_durables - tk_ipi_dec_dur), tk_ipi_dec_dur) * 100

  tk_ipi_pt_core_simple <- safe_divide(tk_ipi_ytd_core, tk_ipi_expected_core) * 100
  tk_ipi_pt_dur_simple  <- safe_divide(tk_ipi_ytd_dur, tk_ipi_expected_dur) * 100

  ipi_for_takeaway <- ipi_data %>% filter(date >= "1996-01-01")
  tk_ipi_core_lp_result <- local_projection_trend_plain(
    ipi_for_takeaway, "imported_core_goods",
    model_start = "1996-04-01", model_end = "2024-12-01",
    base_date = "2024-12-01", forecast_start = "2025-01-01",
    horizon = 24, n_lags = 12, conf_level = 0.90, indicators = TRUE
  )
  tk_ipi_dur_lp_result <- local_projection_trend_plain(
    ipi_for_takeaway, "imported_durables",
    model_start = "1996-04-01", model_end = "2024-12-01",
    base_date = "2024-12-01", forecast_start = "2025-01-01",
    horizon = 24, n_lags = 12, conf_level = 0.90, indicators = TRUE
  )

  tk_ipi_core_lp_dev <- tk_ipi_core_lp_result %>% filter(date == max(date)) %>% pull(vs_trend)
  tk_ipi_dur_lp_dev  <- tk_ipi_dur_lp_result %>% filter(date == max(date)) %>% pull(vs_trend)

  tk_ipi_pt_core_lp <- safe_divide(tk_ipi_core_lp_dev, tk_ipi_expected_core) * 100
  tk_ipi_pt_dur_lp  <- safe_divide(tk_ipi_dur_lp_dev, tk_ipi_expected_dur) * 100

  tk_ipi_core_low  <- min(tk_ipi_pt_core_simple, tk_ipi_pt_core_lp, na.rm = TRUE)
  tk_ipi_core_high <- max(tk_ipi_pt_core_simple, tk_ipi_pt_core_lp, na.rm = TRUE)
  tk_ipi_dur_low   <- min(tk_ipi_pt_dur_simple, tk_ipi_pt_dur_lp, na.rm = TRUE)
  tk_ipi_dur_high  <- max(tk_ipi_pt_dur_simple, tk_ipi_pt_dur_lp, na.rm = TRUE)
} else {
  tk_ipi_ytd_core <- NA; tk_ipi_ytd_dur <- NA
  tk_ipi_core_low <- NA; tk_ipi_core_high <- NA
  tk_ipi_dur_low <- NA; tk_ipi_dur_high <- NA
}

# ==============================================================================
# COMPUTED DATA FOR FIGURES (moved from mid-document chunks)
# ==============================================================================

# --- Trend deviations for PCE (Figures 4-5) ---
core_early_trend <- tk_core_lp
dur_early_trend <- tk_dur_lp

latest_core_dev <- core_early_trend %>%
  filter(date == max(date)) %>% pull(vs_trend)
latest_dur_dev <- dur_early_trend %>%
  filter(date == max(date)) %>% pull(vs_trend)
latest_pce_date <- format(max(core_early_trend$date), "%B")

core_trend_result <- core_early_trend %>% mutate(core_goods_idx = actual_idx)
dur_trend_result <- dur_early_trend %>% mutate(durables_idx = actual_idx)

pce_combined <- core_trend_result %>%
  filter(date >= "2023-01-01") %>%
  dplyr::select(date, core_goods_idx, core_trend = trend, core_lower = trend_lower,
         core_upper = trend_upper, core_vs_trend = vs_trend, is_forecast) %>%
  left_join(
    dur_trend_result %>%
      filter(date >= "2023-01-01") %>%
      dplyr::select(date, durables_idx, dur_trend = trend, dur_lower = trend_lower,
             dur_upper = trend_upper, dur_vs_trend = vs_trend),
    by = "date"
  )

pce_for_fig4 <- pce_combined %>%
  mutate(
    core_goods_vs_trend = core_vs_trend,
    durables_vs_trend = dur_vs_trend
  )

# --- IPI Trends (Figures 6-7) ---
if (ipi_available) {
  ipi_for_trend <- ipi_data %>% filter(date >= "1996-01-01")

  ipi_core_trend_result <- local_projection_trend_plain(
    ipi_for_trend, "imported_core_goods",
    model_start = "1996-04-01", model_end = "2024-12-01",
    base_date = "2024-12-01", forecast_start = "2025-01-01",
    horizon = 24, n_lags = 12, conf_level = 0.90, indicators = TRUE
  )
  ipi_dur_trend_result <- local_projection_trend_plain(
    ipi_for_trend, "imported_durables",
    model_start = "1996-04-01", model_end = "2024-12-01",
    base_date = "2024-12-01", forecast_start = "2025-01-01",
    horizon = 24, n_lags = 12, conf_level = 0.90, indicators = TRUE
  )

  ipi_core_trend_result <- ipi_core_trend_result %>% mutate(core_goods_idx = actual_idx)
  ipi_dur_trend_result <- ipi_dur_trend_result %>% mutate(durables_idx = actual_idx)

  ipi_combined <- ipi_core_trend_result %>%
    filter(date >= "2023-01-01") %>%
    dplyr::select(date, core_goods_idx, core_trend = trend, core_lower = trend_lower,
           core_upper = trend_upper, core_vs_trend = vs_trend, is_forecast) %>%
    left_join(
      ipi_dur_trend_result %>%
        filter(date >= "2023-01-01") %>%
        dplyr::select(date, durables_idx, dur_trend = trend, dur_lower = trend_lower,
               dur_upper = trend_upper, dur_vs_trend = vs_trend),
      by = "date"
    )

  ipi_for_fig6 <- ipi_combined %>%
    mutate(
      core_goods_vs_trend = core_vs_trend,
      durables_vs_trend = dur_vs_trend
    )

  latest_ipi_core_dev <- ipi_core_trend_result %>%
    filter(date == max(date)) %>% pull(vs_trend)
  latest_ipi_dur_dev <- ipi_dur_trend_result %>%
    filter(date == max(date)) %>% pull(vs_trend)
}

# ==============================================================================
# PASSTHROUGH CALCULATION
# ==============================================================================

latest_tariff <- tariff_revenue %>%
  filter(!is.na(effective_rate)) %>%
  filter(date == max(date)) %>%
  pull(effective_rate)

tariff_increase <- latest_tariff / 100 - BASELINE_TARIFF_RATE

expected_core <- tariff_increase * IMPORT_SHARE_CORE_GOODS * 100
expected_dur <- tariff_increase * IMPORT_SHARE_DURABLES * 100

pce_base_date <- get_base_date(pce_prices, "date")
dec_base_core <- get_base_value(pce_prices, "pce_core_goods", pce_base_date)
dec_base_dur <- get_base_value(pce_prices, "pce_durables", pce_base_date)

latest_prices <- pce_prices %>% filter(date == max(date))
ytd_core <- safe_divide((latest_prices$pce_core_goods - dec_base_core), dec_base_core) * 100
ytd_dur <- safe_divide((latest_prices$pce_durables - dec_base_dur), dec_base_dur) * 100

# Helper for safe extraction
safe_val <- function(df, col) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  val <- df[[col]]
  if (length(val) == 0 || is.null(val)) return(NA_real_)
  val[1]
}

latest_actual_date <- pce_prices %>%
  filter(!is.na(pce_core_goods)) %>%
  pull(date) %>% max()

latest_lp <- pce_for_fig4 %>% filter(date == latest_actual_date)
trend_core_lp <- safe_val(latest_lp, "core_goods_vs_trend")
trend_dur_lp <- safe_val(latest_lp, "durables_vs_trend")

# Log-linear trend deviations
core_loglinear_pt <- calc_log_linear_trend(
  pce_prices %>% filter(date >= "2023-01-01"),
  "pce_core_goods",
  estimation_start = "2023-01-01", estimation_end = "2024-12-31",
  base_date = "2024-12-01", forecast_start = "2025-01-01",
  conf_level = 0.90
)
dur_loglinear_pt <- calc_log_linear_trend(
  pce_prices %>% filter(date >= "2023-01-01"),
  "pce_durables",
  estimation_start = "2023-01-01", estimation_end = "2024-12-31",
  base_date = "2024-12-01", forecast_start = "2025-01-01",
  conf_level = 0.90
)

latest_date <- latest_actual_date

get_ll_latest <- function(ll_result, target_date) {
  if (is.null(ll_result) || nrow(ll_result) == 0) return(NULL)
  row <- ll_result %>% filter(date == target_date)
  if (nrow(row) == 0) row <- ll_result %>% filter(date == max(date))
  if (nrow(row) == 0) return(NULL)
  row[1, ]
}

latest_ll_core <- get_ll_latest(core_loglinear_pt, latest_date)
latest_ll_dur <- get_ll_latest(dur_loglinear_pt, latest_date)

trend_core_ll <- safe_val(latest_ll_core, "vs_trend")
trend_dur_ll <- safe_val(latest_ll_dur, "vs_trend")

pt_core_lp <- safe_divide(trend_core_lp, expected_core) * 100
pt_core_ll <- safe_divide(trend_core_ll, expected_core) * 100
pt_core_simple <- safe_divide(ytd_core, expected_core) * 100

pt_dur_lp <- safe_divide(trend_dur_lp, expected_dur) * 100
pt_dur_ll <- safe_divide(trend_dur_ll, expected_dur) * 100
pt_dur_simple <- safe_divide(ytd_dur, expected_dur) * 100

data_month <- format(max(pce_prices$date), "%B %Y")

nov_2025_rate <- tariff_revenue %>%
  filter(year == 2025, month == 11) %>%
  pull(effective_rate)
if (length(nov_2025_rate) == 0) nov_2025_rate <- NA

# June 2025 passthrough estimates
jun_2025_date <- as.Date("2025-06-01")
jun_month <- "June 2025"

jun_tariff <- tariff_revenue %>%
  filter(year == 2025, month == 6) %>%
  pull(effective_rate)
if (length(jun_tariff) == 0) jun_tariff <- NA
jun_tariff_increase <- jun_tariff / 100 - BASELINE_TARIFF_RATE
jun_expected_core <- jun_tariff_increase * IMPORT_SHARE_CORE_GOODS * 100
jun_expected_dur <- jun_tariff_increase * IMPORT_SHARE_DURABLES * 100

jun_lp <- pce_for_fig4 %>% filter(date == jun_2025_date)
trend_core_lp_jun <- safe_val(jun_lp, "core_goods_vs_trend")
trend_dur_lp_jun <- safe_val(jun_lp, "durables_vs_trend")

jun_ll_core <- get_ll_latest(core_loglinear_pt, jun_2025_date)
jun_ll_dur <- get_ll_latest(dur_loglinear_pt, jun_2025_date)
trend_core_ll_jun <- safe_val(jun_ll_core, "vs_trend")
trend_dur_ll_jun <- safe_val(jun_ll_dur, "vs_trend")

jun_prices <- pce_prices %>% filter(date == jun_2025_date)
ytd_core_jun <- if (nrow(jun_prices) > 0) safe_divide((jun_prices$pce_core_goods - dec_base_core), dec_base_core) * 100 else NA
ytd_dur_jun <- if (nrow(jun_prices) > 0) safe_divide((jun_prices$pce_durables - dec_base_dur), dec_base_dur) * 100 else NA

pt_core_lp_jun <- safe_divide(trend_core_lp_jun, jun_expected_core) * 100
pt_core_ll_jun <- safe_divide(trend_core_ll_jun, jun_expected_core) * 100
pt_core_simple_jun <- safe_divide(ytd_core_jun, jun_expected_core) * 100
pt_dur_lp_jun <- safe_divide(trend_dur_lp_jun, jun_expected_dur) * 100
pt_dur_ll_jun <- safe_divide(trend_dur_ll_jun, jun_expected_dur) * 100
pt_dur_simple_jun <- safe_divide(ytd_dur_jun, jun_expected_dur) * 100

# Consumer-goods-specific tariff rates
tau_c_data <- tryCatch(
  read_csv(file.path(here("output"), "emp_idx_tau_c.csv"), show_col_types = FALSE),
  error = function(e) NULL
)

if (!is.null(tau_c_data)) {
  dur_bea <- c("321", "327", "331", "332", "333", "334", "335", "3361MV",
               "3364OT", "337", "339")
  food_energy_bea <- c("111CA", "311FT", "211", "324")

  core_tau <- tau_c_data %>% filter(!(bea_code %in% food_energy_bea))
  dur_tau <- tau_c_data %>% filter(bea_code %in% dur_bea)

  tau_core_2025 <- sum(core_tau$duties_2025) / sum(core_tau$customs_value_2025) * 100
  tau_dur_2025 <- sum(dur_tau$duties_2025) / sum(dur_tau$customs_value_2025) * 100

  tau_core_recent <- sum(core_tau$tau_c_recent * core_tau$customs_value_2025) /
    sum(core_tau$customs_value_2025) * 100
  tau_dur_recent <- sum(dur_tau$tau_c_recent * dur_tau$customs_value_2025) /
    sum(dur_tau$customs_value_2025) * 100

  if (hasName(tau_c_data, "tau_c_june")) {
    tau_core_june <- sum(core_tau$tau_c_june * core_tau$customs_value_2025, na.rm = TRUE) /
      sum(core_tau$customs_value_2025) * 100
    tau_dur_june <- sum(dur_tau$tau_c_june * dur_tau$customs_value_2025, na.rm = TRUE) /
      sum(dur_tau$customs_value_2025) * 100
  } else {
    tau_core_june <- tau_core_2025
    tau_dur_june <- tau_dur_2025
  }

  tariff_increase_core <- tau_core_recent / 100 - BASELINE_TARIFF_RATE
  tariff_increase_dur  <- tau_dur_recent / 100 - BASELINE_TARIFF_RATE
  jun_tariff_increase_core <- tau_core_june / 100 - BASELINE_TARIFF_RATE
  jun_tariff_increase_dur  <- tau_dur_june / 100 - BASELINE_TARIFF_RATE
} else {
  tau_core_2025 <- NA; tau_dur_2025 <- NA
  tau_core_recent <- NA; tau_dur_recent <- NA
  tau_core_june <- NA; tau_dur_june <- NA
  tariff_increase_core <- tariff_increase
  tariff_increase_dur  <- tariff_increase
  jun_tariff_increase_core <- jun_tariff_increase
  jun_tariff_increase_dur  <- jun_tariff_increase
}

tau_baseline_pct <- BASELINE_TARIFF_RATE * 100

# Store trend methodology info
core_model <- attr(core_trend_result, "model_summary")
trend_method <- if (!is.null(core_model$method)) core_model$method else "local_projection_plain"

# ==============================================================================
# IPI PASSTHROUGH (conditional on import_price_index.R output)
# ==============================================================================

if (ipi_available) {
  ipi_expected_core <- tariff_increase_core * IMPORT_SHARE_CORE_GOODS * 100
  ipi_expected_dur <- tariff_increase_dur * IMPORT_SHARE_DURABLES * 100

  ipi_latest_lp <- ipi_for_fig6 %>% filter(date == latest_actual_date)
  ipi_trend_core_lp <- safe_val(ipi_latest_lp, "core_goods_vs_trend")
  ipi_trend_dur_lp <- safe_val(ipi_latest_lp, "durables_vs_trend")
  ipi_pt_core_lp <- safe_divide(ipi_trend_core_lp, ipi_expected_core) * 100
  ipi_pt_dur_lp <- safe_divide(ipi_trend_dur_lp, ipi_expected_dur) * 100

  ipi_core_ll <- calc_log_linear_trend(
    ipi_data %>% filter(date >= "2023-01-01"),
    "imported_core_goods",
    estimation_start = "2023-01-01", estimation_end = "2024-12-31",
    base_date = "2024-12-01", forecast_start = "2025-01-01",
    conf_level = 0.90
  )
  ipi_dur_ll <- calc_log_linear_trend(
    ipi_data %>% filter(date >= "2023-01-01"),
    "imported_durables",
    estimation_start = "2023-01-01", estimation_end = "2024-12-31",
    base_date = "2024-12-01", forecast_start = "2025-01-01",
    conf_level = 0.90
  )
  ipi_latest_ll_core <- get_ll_latest(ipi_core_ll, latest_date)
  ipi_latest_ll_dur <- get_ll_latest(ipi_dur_ll, latest_date)
  ipi_trend_core_ll <- safe_val(ipi_latest_ll_core, "vs_trend")
  ipi_trend_dur_ll <- safe_val(ipi_latest_ll_dur, "vs_trend")
  ipi_pt_core_ll <- safe_divide(ipi_trend_core_ll, ipi_expected_core) * 100
  ipi_pt_dur_ll <- safe_divide(ipi_trend_dur_ll, ipi_expected_dur) * 100

  ipi_base_date <- get_base_date(ipi_data, "date")
  ipi_dec_base_core <- get_base_value(ipi_data, "imported_core_goods", ipi_base_date)
  ipi_dec_base_dur <- get_base_value(ipi_data, "imported_durables", ipi_base_date)
  ipi_latest_prices <- ipi_data %>% filter(date == max(date))
  ipi_ytd_core <- safe_divide((ipi_latest_prices$imported_core_goods - ipi_dec_base_core), ipi_dec_base_core) * 100
  ipi_ytd_dur <- safe_divide((ipi_latest_prices$imported_durables - ipi_dec_base_dur), ipi_dec_base_dur) * 100
  ipi_pt_core_simple <- safe_divide(ipi_ytd_core, ipi_expected_core) * 100
  ipi_pt_dur_simple <- safe_divide(ipi_ytd_dur, ipi_expected_dur) * 100

  # June 2025
  ipi_jun_expected_core <- jun_tariff_increase_core * IMPORT_SHARE_CORE_GOODS * 100
  ipi_jun_expected_dur <- jun_tariff_increase_dur * IMPORT_SHARE_DURABLES * 100

  ipi_jun_lp <- ipi_for_fig6 %>% filter(date == jun_2025_date)
  ipi_trend_core_lp_jun <- safe_val(ipi_jun_lp, "core_goods_vs_trend")
  ipi_trend_dur_lp_jun <- safe_val(ipi_jun_lp, "durables_vs_trend")
  ipi_pt_core_lp_jun <- safe_divide(ipi_trend_core_lp_jun, ipi_jun_expected_core) * 100
  ipi_pt_dur_lp_jun <- safe_divide(ipi_trend_dur_lp_jun, ipi_jun_expected_dur) * 100

  ipi_jun_ll_core <- get_ll_latest(ipi_core_ll, jun_2025_date)
  ipi_jun_ll_dur <- get_ll_latest(ipi_dur_ll, jun_2025_date)
  ipi_trend_core_ll_jun <- safe_val(ipi_jun_ll_core, "vs_trend")
  ipi_trend_dur_ll_jun <- safe_val(ipi_jun_ll_dur, "vs_trend")
  ipi_pt_core_ll_jun <- safe_divide(ipi_trend_core_ll_jun, ipi_jun_expected_core) * 100
  ipi_pt_dur_ll_jun <- safe_divide(ipi_trend_dur_ll_jun, ipi_jun_expected_dur) * 100

  ipi_jun_prices <- ipi_data %>% filter(date == jun_2025_date)
  ipi_ytd_core_jun <- if (nrow(ipi_jun_prices) > 0) safe_divide((ipi_jun_prices$imported_core_goods - ipi_dec_base_core), ipi_dec_base_core) * 100 else NA
  ipi_ytd_dur_jun <- if (nrow(ipi_jun_prices) > 0) safe_divide((ipi_jun_prices$imported_durables - ipi_dec_base_dur), ipi_dec_base_dur) * 100 else NA
  ipi_pt_core_simple_jun <- safe_divide(ipi_ytd_core_jun, ipi_jun_expected_core) * 100
  ipi_pt_dur_simple_jun <- safe_divide(ipi_ytd_dur_jun, ipi_jun_expected_dur) * 100
} else {
  ipi_trend_core_lp <- NA; ipi_trend_dur_lp <- NA
  ipi_pt_core_lp <- NA; ipi_pt_dur_lp <- NA
  ipi_trend_core_ll <- NA; ipi_trend_dur_ll <- NA
  ipi_pt_core_ll <- NA; ipi_pt_dur_ll <- NA
  ipi_ytd_core <- NA; ipi_ytd_dur <- NA
  ipi_pt_core_simple <- NA; ipi_pt_dur_simple <- NA
  ipi_trend_core_lp_jun <- NA; ipi_trend_dur_lp_jun <- NA
  ipi_pt_core_lp_jun <- NA; ipi_pt_dur_lp_jun <- NA
  ipi_trend_core_ll_jun <- NA; ipi_trend_dur_ll_jun <- NA
  ipi_pt_core_ll_jun <- NA; ipi_pt_dur_ll_jun <- NA
  ipi_ytd_core_jun <- NA; ipi_ytd_dur_jun <- NA
  ipi_pt_core_simple_jun <- NA; ipi_pt_dur_simple_jun <- NA
  ipi_expected_core <- NA; ipi_expected_dur <- NA
  ipi_jun_expected_core <- NA; ipi_jun_expected_dur <- NA
  IPI_EFF_SHARE_CORE <- NA; IPI_EFF_SHARE_DUR <- NA
}

# ==============================================================================
# PASSTHROUGH TABLE TIBBLES
# ==============================================================================

if (ipi_available) {
  ipi_passthrough_jun <- tibble(
    Category = c("Imported Core Goods", "Imported Durables"),
    `Import Share` = c(sprintf("%.1f%%", IMPORT_SHARE_CORE_GOODS * 100),
                       sprintf("%.1f%%", IMPORT_SHARE_DURABLES * 100)),
    `Tariff Increase` = c(sprintf("%.1f pp", jun_tariff_increase_core * 100),
                          sprintf("%.1f pp", jun_tariff_increase_dur * 100)),
    `Expected Effect` = c(sprintf("%.1f%%", ipi_jun_expected_core),
                          sprintf("%.1f%%", ipi_jun_expected_dur)),
    `2025 Change` = c(fmt_pct(ipi_ytd_core_jun),
                     fmt_pct(ipi_ytd_dur_jun)),
    `vs LP Trend` = c(fmt_pct(ipi_trend_core_lp_jun),
                      fmt_pct(ipi_trend_dur_lp_jun)),
    `vs Log-Linear` = c(fmt_pct(ipi_trend_core_ll_jun),
                        fmt_pct(ipi_trend_dur_ll_jun)),
    `2025 Change ` = c(fmt_pt(ipi_pt_core_simple_jun),
                      fmt_pt(ipi_pt_dur_simple_jun)),
    `vs LP Trend ` = c(fmt_pt(ipi_pt_core_lp_jun),
                       fmt_pt(ipi_pt_dur_lp_jun)),
    `vs Log-Linear ` = c(fmt_pt(ipi_pt_core_ll_jun),
                         fmt_pt(ipi_pt_dur_ll_jun))
  )

  ipi_passthrough <- tibble(
    Category = c("Imported Core Goods", "Imported Durables"),
    `Import Share` = c(sprintf("%.1f%%", IMPORT_SHARE_CORE_GOODS * 100),
                       sprintf("%.1f%%", IMPORT_SHARE_DURABLES * 100)),
    `Tariff Increase` = c(sprintf("%.1f pp", tariff_increase_core * 100),
                          sprintf("%.1f pp", tariff_increase_dur * 100)),
    `Expected Effect` = c(sprintf("%.1f%%", ipi_expected_core),
                          sprintf("%.1f%%", ipi_expected_dur)),
    `2025 Change` = c(fmt_pct(ipi_ytd_core),
                     fmt_pct(ipi_ytd_dur)),
    `vs LP Trend` = c(fmt_pct(ipi_trend_core_lp),
                      fmt_pct(ipi_trend_dur_lp)),
    `vs Log-Linear` = c(fmt_pct(ipi_trend_core_ll),
                        fmt_pct(ipi_trend_dur_ll)),
    `2025 Change ` = c(fmt_pt(ipi_pt_core_simple),
                      fmt_pt(ipi_pt_dur_simple)),
    `vs LP Trend ` = c(fmt_pt(ipi_pt_core_lp),
                       fmt_pt(ipi_pt_dur_lp)),
    `vs Log-Linear ` = c(fmt_pt(ipi_pt_core_ll),
                         fmt_pt(ipi_pt_dur_ll))
  )
}

# Appendix passthrough tables (all-PCE)
passthrough_data_jun <- tibble(
  Category = c("Core Goods", "Durables"),
  `Import Share` = c(sprintf("%.1f%%", IMPORT_SHARE_CORE_GOODS * 100),
                     sprintf("%.1f%%", IMPORT_SHARE_DURABLES * 100)),
  `Tariff Increase` = c(sprintf("%.1f pp", jun_tariff_increase * 100),
                        sprintf("%.1f pp", jun_tariff_increase * 100)),
  `Expected Effect` = c(sprintf("%.1f%%", jun_expected_core),
                        sprintf("%.1f%%", jun_expected_dur)),
  `2025 Change` = c(fmt_pct(ytd_core_jun),
                   fmt_pct(ytd_dur_jun)),
  `vs LP Trend` = c(fmt_pct(trend_core_lp_jun),
                    fmt_pct(trend_dur_lp_jun)),
  `vs Log-Linear` = c(fmt_pct(trend_core_ll_jun),
                      fmt_pct(trend_dur_ll_jun)),
  `2025 Change ` = c(fmt_pt(pt_core_simple_jun),
                    fmt_pt(pt_dur_simple_jun)),
  `vs LP Trend ` = c(fmt_pt(pt_core_lp_jun),
                     fmt_pt(pt_dur_lp_jun)),
  `vs Log-Linear ` = c(fmt_pt(pt_core_ll_jun),
                       fmt_pt(pt_dur_ll_jun))
)

passthrough_data <- tibble(
  Category = c("Core Goods", "Durables"),
  `Import Share` = c(sprintf("%.1f%%", IMPORT_SHARE_CORE_GOODS * 100),
                     sprintf("%.1f%%", IMPORT_SHARE_DURABLES * 100)),
  `Tariff Increase` = c(sprintf("%.1f pp", tariff_increase * 100),
                        sprintf("%.1f pp", tariff_increase * 100)),
  `Expected Effect` = c(sprintf("%.1f%%", expected_core),
                        sprintf("%.1f%%", expected_dur)),
  `2025 Change` = c(sprintf("%.1f%%", ytd_core),
                   sprintf("%.1f%%", ytd_dur)),
  `vs LP Trend` = c(fmt_pct(trend_core_lp),
                    fmt_pct(trend_dur_lp)),
  `vs Log-Linear` = c(fmt_pct(trend_core_ll),
                      fmt_pct(trend_dur_ll)),
  `2025 Change ` = c(fmt_pt(pt_core_simple),
                    fmt_pt(pt_dur_simple)),
  `vs LP Trend ` = c(fmt_pt(pt_core_lp),
                     fmt_pt(pt_dur_lp)),
  `vs Log-Linear ` = c(fmt_pt(pt_core_ll),
                       fmt_pt(pt_dur_ll))
)

# ==============================================================================
# EMPLOYMENT STATS
# ==============================================================================

emp_latest <- employment %>% filter(!is.na(emp_nonfarm)) %>% filter(date == max(date))
emp_1yr_ago <- employment %>% filter(date == emp_latest$date %m-% months(12)) %>% slice(1)
unemp_latest_date <- format(emp_latest$date, "%B %Y")
nonfarm_yoy_change <- if (nrow(emp_1yr_ago) > 0) {
  (emp_latest$emp_nonfarm - emp_1yr_ago$emp_nonfarm)
} else { NA }
emp_2yr_ago <- employment %>% filter(date == emp_latest$date %m-% months(24)) %>% slice(1)
nonfarm_yoy_change_prior <- if (nrow(emp_1yr_ago) > 0 && nrow(emp_2yr_ago) > 0) {
  (emp_1yr_ago$emp_nonfarm - emp_2yr_ago$emp_nonfarm)
} else { NA }

if (tariff_emp_available) {
  emp_base <- get_base_value(tariff_emp, "I_t_recent", as.Date("2024-12-01"))
  emp_idx_latest <- tariff_emp %>% filter(date == max(date)) %>%
    mutate(idx = I_t_recent / emp_base * 100)
  emp_idx_ytd_pct <- emp_idx_latest$idx - 100

  emp_idx_series <- tariff_emp %>%
    filter(date >= "2023-01-01") %>%
    mutate(idx = I_t_recent / emp_base * 100)
  emp_idx_with_trend <- calc_trend(emp_idx_series, "idx", "2023-01-01", "2024-12-01")
  emp_trend_latest <- emp_idx_with_trend %>% filter(date == max(date))
  emp_trend_predicted_change <- emp_trend_latest$trend - 100
  emp_shortfall_vs_trend <- emp_idx_ytd_pct - emp_trend_predicted_change

  emp_latest_month <- month(max(tariff_emp$date))
  emp_dec_2023_base <- get_base_value(tariff_emp, "I_t_recent", as.Date("2023-12-01"))
  emp_same_month_2024 <- tariff_emp %>%
    filter(year(date) == 2024, month(date) == emp_latest_month) %>% slice(1)
  emp_2024_pct <- if (nrow(emp_same_month_2024) > 0 && !is.na(emp_dec_2023_base)) {
    (emp_same_month_2024$I_t_recent / emp_dec_2023_base - 1) * 100
  } else { NA }
  emp_shortfall_vs_2024 <- emp_idx_ytd_pct - emp_2024_pct
  emp_month_name <- format(max(tariff_emp$date), "%B")
} else {
  emp_idx_ytd_pct <- NA; emp_shortfall_vs_trend <- NA; emp_shortfall_vs_2024 <- NA
  emp_trend_predicted_change <- NA; emp_2024_pct <- NA
  emp_month_name <- "N/A"
}

if (tariff_emp_mfg_available) {
  mfg_base <- get_base_value(tariff_emp_mfg, "I_t_recent", as.Date("2024-12-01"))
  mfg_idx_latest <- tariff_emp_mfg %>% filter(date == max(date)) %>%
    mutate(idx = I_t_recent / mfg_base * 100)
  mfg_idx_ytd_pct <- mfg_idx_latest$idx - 100

  mfg_idx_series <- tariff_emp_mfg %>%
    filter(date >= "2023-01-01") %>%
    mutate(idx = I_t_recent / mfg_base * 100)
  mfg_idx_with_trend <- calc_trend(mfg_idx_series, "idx", "2023-01-01", "2024-12-01")
  mfg_trend_latest <- mfg_idx_with_trend %>% filter(date == max(date))
  mfg_trend_predicted_change <- mfg_trend_latest$trend - 100
} else {
  mfg_idx_ytd_pct <- NA; mfg_trend_predicted_change <- NA
}

# ==============================================================================
# EMPLOYMENT INDEX FIGURE DATA (Figures 9-10)
# ==============================================================================

if (tariff_emp_available) {
  emp_idx_base_date <- as.Date("2024-12-01")
  dec_base_recent <- get_base_value(tariff_emp, "I_t_recent", emp_idx_base_date)

  tariff_emp_indexed <- tariff_emp %>%
    filter(date >= "2023-01-01") %>%
    mutate(idx_recent = I_t_recent / dec_base_recent * 100)

  trend_recent <- tariff_emp_indexed %>%
    calc_trend("idx_recent", "2023-01-01", "2024-12-01") %>%
    dplyr::select(date, actual = idx_recent, trend, vs_trend)
}

if (tariff_emp_mfg_available) {
  mfg_emp_idx_base_date <- as.Date("2024-12-01")
  dec_base_mfg_recent <- get_base_value(tariff_emp_mfg, "I_t_recent", mfg_emp_idx_base_date)

  tariff_emp_mfg_indexed <- tariff_emp_mfg %>%
    filter(date >= "2023-01-01") %>%
    mutate(idx_recent = I_t_recent / dec_base_mfg_recent * 100)

  trend_mfg_recent <- tariff_emp_mfg_indexed %>%
    calc_trend("idx_recent", "2023-01-01", "2024-12-01") %>%
    dplyr::select(date, actual = idx_recent, trend, vs_trend)
}

# ==============================================================================
# INDUSTRIAL PRODUCTION STATS (Figure 11)
# ==============================================================================

ip_base_val <- get_base_value(industrial_production, "ip_manufacturing", as.Date("2024-12-01"))
ip_latest <- industrial_production %>% filter(!is.na(ip_manufacturing)) %>% filter(date == max(date))
ip_ytd_pct <- (ip_latest$ip_manufacturing / ip_base_val - 1) * 100
ip_month_name <- format(ip_latest$date, "%B")

dec_base_ip <- get_base_value(industrial_production, "ip_manufacturing", as.Date("2024-12-01"))

ip_indexed <- industrial_production %>%
  filter(date >= "2021-01-01") %>%
  mutate(ip_idx = ip_manufacturing / dec_base_ip * 100)

ip_trend <- ip_indexed %>%
  calc_trend("ip_idx", "2023-01-01", "2024-12-01")

# ==============================================================================
# EXCHANGE RATE STATS (Figure 12)
# ==============================================================================

if (fx_daily_available) {
  fx_dec_avg <- fx_daily %>%
    filter(year(date) == 2024, month(date) == 12) %>%
    summarize(
      usd = mean(usd_neer, na.rm = TRUE),
      cny = mean(cny_neer, na.rm = TRUE),
      mxn = mean(mxn_neer, na.rm = TRUE),
      cad = mean(cad_neer, na.rm = TRUE)
    )

  last_full_month <- fx_daily %>%
    mutate(ym = floor_date(date, "month")) %>%
    filter(ym < floor_date(max(date), "month")) %>%
    filter(ym == max(ym))

  fx_latest_avg <- last_full_month %>%
    summarize(
      usd = mean(usd_neer, na.rm = TRUE),
      cny = mean(cny_neer, na.rm = TRUE),
      mxn = mean(mxn_neer, na.rm = TRUE),
      cad = mean(cad_neer, na.rm = TRUE)
    )

  fx_latest_date <- format(max(last_full_month$date), "%B %Y")

  twi_vs_dec <- (fx_latest_avg$usd / fx_dec_avg$usd - 1) * 100
  cny_vs_dec <- (fx_latest_avg$cny / fx_dec_avg$cny - 1) * 100
  mxn_vs_dec <- (fx_latest_avg$mxn / fx_dec_avg$mxn - 1) * 100
  cad_vs_dec <- (fx_latest_avg$cad / fx_dec_avg$cad - 1) * 100

  twi_longrun_avg <- twi_long %>%
    filter(date >= "2000-01-01", date <= "2024-12-01") %>%
    summarize(avg = mean(twdol, na.rm = TRUE)) %>% pull(avg)
  twi_dec_2024 <- twi_long %>%
    filter(year(date) == 2024, month(date) == 12) %>% slice(1)
  twi_dec_vs_longrun <- (twi_dec_2024$twdol / twi_longrun_avg - 1) * 100

  dec_2024_avg <- fx_daily %>%
    filter(year(date) == 2024, month(date) == 12) %>%
    summarize(
      usd_base = mean(usd_neer, na.rm = TRUE),
      mxn_base = mean(mxn_neer, na.rm = TRUE),
      cad_base = mean(cad_neer, na.rm = TRUE),
      cny_base = mean(cny_neer, na.rm = TRUE)
    )

  fx_daily_indexed <- fx_daily %>%
    mutate(
      `US Dollar` = 100 * (usd_neer / dec_2024_avg$usd_base),
      `Mexico (MXN)` = 100 * (mxn_neer / dec_2024_avg$mxn_base),
      `Canada (CAD)` = 100 * (cad_neer / dec_2024_avg$cad_base),
      `China (CNY)` = 100 * (cny_neer / dec_2024_avg$cny_base)
    ) %>%
    dplyr::select(date, `US Dollar`, `Mexico (MXN)`, `Canada (CAD)`, `China (CNY)`) %>%
    pivot_longer(-date, names_to = "currency", values_to = "index")
}

# ==============================================================================
# TRADE STATS (Figures 13-15)
# ==============================================================================

trade_dec_2024 <- trade_flows %>%
  filter(year(date) == 2024, month(date) == 12) %>% slice(1)
imports_deflator <- trade_dec_2024$imports_nominal / trade_dec_2024$imports_real
exports_deflator <- trade_dec_2024$exports_nominal / trade_dec_2024$exports_real

trade_for_pre <- trade_flows %>% filter(date >= "2023-01-01")
imports_pre <- trade_for_pre %>% filter(date < "2025-01-01")
exports_pre <- trade_for_pre %>% filter(date < "2025-01-01")
imports_pre$time_idx <- as.numeric(imports_pre$date - min(imports_pre$date))
exports_pre$time_idx <- as.numeric(exports_pre$date - min(exports_pre$date))
imports_lm_pre <- lm(imports_real ~ time_idx, data = imports_pre)
exports_lm_pre <- lm(exports_real ~ time_idx, data = exports_pre)

trade_pre_trend <- trade_for_pre %>%
  mutate(
    time_idx = as.numeric(date - min(date)),
    imports_trend = predict(imports_lm_pre, newdata = data.frame(time_idx = time_idx)),
    exports_trend = predict(exports_lm_pre, newdata = data.frame(time_idx = time_idx)),
    imports_vs_trend = (imports_real / imports_trend - 1) * 100,
    exports_vs_trend = (exports_real / exports_trend - 1) * 100
  )

mar_2025 <- trade_pre_trend %>% filter(date == as.Date("2025-03-01"))
dec_2024_trade <- trade_pre_trend %>% filter(date == as.Date("2024-12-01"))
mar_imports_vs_trend <- if (nrow(mar_2025) > 0 && nrow(dec_2024_trade) > 0) {
  mar_2025$imports_vs_trend - dec_2024_trade$imports_vs_trend
} else NA
mar_imports_excess <- if (nrow(mar_2025) > 0 && nrow(dec_2024_trade) > 0) {
  ((mar_2025$imports_real - mar_2025$imports_trend) -
   (dec_2024_trade$imports_real - dec_2024_trade$imports_trend)) * imports_deflator / 1000
} else NA

post_apr <- trade_pre_trend %>% filter(date >= "2025-04-01")
post_apr_avg_imports_vs_trend <- if (nrow(post_apr) > 0) mean(post_apr$imports_vs_trend, na.rm = TRUE) else NA

trade_latest <- trade_pre_trend %>% filter(date == max(date))
trade_latest_date <- format(trade_latest$date, "%B %Y")
exports_latest_vs_trend <- trade_latest$exports_vs_trend
exports_dir <- if (!is.na(exports_latest_vs_trend) && exports_latest_vs_trend > 0) "above" else "below"

cum_gap <- trade_pre_trend %>%
  filter(date >= "2024-12-01") %>%
  mutate(
    monthly_gap = (imports_real - imports_trend) * imports_deflator / 1000,
    cumulative_gap = cumsum(monthly_gap)
  )
cum_imports_total <- cum_gap %>% filter(date == max(date)) %>% pull(cumulative_gap)

# Trade data for figures
trade_with_trend <- trade_pre_trend %>%
  mutate(
    imports_real_2025 = imports_real * imports_deflator,
    imports_trend_2025 = imports_trend * imports_deflator,
    exports_real_2025 = exports_real * exports_deflator,
    exports_trend_2025 = exports_trend * exports_deflator
  )

imports_trend_data <- trade_with_trend %>%
  mutate(imports_idx = imports_real, trend = imports_trend,
         vs_trend = (imports_real / imports_trend - 1) * 100)
exports_trend_data <- trade_with_trend %>%
  mutate(exports_idx = exports_real, trend = exports_trend,
         vs_trend = (exports_real / exports_trend - 1) * 100)

cumulative_trade <- trade_with_trend %>%
  filter(date >= "2024-12-01") %>%
  mutate(
    imports_gap = (imports_real_2025 - imports_trend_2025) / 1000,
    cumul_imports = cumsum(imports_gap)
  )

cumulative_imports <- cumulative_trade %>%
  dplyr::select(date, imports_real, imports_trend,
         monthly_gap = imports_gap, cumulative_gap = cumul_imports)

# ==============================================================================
# IMPORT PRICE FIGURE DATA (Figure 8)
# ==============================================================================

import_base_date <- get_base_date(import_prices, "date")
dec_base_import <- get_base_value(import_prices, "import_price_nonpetroleum", import_base_date)

import_indexed <- import_prices %>%
  filter(date >= "2023-01-01") %>%
  mutate(import_idx = import_price_nonpetroleum / dec_base_import * 100)

import_trend <- import_indexed %>%
  calc_trend("import_idx", "2023-01-01", "2024-12-01")

# ==============================================================================
# REVENUE REAL (consolidated -- used by Figures 3 and exports)
# ==============================================================================

revenue_real <- tariff_revenue %>%
  filter(date >= "2020-01-01") %>%
  left_join(cpi_data %>% dplyr::select(date, cpi_all), by = "date") %>%
  filter(!is.na(cpi_all)) %>%
  mutate(customs_duties_real = customs_duties * (latest_cpi_val / cpi_all))

pre_2025_avg <- revenue_real %>%
  filter(date >= as.Date("2022-01-01") & date < as.Date("2025-01-01")) %>%
  summarize(avg = mean(customs_duties_real, na.rm = TRUE)) %>%
  pull(avg)

# ==============================================================================
# LOG-LINEAR TREND DATA (Appendix Figures A2-A3)
# ==============================================================================

core_ll_base <- core_loglinear_pt %>%
  filter(date == as.Date("2024-12-01")) %>%
  pull(pce_core_goods)
dur_ll_base <- dur_loglinear_pt %>%
  filter(date == as.Date("2024-12-01")) %>%
  pull(pce_durables)

ll_combined <- core_loglinear_pt %>%
  filter(date >= "2023-01-01") %>%
  mutate(core_goods_idx = pce_core_goods / core_ll_base * 100) %>%
  dplyr::select(date, core_goods_idx, core_trend = trend, core_lower = trend_lower,
         core_upper = trend_upper, is_forecast) %>%
  left_join(
    dur_loglinear_pt %>%
      filter(date >= "2023-01-01") %>%
      mutate(durables_idx = pce_durables / dur_ll_base * 100) %>%
      dplyr::select(date, durables_idx, dur_trend = trend, dur_lower = trend_lower,
             dur_upper = trend_upper),
    by = "date"
  )

ll_deviations <- ll_combined %>%
  filter(date >= "2024-12-01") %>%
  mutate(
    core_goods_vs_trend = (core_goods_idx / core_trend - 1) * 100,
    durables_vs_trend = (durables_idx / dur_trend - 1) * 100
  )

# ==============================================================================
# DONE
# ==============================================================================

.REPORT_SETUP_LOADED <- TRUE
message("report_setup.R loaded successfully.")
