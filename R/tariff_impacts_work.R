# ==============================================================================
# Tariff Impacts - Data Processing & Employment Index Calculation
# ==============================================================================
#
# Purpose: Pull all data needed for "Short-Run Effects of 2025 Tariffs So Far"
#          and calculate the tariff-exposed employment index
#
# Data Source: Haver Analytics API (requires paid license + ODBC configuration).
#   If Haver is unavailable, pre-generated CSV files in output/ can be used directly.
#
# Sections:
#   1. Configuration & Helper Functions
#   2. Standard Data Pulls (2.1-2.11)
#
# Author: John Iselin, Yale Budget Lab
# Date: January 2026
# ==============================================================================

library(Haver)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(here)
library(stringr)

# ==============================================================================
# SECTION 1: CONFIGURATION & HELPER FUNCTIONS
# ==============================================================================

here::i_am("R/tariff_impacts_work.R")

# Source shared utilities
source(here("R", "utils.R"))

INPUT_DIR  <- here("input")
OUTPUT_DIR <- here("output")
LOG_DIR    <- here("logs")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)

# Create log file for this script and get logging function
WORK_LOG_FILE <- file.path(LOG_DIR, paste0("tariff_impacts_work_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_msg <- create_logger(WORK_LOG_FILE)

# Date ranges
START_LONG    <- "1982-01-01"
START_RECENT  <- "2020-01-01"
START_2023    <- "2023-01-01"

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

# Helper: pull from Haver, rename columns, optionally transform, then save as CSV
# col_names: named vector c(old_name = "new_name", ...)
# transform_fn: optional function(df) for adding computed columns before saving
pull_rename_save <- function(codes, col_names = NULL, output_name,
                              start_date = START_LONG, transform_fn = NULL) {
  log_msg("INFO", paste0("Pulling ", output_name, " data..."))
  data <- pull_haver_local(codes, "monthly", start_date)
  if (is.null(data)) return(invisible(NULL))
  if (!is.null(col_names)) {
    for (old_name in names(col_names)) {
      idx <- which(names(data) == old_name)
      if (length(idx) == 1) names(data)[idx] <- col_names[[old_name]]
    }
  }
  if (!is.null(transform_fn)) data <- transform_fn(data)
  write_csv(data, file.path(OUTPUT_DIR, paste0(output_name, ".csv")))
  log_msg("INFO", paste0(output_name, ": ", nrow(data), " rows"))
  invisible(data)
}

# ==============================================================================
# SECTION 2: STANDARD DATA PULLS
# ==============================================================================

log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))
log_msg("INFO", "SECTION 2: Standard Data Pulls")
log_msg("INFO", "=" %>% rep(60) %>% paste(collapse = ""))

# ------------------------------------------------------------------------------
# 2.1 Tariff Revenue & Effective Rates
#   FTRU@GOVFIN  = Federal customs duties ($M, SAAR)
#   TMMCN@USINT  = Goods imports, customs value ($M, SAAR)
# ------------------------------------------------------------------------------

tariff_revenue <- pull_rename_save(
  c("FTRU@GOVFIN", "TMMCN@USINT"),
  c(ftru = "customs_duties", tmmcn = "imports_value"),
  "tariff_revenue", START_LONG,
  transform_fn = function(d) {
    d %>% mutate(
      effective_rate = (customs_duties / imports_value) * 100,
      year = year(date), month = month(date)
    )
  }
)

# ------------------------------------------------------------------------------
# 2.2 Import Shares by Country
#   DV{code}@USTRADE  = Dutiable value of imports by country ($M)
#   M{code}Z0@USTRADE = Total merchandise imports by country ($B)
#   Country codes: 156=Canada, 273=Mexico, 924=China, 997=EU, 001=World
# ------------------------------------------------------------------------------

import_shares <- pull_rename_save(
  c("DV156@USTRADE", "DV273@USTRADE", "DV924@USTRADE", "DV997@USTRADE", "DV001@USTRADE",
    "M156Z0@USTRADE", "M273Z0@USTRADE", "M924Z0@USTRADE", "M997Z0@USTRADE", "M001Z0@USTRADE"),
  c(dv156 = "dv_canada", dv273 = "dv_mexico", dv924 = "dv_china", dv997 = "dv_eu", dv001 = "dv_world",
    m156z0 = "m_canada", m273z0 = "m_mexico", m924z0 = "m_china", m997z0 = "m_eu", m001z0 = "m_world"),
  "import_shares", "2002-01-01",
  transform_fn = function(d) {
    d %>% mutate(
      # dv is in $M, m is in $B; divide dv by 1000 to convert $M -> $B
      share_canada = (dv_canada / 1000) / m_canada * 100,
      share_mexico = (dv_mexico / 1000) / m_mexico * 100,
      share_china = (dv_china / 1000) / m_china * 100,
      share_eu = (dv_eu / 1000) / m_eu * 100,
      dv_row = dv_world - dv_canada - dv_mexico - dv_china - dv_eu,
      m_row = m_world - m_canada - m_mexico - m_china - m_eu,
      share_row = (dv_row / 1000) / m_row * 100
    )
  }
)

# ------------------------------------------------------------------------------
# 2.3 PCE Price Indices
#   JCDM@USNA    = PCE Durables price index (2017=100)
#   JCGXFEM@USNA = PCE Core Goods (ex food & energy) price index (2017=100)
# ------------------------------------------------------------------------------

pce_prices <- pull_rename_save(
  c("JCDM@USNA", "JCGXFEM@USNA"),
  c(jcdm = "pce_durables", jcgxfem = "pce_core_goods"),
  "pce_prices"
)

# ------------------------------------------------------------------------------
# 2.4 Import Prices
#   PMENP@USECON = Import price index, nonpetroleum (2000=100)
#   PMEA@USECON  = Import price index, all commodities (2000=100)
#   PMEPI@USECON = Import price index, petroleum (2000=100)
# ------------------------------------------------------------------------------

import_prices <- pull_rename_save(
  c("PMENP@USECON", "PMEA@USECON", "PMEPI@USECON"),
  c(pmenp = "import_price_nonpetroleum", pmea = "import_price_all", pmepi = "import_price_fuels"),
  "import_prices", START_RECENT
)

# ------------------------------------------------------------------------------
# 2.5 Employment (Basic)
# ------------------------------------------------------------------------------

employment <- pull_rename_save(
  c("LANAGRA@USECON", "LAPRIVA@USECON", "LAMANUA@USECON",
    "LAF1A@LABOR", "LAF7A@LABOR", "LAG1A@LABOR", "LAG2A@LABOR",
    "LAG3A@LABOR", "LAG4A@LABOR", "LAG5A@LABOR", "LAG6A@LABOR", "LAG7A@LABOR",
    "LAWTRDA@LABOR", "LARTRDA@LABOR", "LAK4A@LABOR", "LAL3A@LABOR",
    "LACONSA@LABOR", "LAB1A@LABOR"),
  c(lanagra = "emp_nonfarm", lapriva = "emp_private", lamanua = "emp_manufacturing",
    laf1a = "emp_wood", laf7a = "emp_nonmetallic", lag1a = "emp_primary_metals",
    lag2a = "emp_fabricated_metals", lag3a = "emp_machinery", lag4a = "emp_computer",
    lag5a = "emp_electrical", lag6a = "emp_transport_equip", lag7a = "emp_furniture",
    lawtrda = "emp_wholesale", lartrda = "emp_retail", lak4a = "emp_trucking",
    lal3a = "emp_warehousing", laconsa = "emp_construction", lab1a = "emp_oil_gas"),
  "employment", "1990-01-01"
)

# ------------------------------------------------------------------------------
# 2.6 Industrial Production
# ------------------------------------------------------------------------------

industrial_production <- pull_rename_save(
  c("IP@USECON", "IPMFG@USECON", "IPMDG@USECON", "IPMND@USECON"),
  c(ip = "ip_total", ipmfg = "ip_manufacturing", ipmdg = "ip_durables", ipmnd = "ip_nondurables"),
  "industrial_production", START_RECENT
)

# ------------------------------------------------------------------------------
# 2.7 Trade Flows
#   TMMCAH@USECON = Goods imports, customs value, real chained 2017$ ($B, SAAR)
#   TMXAH@USECON  = Goods exports, real chained 2017$ ($B, SAAR)
#   TMXA@USECON   = Goods exports, nominal ($B, SAAR)
#   TMMCA@USECON  = Goods imports, customs value, nominal ($B, SAAR)
# ------------------------------------------------------------------------------

trade_flows <- pull_rename_save(
  c("TMMCAH@USECON", "TMXAH@USECON", "TMXA@USECON", "TMMCA@USECON"),
  c(tmmcah = "imports_real", tmxah = "exports_real", tmxa = "exports_nominal", tmmca = "imports_nominal"),
  "trade_flows", START_RECENT,
  transform_fn = function(d) {
    d %>% mutate(
      trade_balance_real = exports_real - imports_real,
      trade_balance_nominal = exports_nominal - imports_nominal
    )
  }
)

# ------------------------------------------------------------------------------
# 2.8 Fed Policy
#   FFED@USECON  = Federal funds effective rate (%)
#   FCM10@USECON = 10-year Treasury constant maturity yield (%)
#   FCM2@USECON  = 2-year Treasury constant maturity yield (%)
#   FTBS3@USECON = 3-month Treasury bill secondary market rate (%)
# ------------------------------------------------------------------------------

fed_policy <- pull_rename_save(
  c("FFED@USECON", "FCM10@USECON", "FCM2@USECON", "FTBS3@USECON"),
  c(ffed = "fed_funds", fcm10 = "tsy_10y", fcm2 = "tsy_2y", ftbs3 = "tsy_3m"),
  "fed_policy", START_RECENT
)

# ------------------------------------------------------------------------------
# 2.9 Exchange Rates (monthly)
#   FXTWBDI@USECON = Trade-weighted dollar index (broad, 2006=100)
#   FXCAN@USECON   = USD/CAD exchange rate
#   FXMEX@USECON   = USD/MXN exchange rate
#   FXCHI@USECON   = USD/CNY exchange rate
# ------------------------------------------------------------------------------

exchange_rates <- pull_rename_save(
  c("FXTWBDI@USECON", "FXCAN@USECON", "FXMEX@USECON", "FXCHI@USECON"),
  c(fxtwbdi = "twdol", fxcan = "usd_cad", fxmex = "usd_mxn", fxchi = "usd_cny"),
  "exchange_rates", START_RECENT
)

# TWI long history (for long-run average comparison in report)
pull_rename_save(
  "FXTWBDI@USECON",
  c(fxtwbdi = "twdol"),
  "twi_long", START_LONG
)

# ------------------------------------------------------------------------------
# 2.9b Daily Exchange Rates -- BIS Nominal Effective Exchange Rates (INTDAILY)
#   X111DNB@INTDAILY = US broad NEER index (higher = USD appreciation)
#   X273DNB@INTDAILY = Mexico broad NEER index (higher = MXN appreciation)
#   X156DNB@INTDAILY = Canada broad NEER index (higher = CAD appreciation)
#   X924DNB@INTDAILY = China broad NEER index (higher = CNY appreciation)
#   These are trade-weighted indices, NOT bilateral rates.
#   Note: This pull bypasses pull_rename_save because it uses daily frequency.
#   Column rename is positional because Haver daily returns may not preserve names.
# ------------------------------------------------------------------------------

log_msg("INFO", "Pulling exchange_rates_daily data...")
fx_daily_raw <- pull_haver(
  codes = c("X111DNB@INTDAILY", "X273DNB@INTDAILY", "X156DNB@INTDAILY", "X924DNB@INTDAILY"),
  frequency = "daily",
  start_date = START_2023,
  haver_available = haver_available,
  log_fn = log_msg
)
if (!is.null(fx_daily_raw)) {
  names(fx_daily_raw) <- c("date", "usd_neer", "mxn_neer", "cad_neer", "cny_neer")
  write_csv(fx_daily_raw, file.path(OUTPUT_DIR, "exchange_rates_daily.csv"))
  log_msg("INFO", paste0("exchange_rates_daily: ", nrow(fx_daily_raw), " rows"))
}

# ------------------------------------------------------------------------------
# 2.10 CPI Data
#   PCUN@USECON    = CPI-U, all items (1982-84=100)
#   PCUCCD@USECON  = CPI-U, durables (1982-84=100)
#   PCUSCFE@USECON = CPI-U, core goods ex food & energy (1982-84=100)
#   PCUSLFE@USECON = CPI-U, services less energy (1982-84=100)
# ------------------------------------------------------------------------------

cpi_data <- pull_rename_save(
  c("PCUN@USECON", "PCUCCD@USECON", "PCUSCFE@USECON", "PCUSLFE@USECON"),
  c(pcun = "cpi_all", pcuccd = "cpi_durables", pcuscfe = "cpi_core_goods", pcuslfe = "cpi_services_less_energy"),
  "cpi_data", START_RECENT
)

cat("\n")
