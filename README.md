# Tariff Impacts

Automated R scripts to generate tariff impact figures and analysis using Haver Analytics data.

## Overview

This repository automates the data pipeline for the [Tracking the Economic Effects of Tariffs](https://budgetlab.yale.edu/research/tracking-economic-effects-tariffs) analysis. A standalone [Methodological Appendix](https://budgetlab.yale.edu/research/methodological-appendix-tracking-economic-effects-tariffs) is also available.

## Requirements

- R 4.0+
- Haver Analytics ODBC connection configured (paid license required; see note below)
- Required R packages:

```r
install.packages(c("dplyr", "tidyr", "lubridate", "readr", "readxl", "here",
                   "stringr", "ggplot2", "scales", "knitr", "kableExtra",
                   "flextable", "officer", "rmarkdown", "sandwich", "lmtest",
                   "patchwork", "MSwM", "depmixS4", "openxlsx"))

# Haver package (from Haver Analytics)
install.packages("Haver", repos = "http://www.haver.com/r/")
```

**Note on Haver Analytics:** The data pull script (`tariff_impacts_work.R`) requires a Haver Analytics DLX license and ODBC connection. If Haver is unavailable, pre-generated CSV files in `output/` can be used directly -- the report and standalone scripts will load from these cached files automatically.

## Quick Start

### Option 1: Run Everything

```r
source("run_all.R")
```

This will:
1. Check and install missing packages
2. Run all data processing (Haver pulls)
3. Calculate the tariff-exposed employment index (if input files available)
4. Calculate the imported PCE price index (if input files available)
5. Generate the main HTML report with Excel data export
6. Generate the standalone methodology document

### Option 2: Run Step by Step

```r
# Step 1: Run core data processing (requires Haver; skip if using cached CSVs)
source("R/tariff_impacts_work.R")

# Step 2: Run employment index calculation (requires input files)
source("R/employment_index.R")

# Step 3: Run import price index calculation (requires input files)
source("R/import_price_index.R")

# Step 4: Generate main report
rmarkdown::render("R/tariff_impacts_report.Rmd", output_dir = "output")

# Step 5: Generate methodology document
rmarkdown::render("R/methodology.Rmd", output_dir = "output")
```

## Directory Structure

```
tariff-impacts/
├── R/
│   ├── utils.R                           # Shared utility functions
│   ├── tariff_impacts_work.R             # Haver API data pulls
│   ├── employment_index.R                # Employment index calculation (I_t)
│   ├── import_price_index.R              # Import-weighted PCE price index
│   ├── tariff_impacts_report.Rmd         # Main R Markdown report
│   └── methodology.Rmd                   # Methodology document (standalone)
├── input/                                # Input data files
│   ├── USITC - Customs and Duties - January 2026.xlsx
│   ├── BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx
│   ├── BEA - The Use of Commodities by Industry - Summary - 2024.xlsx
│   ├── PCEBridge_Summary.xlsx
│   ├── naics_to_bea_crosswalk.csv
│   ├── x_codes.csv
├── output/                               # Generated CSV files and reports
├── logs/                                 # Execution logs
├── run_all.R                             # Pipeline orchestrator
└── README.md
```

## Output Files

### Core Data (from `tariff_impacts_work.R`)
| File | Description |
|------|-------------|
| `tariff_revenue.csv` | Customs duties & effective tariff rates (1982-present) |
| `import_shares.csv` | Import shares by country (Canada, Mexico, China, EU, ROW) |
| `pce_prices.csv` | PCE price indices (core goods, durables) |
| `import_prices.csv` | Import price indices (all, non-petroleum, fuels) |
| `employment.csv` | Employment by industry (18 series) |
| `industrial_production.csv` | Industrial production indices |
| `trade_flows.csv` | Real and nominal imports & exports |
| `fed_policy.csv` | Interest rates (Fed funds, 10Y, 2Y, 3M Treasury) |
| `exchange_rates.csv` | Monthly dollar and currency indices |
| `exchange_rates_daily.csv` | Daily exchange rates (USD index, MXN, CAD, CNY) |
| `cpi_data.csv` | CPI price indices |

### Tariff Employment Index (from `employment_index.R`)
| File | Description |
|------|-------------|
| `employment_index_I_t.csv` | Monthly tariff-exposed employment index (all industries) |
| `employment_index_I_t_manufacturing.csv` | Manufacturing-only version of the index |
| `emp_idx_tau_c.csv` | Commodity-level tariff rates (2025 avg, most recent month, June) |
| `e_jt_employment_index.csv` | Industry-level employment time series (cache) |

### Imported PCE Price Index (from `import_price_index.R`)
| File | Description |
|------|-------------|
| `import_price_index.csv` | Final import-weighted PCE price index |
| `ipi_effective_shares.csv` | Effective import shares by variant (core goods, durables) |
| `ipi_expenditure_data.csv` | PCE expenditure data by category (cache) |
| `ipi_price_data.csv` | PCE price data by category (cache) |

### Report Outputs
| File | Description |
|------|-------------|
| `tariff_impacts_report.html` | Main analysis report with all figures and tables |
| `methodology.html` | Standalone methodology document (trend estimation, index construction, passthrough) |
| `tariff_impacts_results_YYYYMMDD.xlsx` | Excel workbook with raw data for every figure and table |

## Report Contents

### Figures

| # | Title | Trend Method |
|---|-------|-------------|
| 1 | Customs Duty Revenue (Inflation-Adjusted) | -- |
| 2 | Effective Tariff Rate | -- |
| 3 | PCE Core Goods & Durables Prices | Local Projection (90% CI) |
| 4 | Deviation from LP Trend | Local Projection |
| 5 | Imported PCE Core Goods & Durables Prices* | Local Projection (90% CI) |
| 6 | Imported PCE Deviation from Trend* | Local Projection |
| 7 | Non-Petroleum Import Prices | Simple Linear |
| 8 | Tariff-Exposed Employment Index* | Simple Linear |
| 9 | Manufacturing Employment Index* | Simple Linear |
| 10 | Industrial Production (2021--Present) | Simple Linear |
| 11 | Exchange Rates vs. Major Trading Partners | -- |
| 12 | Real US Imports and Exports (2025 USD) | Simple Linear |
| 13 | Trade Deviation from Trend (2 panels) | Simple Linear |

*Conditional on optional data files from standalone scripts.

### Tables

| # | Title |
|---|-------|
| 1a | Implied Passthrough of Imported PCE Goods Prices (June 2025)* |
| 1b | Implied Passthrough of Imported PCE Goods Prices (Latest Month)* |

Each passthrough table shows three price-change columns (YTD, vs. LP trend, vs. log-linear trend) and three implied-passthrough columns. Tables 1a/1b use category-specific effective tariff rates from USITC trade data (different rates for core goods vs. durables).

*Conditional on import price index data.

### Appendix

| # | Title |
|---|-------|
| Table A1 | Implied Consumer Passthrough -- All PCE Goods (June 2025) |
| Table A2 | Implied Consumer Passthrough -- All PCE Goods (Latest Month) |
| Figure A1 | PCE Core Goods & Durables (Log-Linear Trend) |
| Figure A2 | Deviation from Log-Linear Trend |
| Figure A3 | Manufacturing Industrial Production (2015-Present) |

---

## Methodology

Full methodology details---including trend estimation (local projection and simple linear), the Imported PCE Goods Price Index, total import content shares, consumer passthrough calculation, and the tariff-exposed employment index---are documented in the standalone methodology document (`R/methodology.Rmd`). Render it via:

```r
rmarkdown::render("R/methodology.Rmd", output_dir = "output")
```

### Required Input Files

Place these files in `input/`:

| File | Used By |
|------|---------|
| `USITC - Customs and Duties - January 2026.xlsx` | Employment index |
| `BEA - Import Matrix, Before Redefinitions - Summary - 2024.xlsx` | Employment index, Import price index |
| `BEA - The Use of Commodities by Industry - Summary - 2024.xlsx` | Import price index |
| `BEA - Commodity-by-Commodity Total Requirements - Summary - 2024.xlsx` | Import price index (optional) |
| `PCEBridge_Summary.xlsx` | Import price index |
| `naics_to_bea_crosswalk.csv` | Employment index |
| `x_codes.csv` | Employment index |

---

## Data Sources

### Haver Analytics (API, paid license required)

Time-series data pulled via the R `Haver` package. If Haver is unavailable, the pipeline falls back to cached CSV files in `output/`.

| Database | Provider | Content |
|----------|----------|---------|
| USECON | BLS, Census, Fed | Employment aggregates, import/export prices, industrial production, trade flows, exchange rates, CPI, interest rates |
| USNA | BEA | PCE price indices and expenditure by category (32 component series for IPI construction) |
| GOVFIN | U.S. Treasury | Federal customs duty revenue |
| USTRADE | Census | Dutiable value and total imports by country (Canada, Mexico, China, EU) |
| USINT | Census | Goods imports, customs value (aggregate) |
| INTDAILY | Fed, multiple | Daily exchange rates (USD index, MXN, CAD, CNY) |
| LABOR | BLS | Employment by detailed industry (72 series for employment index) |

### BEA Input-Output Tables (static Excel files, `input/`)

Annual snapshot tables from the Bureau of Economic Analysis, used for import share and supply-chain calculations.

| File | Content |
|------|---------|
| Import Matrix, Before Redefinitions (2024) | Imported commodity inputs by industry (~73 commodities x ~95 industries) |
| Use of Commodities by Industry (2024) | Total commodity use by industry (denominator for import shares) |
| Commodity-by-Commodity Total Requirements (2024) | Leontief inverse matrix for total import content (direct + indirect through supply chains) |
| PCE Bridge Table (`PCEBridge_Summary.xlsx`) | Maps BEA commodities to 32 PCE categories with purchasers' values |

### USITC Tariff Data (static Excel file, `input/`)

| File | Content |
|------|---------|
| Customs and Duties (January 2026) | Monthly customs value and calculated duties by 6-digit NAICS commodity for 2025. Used to compute commodity-level effective tariff rates. |

### Crosswalk Files (CSV, `input/`)

| File | Content |
|------|---------|
| `naics_to_bea_crosswalk.csv` | Maps 6-digit NAICS tariff codes to BEA commodity/industry codes |
| `x_codes.csv` | BEA commodity code descriptions (lookup table from import matrix) |

---

## Constants

| Constant | Value | Description | Source |
|----------|-------|-------------|--------|
| `IMPORT_SHARE_CORE_GOODS` | From data | Total import content share of core goods (direct + indirect via Leontief inverse) | `ipi_effective_shares.csv` (from BEA I-O tables) |
| `IMPORT_SHARE_DURABLES` | From data | Total import content share of durables (direct + indirect via Leontief inverse) | `ipi_effective_shares.csv` (from BEA I-O tables) |
| `BASELINE_TARIFF_RATE` | From data | Post-COVID (2022--2024) average effective tariff rate (customs duties / imports) | `tariff_revenue.csv` (Haver: FTRU@GOVFIN, TMMCN@USINT) |

---

## Caveats

1. **Timing**: Tariffs take 3-6 months to flow through supply chains; early passthrough estimates may understate ultimate effects
2. **Import shares**: Approximate values from BEA Use Tables; true shares vary by product
3. **Other factors**: Dollar movements, demand shifts, and inventory effects influence prices
4. **Tariff avoidance**: Effective rates may differ from statutory rates
5. **Modeled estimates**: Certain dollar figures cited in the report text (e.g., projected tariff revenue) come from TBL tariff modeling and author's calculations, and are updated manually rather than computed by this pipeline

---

## To Do

- [ ] **Automate USITC Customs & Duties download.** The `employment_index.R` script currently reads from a manually downloaded Excel file (`input/USITC - Customs and Duties - January 2026.xlsx`). This file must be re-downloaded from the [USITC DataWeb](https://dataweb.usitc.gov/) each month to pick up the latest tariff data. Proposed approach:
  1. Use the [DataWeb API](https://www.usitc.gov/applications/dataweb/api/dataweb_query_api.html) (`POST` to `https://datawebws.usitc.gov/dataweb/api/v2/report2/runReport`) to programmatically query monthly customs value and calculated duties by NAICS commodity for 2025.
  2. Authenticate via a Bearer token tied to a USITC DataWeb account (requires [Login.gov](https://login.gov/) MFA). Store the token in an `.Renviron` file (git-ignored).
  3. Add a new script (e.g., `R/download_usitc.R`) that builds the JSON query payload, hits the API, parses the response, and writes the result to `input/` in the same two-sheet Excel format the pipeline expects -- or refactor `employment_index.R` to accept CSV/data-frame input directly.
  4. Integrate into `run_all.R` as an optional Step 0 (skip if the token is not configured, with a warning).

---

## Author

John Iselin, Yale Budget Lab
