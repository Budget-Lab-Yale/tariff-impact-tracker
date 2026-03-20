# ==============================================================================
# Tariff Impacts - Export Website Data (Excel + CSVs)
# ==============================================================================
#
# Purpose: Export figure/table data to Excel workbook and website CSVs.
#          Extracted from tariff_impacts_report.Rmd for independent execution.
#
# Usage:
#   source(here("R", "export_website_data.R"))
#   # Or from command line:
#   Rscript R/export_website_data.R
#
# Outputs:
#   - output/tariff_impacts_results_YYYYMMDD.xlsx (always)
#   - website/csv/*.csv (publication runs only)
#   - website/vintages/YYYYMMDD/ (publication runs only)
#
# Author: John Iselin, Yale Budget Lab
# Date: March 2026
# ==============================================================================

library(here)

# Source shared setup (data loading, computation, metadata)
source(here("R", "report_setup.R"))

# openxlsx is listed in REQUIRED_PACKAGES in run_all.R
library(openxlsx)

# ==============================================================================
# EXCEL EXPORT
# ==============================================================================

wb <- createWorkbook()

# --- TBL House Styles (Aptos Narrow 11pt throughout) ---
titleStyle    <- createStyle(fontName = "Aptos Narrow", fontSize = 11,
                             textDecoration = "bold")
metaStyle     <- createStyle(fontName = "Aptos Narrow", fontSize = 11)
headerStyle   <- createStyle(fontName = "Aptos Narrow", fontSize = 11,
                             textDecoration = "bold", halign = "center",
                             border = "bottom", borderStyle = "thin")
dataStyleNum  <- createStyle(fontName = "Aptos Narrow", fontSize = 11,
                             halign = "center", numFmt = "0.00")
dataStyleDate <- createStyle(fontName = "Aptos Narrow", fontSize = 11,
                             halign = "center", numFmt = "mmm-yy")
dataStyleChar <- createStyle(fontName = "Aptos Narrow", fontSize = 11,
                             halign = "center")
tocLinkStyle  <- createStyle(fontName = "Aptos Narrow", fontSize = 11)

# Track sheet info for Data TOC
sheet_info <- tibble::tibble(sheet_name = character(), title = character())

# Helper: add a data sheet in TBL house style (7-row header structure)
add_tbl_sheet <- function(wb, sheet_name, data, title, subtitle, source,
                          notes = "", date_format = NULL) {
  tryCatch({
    addWorksheet(wb, sheet_name, gridLines = FALSE)

    writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
    addStyle(wb, sheet_name, titleStyle, rows = 1, cols = 1)

    writeData(wb, sheet_name, paste0("Subtitle: ", subtitle),
              startRow = 2, startCol = 1)
    addStyle(wb, sheet_name, metaStyle, rows = 2, cols = 1)

    current_row <- 3

    if (nzchar(notes)) {
      writeData(wb, sheet_name, paste0("Notes: ", notes),
                startRow = current_row, startCol = 1)
      addStyle(wb, sheet_name, metaStyle, rows = current_row, cols = 1)
      current_row <- current_row + 1
    }

    writeData(wb, sheet_name, paste0("Source: ", source),
              startRow = current_row, startCol = 1)
    addStyle(wb, sheet_name, metaStyle, rows = current_row, cols = 1)

    data_start <- current_row + 2
    writeData(wb, sheet_name, data, startRow = data_start, startCol = 1)

    ncols <- ncol(data)
    nrows <- nrow(data)

    addStyle(wb, sheet_name, headerStyle, rows = data_start, cols = 1:ncols,
             gridExpand = TRUE)

    if (nrows > 0) {
      for (j in seq_len(ncols)) {
        col_data <- data[[j]]
        if (inherits(col_data, "Date") || inherits(col_data, "POSIXct")) {
          ds <- if (!is.null(date_format)) {
            createStyle(fontName = "Aptos Narrow", fontSize = 11,
                        halign = "center", numFmt = date_format)
          } else dataStyleDate
          addStyle(wb, sheet_name, ds,
                   rows = (data_start + 1):(data_start + nrows), cols = j, gridExpand = TRUE)
        } else if (is.numeric(col_data)) {
          addStyle(wb, sheet_name, dataStyleNum,
                   rows = (data_start + 1):(data_start + nrows), cols = j, gridExpand = TRUE)
        } else {
          addStyle(wb, sheet_name, dataStyleChar,
                   rows = (data_start + 1):(data_start + nrows), cols = j, gridExpand = TRUE)
        }
      }
    }

    if (nrows > 0) {
      bottomBorder <- createStyle(border = "bottom", borderStyle = "thin", borderColour = "#000000")
      addStyle(wb, sheet_name, bottomBorder,
               rows = data_start + nrows, cols = 1:ncols,
               gridExpand = TRUE, stack = TRUE)
    }

    setColWidths(wb, sheet_name, cols = 1:ncols, widths = "auto")

    sheet_info <<- bind_rows(sheet_info, tibble::tibble(
      sheet_name = sheet_name,
      title = title
    ))

    TRUE
  }, error = function(e) {
    message(paste("Could not add sheet", sheet_name, ":", e$message))
    FALSE
  })
}

# --- Figure/Table data sheets ---

# Figure 1: Daily Effective Tariff Rate
if (daily_etr_available) {
  fig1_data_etr <- daily_etr %>%
    filter(date <= Sys.Date()) %>%
    dplyr::select("Date" = date,
                  "Revision" = revision,
                  "Weighted ETR" = weighted_etr,
                  "Weighted ETR (additional)" = weighted_etr_additional,
                  "Matched imports ($B)" = matched_imports_b,
                  "Total imports ($B)" = total_imports_b)
  add_tbl_sheet(wb, "F1", fig1_data_etr,
    title    = fig_excel_title("F1"),
    subtitle = fig_meta("F1", "subtitle"),
    source   = fig_meta("F1", "source"),
    notes    = fig_meta("F1", "notes"),
    date_format = "yyyy-mm-dd")
}

# Figure 2: Effective Tariff Rate
fig2_data <- tariff_revenue %>%
  filter(date >= "2020-01-01") %>%
  dplyr::select("Date" = date,
                "Customs duties (nominal USD)" = customs_duties,
                "Import values (nominal USD)" = imports_value,
                "Effective tariff rate (%)" = effective_rate)
add_tbl_sheet(wb, "F2", fig2_data,
  title    = fig_excel_title("F2"),
  subtitle = fig_meta("F2", "subtitle"),
  source   = fig_meta("F2", "source"))

# Figure 3: Customs Duty Revenue
fig3_data_rev <- revenue_real %>%
  dplyr::select("Date"= date,
                "Customs duties (nominal USD)" = customs_duties,
                "Customs duties (real)" = customs_duties_real) %>%
  mutate("2022-2024 average (real)" = pre_2025_avg)
add_tbl_sheet(wb, "F3", fig3_data_rev,
  title    = fig_excel_title("F3"),
  subtitle = fig_meta("F3", "subtitle"),
  source   = fig_meta("F3", "source"),
  notes    = fig_meta("F3", "notes"))

# Figure A1: Daily ETR by Authority
if (daily_etr_available) {
  figA1_auth_data <- daily_auth %>%
    filter(date <= Sys.Date()) %>%
    dplyr::select("Date" = date,
                  "Revision" = revision,
                  "Section 232" = etr_232,
                  "Section 301" = etr_301,
                  "IEEPA Reciprocal" = etr_ieepa,
                  "IEEPA Fentanyl" = etr_fentanyl,
                  "Section 122" = etr_s122)
  add_tbl_sheet(wb, "FA1", figA1_auth_data,
    title    = fig_excel_title("FA1"),
    subtitle = fig_meta("FA1", "subtitle"),
    source   = fig_meta("FA1", "source"),
    notes    = fig_meta("FA1", "notes"),
    date_format = "yyyy-mm-dd")
}

# Figure 4: PCE Prices with Trend
fig5_data <- pce_combined %>%
  dplyr::select("Date" = date,
         "Core goods index" = core_goods_idx,
         "Core goods trend" = core_trend,
         "Core goods trend, lower bound" = core_lower,
         "Core goods trend, upper bound" = core_upper,
         "Durable goods index" = durables_idx,
         "Durable goods trend" = dur_trend,
         "Durable goods trend, lower bound" = dur_lower,
         "Durable goods trend, upper bound" = dur_upper,
         "Forecast indicator" = is_forecast)
add_tbl_sheet(wb, "F4", fig5_data,
  title    = fig_excel_title("F4"),
  subtitle = fig_meta("F4", "subtitle"),
  source   = fig_meta("F4", "source"))

# Figure 5: Deviation from Trend
fig6_data <- pce_for_fig4 %>%
  filter(date >= "2024-12-01") %>%
  dplyr::select("Date" = date,
                "Core Goods" = core_goods_vs_trend,
                "Durable Goods" = dur_vs_trend)
add_tbl_sheet(wb, "F5", fig6_data,
  title    = fig_excel_title("F5"),
  subtitle = fig_meta("F5", "subtitle"),
  source   = fig_meta("F5", "source"))

# Table 1a: IPI Passthrough (June 2025)
if (ipi_available && exists("ipi_passthrough_jun")) {
  tab1a_data <- ipi_passthrough_jun %>%
    dplyr::select(Category,
                  "Import share" = "Import Share",
                  "Tariff increase (pp)" = "Tariff Increase",
                  "Expected price change under 100% passthrough" = "Expected Effect",
                  "Price change, 2025 change" = "2025 Change",
                  "Price change, vs LP trend" = "vs LP Trend",
                  "Price change, vs Log-Linear" = "vs Log-Linear",
                  "Passthrough, via 2025 change" = "2025 Change ",
                  "Passthrough, vs LP trend" =  "vs LP Trend ",
                  "vs Log-Linear" = "vs Log-Linear "
                  )
  add_tbl_sheet(wb, "T1a", tab1a_data,
    title    = fig_excel_title("T1a"),
    subtitle = fig_meta("T1a", "subtitle"),
    source   = fig_meta("T1a", "source"),
    notes    = passthrough_notes("T1a"))
}

# Table 1b: IPI Passthrough (latest month)
if (ipi_available && exists("ipi_passthrough")) {
  tab1b_data <- ipi_passthrough %>%
    dplyr::select(Category,
                  "Import share" = "Import Share",
                  "Tariff increase (pp)" = "Tariff Increase",
                  "Expected price change under 100% passthrough" = "Expected Effect",
                  "Price change, 2025 change" = "2025 Change",
                  "Price change, vs LP trend" = "vs LP Trend",
                  "Price change, vs Log-Linear" = "vs Log-Linear",
                  "Passthrough, via 2025 change" = "2025 Change ",
                  "Passthrough, vs LP trend" =  "vs LP Trend ",
                  "Passthrough, vs Log-Linear" = "vs Log-Linear "
                  )
  add_tbl_sheet(wb, "T1b", tab1b_data,
    title    = paste0(fig_excel_title("T1b"), " (", data_month, ")"),
    subtitle = fig_meta("T1b", "subtitle"),
    source   = fig_meta("T1b", "source"),
    notes    = passthrough_notes("T1b"))
}

# Table A1: All-PCE Passthrough (June 2025)
tblA1_data <- passthrough_data_jun %>%
  dplyr::select(Category,
                "Import share" = "Import Share",
                "Tariff increase (pp)" = "Tariff Increase",
                "Expected price change under 100% passthrough" = "Expected Effect",
                "Price change, 2025 change" = "2025 Change",
                "Price change, vs LP trend" = "vs LP Trend",
                "Price change, vs Log-Linear" = "vs Log-Linear",
                "Passthrough, via 2025 change" = "2025 Change ",
                "Passthrough, vs LP trend" = "vs LP Trend ",
                "Passthrough, vs Log-Linear" = "vs Log-Linear "
                )
add_tbl_sheet(wb, "TA1", tblA1_data,
  title    = fig_excel_title("TA1"),
  subtitle = fig_meta("TA1", "subtitle"),
  source   = fig_meta("TA1", "source"))

# Table A2: All-PCE Passthrough (latest month)
tblA2_data <- passthrough_data %>%
  dplyr::select(Category,
                "Import share" = "Import Share",
                "Tariff increase (pp)" = "Tariff Increase",
                "Expected price change under 100% passthrough" = "Expected Effect",
                "Price change, 2025 change" = "2025 Change",
                "Price change, vs LP trend" = "vs LP Trend",
                "Price change, vs Log-Linear" = "vs Log-Linear",
                "Passthrough, via 2025 change" = "2025 Change ",
                "Passthrough, vs LP trend" = "vs LP Trend ",
                "Passthrough, vs Log-Linear" = "vs Log-Linear "
                )
add_tbl_sheet(wb, "TA2", tblA2_data,
  title    = paste0(fig_excel_title("TA2"), " (", data_month, ")"),
  subtitle = fig_meta("TA2", "subtitle"),
  source   = fig_meta("TA2", "source"))

# Figures 6-7: Imported PCE Price Index
if (ipi_available && exists("ipi_combined")) {
  fig6_ipi_data <- ipi_combined %>%
    dplyr::select("Date" = date,
           "Imported core goods index" = core_goods_idx,
           "Imported core goods trend" = core_trend,
           "Imported core goods trend, lower bound" = core_lower,
           "Imported core goods trend, upper bound" = core_upper,
           "Imported durables index" = durables_idx,
           "Imported durables trend" = dur_trend,
           "Imported durables trend, lower bound" = dur_lower,
           "Imported durables trend, upper bound" = dur_upper,
           "Forecast indicator" = is_forecast)
  add_tbl_sheet(wb, "F6", fig6_ipi_data,
    title    = fig_excel_title("F6"),
    subtitle = fig_meta("F6", "subtitle"),
    source   = fig_meta("F6", "source"))

  fig7_ipi_data <- ipi_for_fig6 %>%
    filter(date >= "2024-12-01") %>%
    dplyr::select("Date" = date,
           "Imported Core Goods" = core_goods_vs_trend,
           "Imported Durables" = durables_vs_trend)
  add_tbl_sheet(wb, "F7", fig7_ipi_data,
    title    = fig_excel_title("F7"),
    subtitle = fig_meta("F7", "subtitle"),
    source   = fig_meta("F7", "source"))
}

# Figure 8: Import Prices
fig8_ip_data <- import_trend %>%
  dplyr::select("Date" = date,
                "Import price index" = import_idx,
                "Trend" = trend,
                "Deviation from trend (%)" = vs_trend)
add_tbl_sheet(wb, "F8", fig8_ip_data,
  title    = fig_excel_title("F8"),
  subtitle = fig_meta("F8", "subtitle"),
  source   = fig_meta("F8", "source"),
  notes    = fig_meta("F8", "notes"))

# Figure 9: Tariff Employment Index
if (tariff_emp_available && exists("trend_recent")) {
  fig9_emp_data <- trend_recent %>%
    dplyr::select("Date" = date,
                  "Employment index" = actual,
                  "Trend" = trend,
                  "Deviation from trend (%)" = vs_trend)
  add_tbl_sheet(wb, "F9", fig9_emp_data,
    title    = fig_excel_title("F9"),
    subtitle = fig_meta("F9", "subtitle"),
    source   = fig_meta("F9", "source"))
}

# Figure 10: Manufacturing Employment Index
if (tariff_emp_mfg_available && exists("trend_mfg_recent")) {
  fig10_mfg_data <- trend_mfg_recent %>%
    dplyr::select("Date" = date,
                  "Manufacturing employment index" = actual,
                  "Trend" = trend,
                  "Deviation from trend (%)" = vs_trend)
  add_tbl_sheet(wb, "F10", fig10_mfg_data,
    title    = fig_excel_title("F10"),
    subtitle = fig_meta("F10", "subtitle"),
    source   = fig_meta("F10", "source"))
}

# Figure 11: Industrial Production
fig11_ip_data <- ip_trend %>%
  dplyr::select("Date" = date,
                "Industrial production index" = ip_idx,
                "Trend" = trend,
                "Deviation from trend (%)" = vs_trend)
add_tbl_sheet(wb, "F11", fig11_ip_data,
  title    = fig_excel_title("F11"),
  subtitle = fig_meta("F11", "subtitle"),
  source   = fig_meta("F11", "source"))

# Figure 12: Exchange Rates
if (exists("fx_daily_indexed")) {
  fig12_fx_data <- fx_daily_indexed %>%
    pivot_wider(names_from = currency, values_from = index) %>%
    dplyr::rename("Date" = date)
  add_tbl_sheet(wb, "F12", fig12_fx_data,
    title    = fig_excel_title("F12"),
    subtitle = fig_meta("F12", "subtitle"),
    source   = fig_meta("F12", "source"),
    date_format = "yyyy-mm-dd")
}

# Figure 13: Real Imports and Exports
fig13_trade_data <- trade_with_trend %>%
  dplyr::select(date,
         imports_real_2025, imports_trend_2025,
         exports_real_2025, exports_trend_2025) %>%
  mutate(
    imports_real_2025 = imports_real_2025 / 1000,
    imports_trend_2025 = imports_trend_2025 / 1000,
    exports_real_2025 = exports_real_2025 / 1000,
    exports_trend_2025 = exports_trend_2025 / 1000,
    imports_vs_trend = (imports_real_2025 / imports_trend_2025 - 1) * 100,
    exports_vs_trend = (exports_real_2025 / exports_trend_2025 - 1) * 100
  ) %>%
  dplyr::rename(
    "Date" = date,
    "Real imports" = imports_real_2025,
    "Imports trend" = imports_trend_2025,
    "Real exports" = exports_real_2025,
    "Exports trend" = exports_trend_2025,
    "Imports deviation from trend (%)" = imports_vs_trend,
    "Exports deviation from trend (%)" = exports_vs_trend
  )
add_tbl_sheet(wb, "F13", fig13_trade_data,
  title    = fig_excel_title("F13"),
  subtitle = fig_meta("F13", "subtitle"),
  source   = fig_meta("F13", "source"))

# Figure 14: Trade Deviation
fig14_data <- trade_with_trend %>%
  dplyr::select(date, imports_real, imports_trend, exports_real, exports_trend) %>%
  mutate(
    imports_vs_trend_pct = (imports_real / imports_trend - 1) * 100,
    exports_vs_trend_pct = (exports_real / exports_trend - 1) * 100
  ) %>%
  dplyr::rename(
    "Date" = date,
    "Real imports" = imports_real,
    "Imports trend" = imports_trend,
    "Real exports" = exports_real,
    "Exports trend" = exports_trend,
    "Imports deviation from trend (%)" = imports_vs_trend_pct,
    "Exports deviation from trend (%)" = exports_vs_trend_pct
  )
add_tbl_sheet(wb, "F14", fig14_data,
  title    = fig_excel_title("F14"),
  subtitle = fig_meta("F14", "subtitle"),
  source   = fig_meta("F14", "source"))

fig15_data <- cumulative_imports %>%
  dplyr::select("Date" = date,
                "Real imports" = imports_real,
                "Imports trend" = imports_trend,
                "Monthly gap (billions USD)" = monthly_gap,
                "Cumulative gap (billions USD)" = cumulative_gap)
add_tbl_sheet(wb, "F15", fig15_data,
  title    = fig_excel_title("F15"),
  subtitle = fig_meta("F15", "subtitle"),
  source   = fig_meta("F15", "source"))

# Figure A2: PCE Prices with Log-Linear Trend
if (exists("ll_combined")) {
  figA1_data <- ll_combined %>%
    dplyr::select("Date" = date,
           "Core goods index" = core_goods_idx,
           "Core goods trend (log-linear)" = core_trend,
           "Core goods trend, lower bound" = core_lower,
           "Core goods trend, upper bound" = core_upper,
           "Durable goods index" = durables_idx,
           "Durable goods trend (log-linear)" = dur_trend,
           "Durable goods trend, lower bound" = dur_lower,
           "Durable goods trend, upper bound" = dur_upper,
           "Forecast indicator" = is_forecast)
  add_tbl_sheet(wb, "FA2", figA1_data,
    title    = fig_excel_title("FA2"),
    subtitle = fig_meta("FA2", "subtitle"),
    source   = fig_meta("FA2", "source"))
}

# Figure A3: Deviation from Log-Linear Trend
if (exists("ll_deviations")) {
  figA2_data <- ll_deviations %>%
    dplyr::select("Date" = date,
                  "Core Goods" = core_goods_vs_trend,
                  "Durable Goods" = durables_vs_trend)
  add_tbl_sheet(wb, "FA3", figA2_data,
    title    = fig_excel_title("FA3"),
    subtitle = fig_meta("FA3", "subtitle"),
    source   = fig_meta("FA3", "source"))
}

# === Data TOC Sheet ===
addWorksheet(wb, "Data TOC", gridLines = FALSE)

writeData(wb, "Data TOC", "Tracking the Economic Effects of Tariffs",
          startRow = 1, startCol = 1)
addStyle(wb, "Data TOC", titleStyle, rows = 1, cols = 1)

writeData(wb, "Data TOC", format(Sys.Date(), "%B %Y"),
          startRow = 2, startCol = 1)
addStyle(wb, "Data TOC", metaStyle, rows = 2, cols = 1)

writeData(wb, "Data TOC", "The Budget Lab at Yale",
          startRow = 3, startCol = 1)
addStyle(wb, "Data TOC", metaStyle, rows = 3, cols = 1)

writeData(wb, "Data TOC", "Tables and Figures",
          startRow = 5, startCol = 1)
addStyle(wb, "Data TOC", titleStyle, rows = 5, cols = 1)

for (i in seq_len(nrow(sheet_info))) {
  link_formula <- paste0("HYPERLINK(\"#'", sheet_info$sheet_name[i],
                         "'!A1\", \"", sheet_info$title[i], "\")")
  writeFormula(wb, "Data TOC", x = link_formula,
               startRow = 5 + i, startCol = 1)
  addStyle(wb, "Data TOC", tocLinkStyle, rows = 5 + i, cols = 1)
}

setColWidths(wb, "Data TOC", cols = 1, widths = 80)

# Move Data TOC to first position
worksheetOrder(wb) <- c(length(names(wb)), 1:(length(names(wb)) - 1))

# Save workbook
excel_filename <- paste0("tariff_impacts_results_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
excel_path <- file.path(OUTPUT_DIR, excel_filename)
saveWorkbook(wb, excel_path, overwrite = TRUE)
n_sheets <- length(names(wb))
message(paste("Excel workbook saved:", excel_filename, "(", n_sheets, "sheets)"))

# ==============================================================================
# WEBSITE CSV EXPORT (publication runs only)
# ==============================================================================

PUBLICATION_RUN_FLAG <- as.logical(Sys.getenv("PUBLICATION_RUN", unset = "FALSE"))
if (is.na(PUBLICATION_RUN_FLAG)) PUBLICATION_RUN_FLAG <- FALSE

if (!PUBLICATION_RUN_FLAG) {
  message("Website CSV export skipped (not a publication run).")
} else {

WEBSITE_DIR  <- here("website")
CSV_DIR      <- file.path(WEBSITE_DIR, "csv")
HTML_DIR     <- file.path(WEBSITE_DIR, "html")
VINTAGE_DATE <- format(Sys.Date(), "%Y%m%d")
VINTAGE_DIR  <- file.path(WEBSITE_DIR, "vintages", VINTAGE_DATE)
VINTAGE_CSV  <- file.path(VINTAGE_DIR, "csv")
dir.create(CSV_DIR,      showWarnings = FALSE, recursive = TRUE)
dir.create(VINTAGE_CSV,  showWarnings = FALSE, recursive = TRUE)

# Helper: write a CSV with metadata notes at the bottom
write_website_csv <- function(data, filename, fig_id) {
  path <- file.path(CSV_DIR, filename)
  tryCatch({
    write.table(data, path, sep = ",", row.names = FALSE,
                quote = TRUE, na = "")
    strip_commas <- function(x) gsub(",", ";", x)
    meta_lines <- c(
      "",
      paste0("# ", strip_commas(paste0(fig_meta(fig_id, "label"), ". ", fig_meta(fig_id, "title")))),
      paste0("# ", strip_commas(fig_meta(fig_id, "subtitle"))),
      paste0("# Source: ", strip_commas(fig_meta(fig_id, "source")))
    )
    notes <- fig_meta(fig_id, "notes")
    if (nzchar(notes)) meta_lines <- c(meta_lines, paste0("# Note: ", strip_commas(notes)))
    cat(paste0(meta_lines, collapse = "\n"), "\n", file = path, append = TRUE)
    file.copy(path, file.path(VINTAGE_CSV, filename), overwrite = TRUE)
    TRUE
  }, error = function(e) {
    message(paste("Could not write CSV", filename, ":", e$message))
    FALSE
  })
}

# F1: Daily ETR
if (daily_etr_available && exists("fig1_data_etr")) {
  fig1_csv <- fig1_data_etr %>% dplyr::select(Date, `Weighted ETR`)
  write_website_csv(fig1_csv, "F1_daily_etr.csv", "F1")
}

# F2: Effective Tariff Rate
fig2_csv <- fig2_data %>% dplyr::select(Date, `Effective tariff rate (%)`)
write_website_csv(fig2_csv, "F2_effective_tariff_rate.csv", "F2")

# F3: Revenue
fig3_csv <- fig3_data_rev %>%
  dplyr::select(Date, `Customs duties (real)`, `2022-2024 average (real)`) %>%
  mutate(Date = format_csv_date_line(Date))
write_website_csv(fig3_csv, "F3_customs_duty_revenue.csv", "F3")

# F4: PCE Prices
fig4_csv <- pce_combined %>%
  dplyr::select("Date" = date,
         "Core goods index" = core_goods_idx,
         "Core goods trend" = core_trend,
         "Durable goods index" = durables_idx,
         "Durable goods trend" = dur_trend) %>%
  mutate(Date = format_csv_date_line(Date))
write_website_csv(fig4_csv, "F4_pce_prices_lp_trend.csv", "F4")

# F5: PCE Deviation
fig5_csv <- pce_for_fig4 %>%
  filter(date >= "2024-12-01") %>%
  dplyr::select("Date" = date,
                "Core Goods" = core_goods_vs_trend,
                "Durable Goods" = dur_vs_trend) %>%
  mutate(Date = format_csv_date(Date))
write_website_csv(fig5_csv, "F5_pce_deviation_from_trend.csv", "F5")

# T1: Combined IPI Passthrough
if (ipi_available && exists("ipi_passthrough_jun") && exists("ipi_passthrough")) {
  t1_path <- file.path(CSV_DIR, "T1_ipi_passthrough.csv")
  tryCatch({
    header_row <- ",Inputs,~~~,~~~,Price Changes,~~~,~~~,Implied Passthrough,~~~,~~~"
    col_names <- c("Category", "Import share", "Tariff increase", "Expected effect",
                   "2025 Change", "vs LP Trend", "vs Log-Linear",
                   "2025 Change", "vs LP Trend", "vs Log-Linear")
    lines <- character()
    lines <- c(lines, header_row)
    lines <- c(lines, paste(shQuote(col_names, type = "cmd"), collapse = ","))
    lines <- c(lines, paste0('"June 2025"', paste(rep(",", 9), collapse = "")))
    for (i in seq_len(nrow(tab1a_data))) {
      lines <- c(lines, paste(shQuote(as.character(tab1a_data[i, ]), type = "cmd"), collapse = ","))
    }
    lines <- c(lines, paste(rep(",", 9), collapse = ""))
    lines <- c(lines, paste0('"', data_month, '"', paste(rep(",", 9), collapse = "")))
    for (i in seq_len(nrow(tab1b_data))) {
      lines <- c(lines, paste(shQuote(as.character(tab1b_data[i, ]), type = "cmd"), collapse = ","))
    }
    writeLines(lines, t1_path)
    file.copy(t1_path, file.path(VINTAGE_CSV, "T1_ipi_passthrough.csv"), overwrite = TRUE)
  }, error = function(e) {
    message(paste("Could not write CSV T1_ipi_passthrough.csv:", e$message))
  })
}

# F6-F7: Imported PCE Prices
if (ipi_available && exists("ipi_combined")) {
  fig6_csv <- ipi_combined %>%
    dplyr::select("Date" = date,
           "Imported core goods index" = core_goods_idx,
           "Imported core goods trend" = core_trend,
           "Imported durables index" = durables_idx,
           "Imported durables trend" = dur_trend) %>%
    mutate(Date = format_csv_date_line(Date))
  write_website_csv(fig6_csv, "F6_imported_pce_prices.csv", "F6")

  fig7_csv <- ipi_for_fig6 %>%
    filter(date >= "2024-12-01") %>%
    dplyr::select("Date" = date,
           "Imported Core Goods" = core_goods_vs_trend,
           "Imported Durables" = durables_vs_trend) %>%
    mutate(Date = format_csv_date(Date))
  write_website_csv(fig7_csv, "F7_imported_pce_deviation.csv", "F7")
}

# F8: Import Prices
fig8_csv <- import_trend %>%
  dplyr::select("Date" = date, "Import price index" = import_idx, "Trend" = trend) %>%
  mutate(Date = format_csv_date_line(Date))
write_website_csv(fig8_csv, "F8_import_prices.csv", "F8")

# F9-F10: Employment indices
if (tariff_emp_available && exists("trend_recent")) {
  fig9_csv <- trend_recent %>%
    dplyr::select("Date" = date, "Employment index" = actual, "Trend" = trend) %>%
    mutate(Date = format_csv_date_line(Date))
  write_website_csv(fig9_csv, "F9_employment_index.csv", "F9")
}

if (tariff_emp_mfg_available && exists("trend_mfg_recent")) {
  fig10_csv <- trend_mfg_recent %>%
    dplyr::select("Date" = date, "Manufacturing employment index" = actual, "Trend" = trend) %>%
    mutate(Date = format_csv_date_line(Date))
  write_website_csv(fig10_csv, "F10_manufacturing_employment_index.csv", "F10")
}

# F11: Industrial Production
fig11_csv <- ip_trend %>%
  dplyr::select("Date" = date, "Industrial production index" = ip_idx, "Trend" = trend) %>%
  mutate(Date = format_csv_date_line(Date))
write_website_csv(fig11_csv, "F11_industrial_production.csv", "F11")

# F12: Exchange Rates
if (exists("fx_daily_indexed")) {
  fig12_csv <- fx_daily_indexed %>%
    pivot_wider(names_from = currency, values_from = index) %>%
    dplyr::rename("Date" = date)
  write_website_csv(fig12_csv, "F12_exchange_rates.csv", "F12")
}

# F13: Real Imports and Exports
fig13_csv <- fig13_trade_data %>%
  dplyr::select(Date, `Real imports`, `Imports trend`, `Real exports`, `Exports trend`) %>%
  mutate(Date = format_csv_date_line(Date))
write_website_csv(fig13_csv, "F13_real_imports_exports.csv", "F13")

# F14: Trade Deviation
fig14_csv <- fig14_data %>%
  dplyr::select(Date,
                "Imports" = `Imports deviation from trend (%)`,
                "Exports" = `Exports deviation from trend (%)`) %>%
  mutate(Date = format_csv_date(Date))
write_website_csv(fig14_csv, "F14_trade_deviation.csv", "F14")

# F15: Cumulative Import Gap
fig15_csv <- fig15_data %>%
  dplyr::select(Date, `Monthly gap (billions USD)`, `Cumulative gap (billions USD)`) %>%
  mutate(Date = format_csv_date_line(Date))
write_website_csv(fig15_csv, "F15_cumulative_import_gap.csv", "F15")

# TA: Combined All-PCE Passthrough
{
  ta_path <- file.path(CSV_DIR, "TA_pce_passthrough.csv")
  tryCatch({
    header_row <- ",Inputs,~~~,~~~,Price Changes,~~~,~~~,Implied Passthrough,~~~,~~~"
    col_names <- c("Category", "Import share", "Tariff increase", "Expected effect",
                   "2025 Change", "vs LP Trend", "vs Log-Linear",
                   "2025 Change", "vs LP Trend", "vs Log-Linear")
    lines <- character()
    lines <- c(lines, header_row)
    lines <- c(lines, paste(shQuote(col_names, type = "cmd"), collapse = ","))
    lines <- c(lines, paste0('"June 2025"', paste(rep(",", 9), collapse = "")))
    for (i in seq_len(nrow(tblA1_data))) {
      lines <- c(lines, paste(shQuote(as.character(tblA1_data[i, ]), type = "cmd"), collapse = ","))
    }
    lines <- c(lines, paste(rep(",", 9), collapse = ""))
    lines <- c(lines, paste0('"', data_month, '"', paste(rep(",", 9), collapse = "")))
    for (i in seq_len(nrow(tblA2_data))) {
      lines <- c(lines, paste(shQuote(as.character(tblA2_data[i, ]), type = "cmd"), collapse = ","))
    }
    writeLines(lines, ta_path)
    file.copy(ta_path, file.path(VINTAGE_CSV, "TA_pce_passthrough.csv"), overwrite = TRUE)
  }, error = function(e) {
    message(paste("Could not write CSV TA_pce_passthrough.csv:", e$message))
  })
}

# FA1: Daily ETR by Authority
if (daily_etr_available && exists("figA1_auth_data")) {
  figA1_csv <- figA1_auth_data %>% dplyr::select(-Revision)
  write_website_csv(figA1_csv, "FA1_daily_etr_by_authority.csv", "FA1")
}

# FA2: PCE Prices with Log-Linear Trend
if (exists("ll_combined")) {
  figA2_csv <- ll_combined %>%
    dplyr::select("Date" = date,
           "Core goods index" = core_goods_idx,
           "Core goods trend (log-linear)" = core_trend,
           "Durable goods index" = durables_idx,
           "Durable goods trend (log-linear)" = dur_trend) %>%
    mutate(Date = format_csv_date_line(Date))
  write_website_csv(figA2_csv, "FA2_pce_prices_loglinear_trend.csv", "FA2")
}

# FA3: Deviation from Log-Linear Trend
if (exists("ll_deviations")) {
  figA3_csv <- ll_deviations %>%
    dplyr::select("Date" = date,
                  "Core Goods" = core_goods_vs_trend,
                  "Durable Goods" = durables_vs_trend) %>%
    mutate(Date = format_csv_date(Date))
  write_website_csv(figA3_csv, "FA3_pce_deviation_loglinear.csv", "FA3")
}

website_csv_count <- length(list.files(CSV_DIR, pattern = "\\.csv$"))
message(paste("Website CSV export complete:", website_csv_count, "files to website/csv/"))
message(paste("Vintage saved to:", VINTAGE_DIR))

} # end if (PUBLICATION_RUN_FLAG)
