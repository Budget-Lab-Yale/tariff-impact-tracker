# ==============================================================================
# Tariff Impacts - Shared Utility Functions
# ==============================================================================
#
# Purpose: Common functions used across tariff impacts scripts
#
# Usage:
#   source(here("R", "utils.R"))
#
# Note: This file uses the %>% pipe operator from dplyr/magrittr.
#   Ensure library(dplyr) is loaded before sourcing this file.
#
# Author: John Iselin, Yale Budget Lab
# Date: January 2026
# ==============================================================================

# Average days per month (365.25 / 12), used to convert day-based difftime to months
DAYS_PER_MONTH <- 365.25 / 12

# ==============================================================================
# LOGGING FUNCTIONS
# ==============================================================================

#' Create a logging function bound to a specific log file
#'
#' @param log_file Path to the log file
#' @return A logging function that writes to both console and file
create_logger <- function(log_file) {
  # Ensure directory exists
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)

  # Initialize log file
  cat(paste0("Log initialized: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"),
      paste0(rep("=", 70), "\n"),
      file = log_file, sep = "")

  # Return logging function
  function(level = "INFO", message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_line <- paste0("[", timestamp, "][", level, "] ", message, "\n")
    # Print to console
    cat(log_line)
    # Append to log file
    if (file.exists(log_file)) {
      cat(log_line, file = log_file, append = TRUE)
    }
  }
}

# ==============================================================================
# DATE NORMALIZATION
# ==============================================================================

#' Normalize monthly dates to first-of-month
#'
#' Converts end-of-month dates (e.g., 2024-01-31, 2024-02-29) to first-of-month
#' (e.g., 2024-01-01, 2024-02-01). This avoids leap-year date arithmetic issues
#' where lubridate::months() can produce mismatched February dates.
#'
#' @param df Data frame with a date column
#' @param date_col Name of the date column (default: "date")
#' @return Data frame with normalized dates

normalize_monthly_dates <- function(df, date_col = "date") {
  if (date_col %in% names(df)) {
    df[[date_col]] <- lubridate::floor_date(df[[date_col]], "month")
  }
  df
}

# ==============================================================================
# HAVER DATA FUNCTIONS
# ==============================================================================

#' Check if Haver connection is available
#'
#' @return TRUE if Haver is available, FALSE otherwise
check_haver_available <- function() {
  tryCatch({
    if (!requireNamespace("Haver", quietly = TRUE)) {
      return(FALSE)
    }
    Haver::haver.direct("on")
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' Pull data from Haver Analytics
#'
#' @param codes Vector of Haver codes to pull
#' @param frequency Data frequency ("monthly", "quarterly", "annual")
#' @param start_date Start date for data pull
#' @param end_date Optional end date for data pull
#' @param haver_available Boolean indicating if Haver is available
#' @param log_fn Optional logging function
#' @return Data frame with date column and data columns, or NULL if unavailable
pull_haver <- function(codes, frequency = "monthly", start_date, end_date = NULL,
                       haver_available = TRUE, log_fn = NULL) {
  if (!haver_available) {
    if (!is.null(log_fn)) log_fn("WARN", "Haver not available - returning NULL")
    return(NULL)
  }

  tryCatch({
    if (is.null(end_date)) {
      hd <- Haver::haver.data(codes = codes, frequency = frequency, start = start_date,
                              aggmode = "relaxed", eop.dates = TRUE)
    } else {
      hd <- Haver::haver.data(codes = codes, frequency = frequency, start = start_date,
                              end = end_date, aggmode = "relaxed", eop.dates = TRUE)
    }

    df <- as.data.frame(hd) %>%
      tibble::rownames_to_column(var = "date") %>%
      dplyr::mutate(date = as.Date(date))

    return(df)
  }, error = function(e) {
    if (!is.null(log_fn)) log_fn("ERROR", paste("Haver pull failed:", e$message))
    return(NULL)
  })
}


# ==============================================================================
# SAFE ARITHMETIC FUNCTIONS
# ==============================================================================

#' Safe division that handles NA, zero, and non-finite denominators
#'
#' @param numerator Numeric vector
#' @param denominator Numeric vector
#' @param default Value to return when division is undefined (default: 0)
#' @return Numeric vector with safe division results
safe_divide <- function(numerator, denominator, default = 0) {
  result <- numerator / denominator
  result[is.na(denominator) | denominator == 0 | !is.finite(result)] <- default
  result
}

#' Validate Haver codes by testing each one
#'
#' @param codes Character vector of Haver codes to validate
#' @param test_start Start date for test pull (default: "2024-01-01")
#' @param log_fn Optional logging function
#' @return List with $valid and $invalid character vectors
validate_haver_codes <- function(codes, test_start = "2024-01-01", log_fn = NULL) {
  valid_codes <- c()
  invalid_codes <- c()

  for (code in codes) {
    tryCatch({
      test <- Haver::haver.data(codes = code, frequency = "monthly", start = test_start,
                                aggmode = "relaxed", eop.dates = TRUE)
      if (!is.null(test) && nrow(test) > 0) {
        valid_codes <- c(valid_codes, code)
      } else {
        invalid_codes <- c(invalid_codes, code)
      }
    }, error = function(e) {
      invalid_codes <<- c(invalid_codes, code)
    })
  }

  if (!is.null(log_fn)) {
    log_fn("INFO", paste("Valid codes:", length(valid_codes), "of", length(codes)))
    if (length(invalid_codes) > 0) {
      log_fn("WARN", paste("Invalid/unavailable codes:", paste(invalid_codes, collapse = ", ")))
    }
  }

  list(valid = valid_codes, invalid = invalid_codes)
}

# ==============================================================================
# DATA CONVERSION FUNCTIONS
# ==============================================================================

#' Safely convert to numeric with logging of conversion failures
#'
#' @param x Vector to convert
#' @param col_name Name of column (for logging)
#' @param log_fn Optional logging function
#' @return Numeric vector with NAs for non-convertible values
safe_as_numeric <- function(x, col_name = "unknown", log_fn = NULL) {
  original_na_count <- sum(is.na(x))
  result <- as.numeric(x)
  new_na_count <- sum(is.na(result))

  conversion_failures <- new_na_count - original_na_count
  if (conversion_failures > 0 && !is.null(log_fn)) {
    log_fn("INFO", paste0("Column '", col_name, "': ", conversion_failures,
                          " non-numeric values converted to NA"))
  }

  result
}

# ==============================================================================
# LOG-LINEAR TREND FUNCTIONS
# ==============================================================================

#' Calculate log-linear trend with Newey-West standard errors
#'
#' Fits a linear model in log space: ln(y) = alpha + beta * t + epsilon
#' Uses Newey-West HAC standard errors for robustness to serial correlation.
#' Confidence intervals are calculated in log space and exponentiated.
#'
#' @param df Data frame with date and value columns
#' @param value_col Name of the value column to model
#' @param estimation_start Start date for model estimation (default: "2015-01-01")
#' @param estimation_end End date for model estimation (default: "2024-12-31")
#' @param base_date Date to use as index base (value = 100) (default: "2024-12-01")
#' @param forecast_start Start of forecast period for flagging (default: "2025-01-01")
#' @param conf_level Confidence level for intervals (default: 0.90)
#' @return Data frame with trend, confidence intervals, and deviations; model summary as attribute

calc_log_linear_trend <- function(df, value_col,
                                   estimation_start = "2015-01-01",
                                   estimation_end = "2024-12-31",
                                   base_date = "2024-12-01",
                                   forecast_start = "2025-01-01",
                                   conf_level = 0.90) {

  # 1. Create estimation dataset with log transformation
  est_data <- df %>%
    dplyr::filter(date >= as.Date(estimation_start) & date <= as.Date(estimation_end))

  # Warn if non-positive values exist (log will produce -Inf/NaN)
  n_nonpos <- sum(est_data[[value_col]] <= 0, na.rm = TRUE)
  if (n_nonpos > 0) {
    warning(paste0("calc_log_linear_trend: ", n_nonpos, " non-positive value(s) in '",
                   value_col, "' will be dropped from estimation"))
  }

  est_data <- est_data %>%
    dplyr::mutate(
      time_idx = as.numeric(difftime(date, as.Date(estimation_start), units = "days")) / DAYS_PER_MONTH,
      ln_y = log(.data[[value_col]])
    ) %>%
    dplyr::filter(!is.na(ln_y) & is.finite(ln_y))

  if (nrow(est_data) < 2) {
    stop(paste0("calc_log_linear_trend: insufficient data (", nrow(est_data),
                " rows) after filtering for '", value_col, "'"))
  }

  # 2. Fit linear model in log space: ln(y) = alpha + beta * t
  model <- lm(ln_y ~ time_idx, data = est_data)

  # 3. Get coefficient variance-covariance matrix with Newey-West
  # Bandwidth follows Andrews (1991): 4*(n/100)^(2/9), appropriate for OLS trend
  n <- nrow(est_data)
  nw_lag <- floor(4 * (n / 100)^(2/9))

  nw_vcov <- sandwich::NeweyWest(model, lag = nw_lag, prewhite = FALSE, adjust = TRUE)

  # Extract coefficients
  alpha <- coef(model)[1]
  beta <- coef(model)[2]

  # Variance of slope coefficient (used for forecast SE)
  var_beta <- nw_vcov[2, 2]

  # 4. Get base date time index
  base_time_idx <- as.numeric(difftime(as.Date(base_date),
                                        as.Date(estimation_start),
                                        units = "days")) / DAYS_PER_MONTH

  # 5. Create prediction dataset
  pred_data <- df %>%
    dplyr::mutate(
      time_idx = as.numeric(difftime(date, as.Date(estimation_start), units = "days")) / DAYS_PER_MONTH,
      h = time_idx - base_time_idx
    )

  # 6. Calculate log forecasts relative to base
  pred_data <- pred_data %>%
    dplyr::mutate(
      f_ln = beta * h,
      se_ln = abs(h) * sqrt(var_beta)
    )

  # 7. Convert to level space (indexed to base = 100)
  z_val <- qnorm(1 - (1 - conf_level) / 2)

  pred_data <- pred_data %>%
    dplyr::mutate(
      trend = 100 * exp(f_ln),
      trend_upper = trend * exp(z_val * se_ln),
      trend_lower = trend * exp(-z_val * se_ln),
      actual_idx = .data[[value_col]]
    )

  # Get the base value for indexing actuals
  base_row <- df %>%
    dplyr::mutate(date_diff = abs(as.numeric(date - as.Date(base_date)))) %>%
    dplyr::filter(date_diff == min(date_diff)) %>%
    dplyr::slice(1)

  base_value <- base_row %>% dplyr::pull(!!rlang::sym(value_col))

  if (length(base_value) == 1 && !is.na(base_value)) {
    pred_data <- pred_data %>%
      dplyr::mutate(actual_idx = .data[[value_col]] / base_value * 100)
  }

  # Deviation from trend (%)
  pred_data <- pred_data %>%
    dplyr::mutate(
      vs_trend = (actual_idx / trend - 1) * 100,
      is_forecast = date >= as.Date(forecast_start)
    )

  # Store model info as attributes
  model_summary <- list(
    alpha = alpha,
    beta = beta,
    var_beta = var_beta,
    nw_vcov = nw_vcov,
    monthly_growth_pct = beta * 100,
    annual_growth_pct = beta * 12 * 100,
    nw_lag = nw_lag,
    n_obs = n,
    r_squared = summary(model)$r.squared
  )

  result <- pred_data %>%
    dplyr::select(date, dplyr::all_of(value_col), h, trend, se_ln, trend_lower, trend_upper,
           vs_trend, is_forecast)

  attr(result, "model_summary") <- model_summary

  return(result)
}

# ==============================================================================
# LOCAL PROJECTION TREND FUNCTIONS (Stata-style methodology)
# ==============================================================================
#
# These functions implement the local projection methodology from the Yale Budget
# Lab tariff analysis, replicating the Stata code specification:
#
# Model: For each horizon h = 0, 1, ..., H:
#   (ln(y_{t+h}) - ln(y_t)) = β₀ + Σᵢβᵢ·L_i(ln(y)) + γ₁·pandemic + γ₂·pr_state + ε
#
# Where:
#   - L_i(ln(y)) = i-th lag of log level (i = 1, ..., 12)
#   - pandemic = dummy for 2020m1 - 2022m9
#   - pr_state = regime probability from Markov-switching model on 12-month log diff
#
# Standard errors: SE_h = stdp × 1.645 × √(h × (1 + h/T))
# This gives 90% confidence intervals.
#
# ==============================================================================

#' Create pandemic dummy variable
#'
#' Creates a binary indicator for the pandemic period (Jan 2020 - Sep 2022),
#' matching the Stata specification.
#'
#' @param dates Vector of dates
#' @param start Pandemic start date (default: "2020-01-01")
#' @param end Pandemic end date (default: "2022-09-30")
#' @return Numeric vector of 0/1 values

create_pandemic_dummy <- function(dates,
                                   start = "2020-01-01",
                                   end = "2022-09-30") {
  as.numeric(dates >= as.Date(start) & dates <= as.Date(end))
}

#' Create NBER recession dummy variable
#'
#' Creates a binary indicator for NBER-dated recessions within the estimation
#' window (1996+): the 2001 dot-com recession, the 2007-2009 Great Recession,
#' and the 2020 COVID recession.
#'
#' @param dates Vector of dates
#' @return Numeric vector of 0/1 values

create_recession_dummy <- function(dates) {
  d <- as.Date(dates)
  as.numeric(
    (d >= as.Date("2001-03-01") & d <= as.Date("2001-11-30")) |
    (d >= as.Date("2007-12-01") & d <= as.Date("2009-06-30")) |
    (d >= as.Date("2020-02-01") & d <= as.Date("2020-04-30"))
  )
}

#' Fit Markov-switching model using MSwM package
#'
#' Fits a 2-state Markov-switching model on the 12-month log difference
#' to identify high/low variance regimes. Returns regime probabilities.
#'
#' @param df Data frame with date and value columns
#' @param value_col Name of the value column
#' @param model_start Start date for estimation
#' @param model_end End date for estimation
#' @return List with regime probabilities and model object, or NULL if fitting fails

fit_markov_switching_mswm <- function(df, value_col,
                                       model_start = "1996-04-01",
                                       model_end = "2024-12-31") {

  if (!requireNamespace("MSwM", quietly = TRUE)) {
    warning("MSwM package not available. Install with: install.packages('MSwM')")
    return(NULL)
  }

  tryCatch({
    # Prepare data: create 12-month log difference
    ms_data <- df %>%
      dplyr::filter(date >= as.Date(model_start) & date <= as.Date(model_end)) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        ln_y = log(.data[[value_col]]),
        d_ln_y = ln_y - dplyr::lag(ln_y, 12)  # 12-month seasonal difference
      ) %>%
      dplyr::filter(!is.na(d_ln_y))

    # Fit simple AR(0) model first (intercept only)
    base_model <- lm(d_ln_y ~ 1, data = ms_data)

    # Fit 2-state Markov-switching model
    # k = 2 states, sw = c(TRUE, TRUE) means both intercept and variance switch
    ms_model <- MSwM::msmFit(base_model, k = 2, sw = c(TRUE, TRUE),
                              control = list(maxiter = 200))

    # Extract smoothed regime probabilities (probability of being in state 2)
    probs <- ms_model@Fit@smoProb[, 2]

    # Handle length mismatch: MSwM sometimes returns fewer probabilities than input rows
    # Pad with NA at the end if needed (the smoothing algorithm may drop boundary obs)
    n_dates <- length(ms_data$date)
    n_probs <- length(probs)
    if (n_probs < n_dates) {
      probs <- c(probs, rep(NA, n_dates - n_probs))
    } else if (n_probs > n_dates) {
      probs <- probs[1:n_dates]
    }

    # Ensure state 2 is the high-variance regime.
    # State labeling is arbitrary in Markov-switching models and can flip
    # between runs. Identify the correct state by comparing sample variance
    # of d_ln_y in each most-likely-state partition.
    most_likely <- ifelse(probs > 0.5, 2, 1)
    var_s1 <- var(ms_data$d_ln_y[most_likely == 1], na.rm = TRUE)
    var_s2 <- var(ms_data$d_ln_y[most_likely == 2], na.rm = TRUE)
    if (!is.na(var_s1) && !is.na(var_s2) && var_s1 > var_s2) {
      probs <- 1 - probs
    }

    # Create data frame with dates and probabilities
    prob_df <- data.frame(
      date = ms_data$date,
      pr_state = probs
    )

    # Calculate 2024 average for forecast periods
    avg_2024 <- prob_df %>%
      dplyr::filter(lubridate::year(date) == 2024) %>%
      dplyr::summarize(mean_pr = mean(pr_state, na.rm = TRUE)) %>%
      dplyr::pull(mean_pr)

    return(list(
      prob_df = prob_df,
      avg_2024 = avg_2024,
      model = ms_model,
      method = "MSwM"
    ))

  }, error = function(e) {
    warning(paste("MSwM fitting failed:", e$message))
    return(NULL)
  })
}

#' Fit Markov-switching model using depmixS4 package
#'
#' Fits a 2-state Hidden Markov Model on the 12-month log difference
#' to identify high/low variance regimes. Returns regime probabilities.
#'
#' @param df Data frame with date and value columns
#' @param value_col Name of the value column
#' @param model_start Start date for estimation
#' @param model_end End date for estimation
#' @return List with regime probabilities and model object, or NULL if fitting fails

fit_markov_switching_depmix <- function(df, value_col,
                                         model_start = "1996-04-01",
                                         model_end = "2024-12-31") {

  if (!requireNamespace("depmixS4", quietly = TRUE)) {
    warning("depmixS4 package not available. Install with: install.packages('depmixS4')")
    return(NULL)
  }

  tryCatch({
    # Prepare data: create 12-month log difference
    ms_data <- df %>%
      dplyr::filter(date >= as.Date(model_start) & date <= as.Date(model_end)) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        ln_y = log(.data[[value_col]]),
        d_ln_y = ln_y - dplyr::lag(ln_y, 12)
      ) %>%
      dplyr::filter(!is.na(d_ln_y))

    # Fit 2-state HMM with Gaussian emissions
    # Response is the 12-month log difference
    mod_spec <- depmixS4::depmix(
      response = d_ln_y ~ 1,
      data = ms_data,
      nstates = 2,
      family = gaussian()
    )

    # Fit the model
    set.seed(42)  # For reproducibility
    mod_fit <- depmixS4::fit(mod_spec, verbose = FALSE)

    # Get posterior state probabilities
    post_probs <- depmixS4::posterior(mod_fit)

    # Start with state 2 probability
    pr_state <- post_probs$S2

    # Handle potential length mismatch (for consistency with MSwM fix)
    n_dates <- length(ms_data$date)
    n_probs <- length(pr_state)
    if (n_probs < n_dates) {
      pr_state <- c(pr_state, rep(NA, n_dates - n_probs))
    } else if (n_probs > n_dates) {
      pr_state <- pr_state[1:n_dates]
    }

    # Ensure state 2 is the high-variance regime.
    # State labeling is arbitrary in HMMs and can flip between runs.
    # Identify the correct state by comparing sample variance of d_ln_y
    # in each most-likely-state partition.
    most_likely <- ifelse(pr_state > 0.5, 2, 1)
    var_s1 <- var(ms_data$d_ln_y[most_likely == 1], na.rm = TRUE)
    var_s2 <- var(ms_data$d_ln_y[most_likely == 2], na.rm = TRUE)
    if (!is.na(var_s1) && !is.na(var_s2) && var_s1 > var_s2) {
      pr_state <- 1 - pr_state
    }

    # Create data frame with dates and probabilities
    prob_df <- data.frame(
      date = ms_data$date,
      pr_state = pr_state
    )

    # Calculate 2024 average for forecast periods
    avg_2024 <- prob_df %>%
      dplyr::filter(lubridate::year(date) == 2024) %>%
      dplyr::summarize(mean_pr = mean(pr_state, na.rm = TRUE)) %>%
      dplyr::pull(mean_pr)

    return(list(
      prob_df = prob_df,
      avg_2024 = avg_2024,
      model = mod_fit,
      method = "depmixS4"
    ))

  }, error = function(e) {
    warning(paste("depmixS4 fitting failed:", e$message))
    return(NULL)
  })
}

#' Local projection trend with MSwM regime switching
#'
#' Convenience wrapper around \code{local_projection_trend_plain} with
#' \code{ms_method = "mswm"} and default horizon = 6.
#'
#' @inheritParams local_projection_trend_plain

local_projection_trend_mswm <- function(df, value_col,
                                         model_start = "1996-04-01",
                                         model_end = "2024-12-31",
                                         base_date = "2024-12-01",
                                         forecast_start = "2025-01-01",
                                         horizon = 6,
                                         n_lags = 12,
                                         conf_level = 0.90) {

  local_projection_trend_plain(
    df = df, value_col = value_col,
    model_start = model_start, model_end = model_end,
    base_date = base_date, forecast_start = forecast_start,
    horizon = horizon, n_lags = n_lags, conf_level = conf_level,
    ms_method = "mswm"
  )
}

#' Local projection trend with depmixS4 regime switching
#'
#' Convenience wrapper around \code{local_projection_trend_plain} with
#' \code{ms_method = "depmix"} and default horizon = 6.
#'
#' @inheritParams local_projection_trend_plain

local_projection_trend_depmix <- function(df, value_col,
                                           model_start = "1996-04-01",
                                           model_end = "2024-12-31",
                                           base_date = "2024-12-01",
                                           forecast_start = "2025-01-01",
                                           horizon = 6,
                                           n_lags = 12,
                                           conf_level = 0.90) {

  local_projection_trend_plain(
    df = df, value_col = value_col,
    model_start = model_start, model_end = model_end,
    base_date = base_date, forecast_start = forecast_start,
    horizon = horizon, n_lags = n_lags, conf_level = conf_level,
    ms_method = "depmix"
  )
}

# ==============================================================================
# LOCAL PROJECTION TREND — PLAIN (primary entry point)
# ==============================================================================

#' Local projection trend estimation
#'
#' Core local projection implementation. When ms_method = "none" (the default),
#' runs a plain LP regression using only lagged log levels as controls.
#' When ms_method = "mswm" or "depmix", also includes a pandemic dummy and
#' Markov-switching regime probabilities as controls.
#'
#' @param df Data frame with date and value columns
#' @param value_col Name of the value column to model
#' @param model_start Start date for model estimation
#' @param model_end End date for model estimation
#' @param base_date Date to use as index base (value = 100)
#' @param forecast_start Start of forecast period
#' @param horizon Maximum forecast horizon in months
#' @param n_lags Number of lags of log level to include (default: 12)
#' @param conf_level Confidence level (default: 0.90)
#' @param seasonal Logical; if TRUE, include month-of-year dummies (default: FALSE)
#' @param indicators Logical; if TRUE, include NBER recession and pandemic
#'   dummy variables as additional regressors (default: FALSE)
#' @param ms_method Markov-switching method: "none" (default), "mswm", or "depmix"
#' @return Data frame with trend, confidence intervals, and deviations

local_projection_trend_plain <- function(df, value_col,
                                          model_start = "1996-04-01",
                                          model_end = "2024-12-31",
                                          base_date = "2024-12-01",
                                          forecast_start = "2025-01-01",
                                          horizon = 12,
                                          n_lags = 12,
                                          conf_level = 0.90,
                                          seasonal = FALSE,
                                          indicators = FALSE,
                                          ms_method = "none") {

  # Step 1: Index data to base_date = 100
  base_row <- df %>%
    dplyr::mutate(date_diff = abs(as.numeric(date - as.Date(base_date)))) %>%
    dplyr::filter(date_diff == min(date_diff)) %>%
    dplyr::slice(1)

  base_value <- base_row[[value_col]]
  if (length(base_value) == 0 || is.na(base_value)) {
    stop(paste0("local_projection_trend_plain: base date value is missing or NA for '",
                value_col, "' at base_date=", base_date))
  }

  df <- df %>%
    dplyr::mutate(
      value_indexed = .data[[value_col]] / base_value * 100,
      ln_y = log(value_indexed)
    )

  # Step 2: Create lagged variables
  df <- df %>% dplyr::arrange(date)

  for (i in 1:n_lags) {
    lag_name <- paste0("L", i, "_ln_y")
    df[[lag_name]] <- dplyr::lag(df$ln_y, i)
  }

  # Step 2b: Optional controls
  if (seasonal) {
    df$month_factor <- factor(lubridate::month(df$date))
  }

  if (indicators || ms_method != "none") {
    # Pandemic dummy is always included when using MS methods
    df$pandemic <- create_pandemic_dummy(df$date)
  }

  if (indicators) {
    df$recession <- create_recession_dummy(df$date)
  }

  # Step 2c: Markov-switching regime probabilities
  ms_result <- NULL
  if (ms_method != "none") {
    if (ms_method == "mswm") {
      ms_result <- fit_markov_switching_mswm(df, "value_indexed", model_start, model_end)
    } else {
      ms_result <- fit_markov_switching_depmix(df, "value_indexed", model_start, model_end)
    }

    if (is.null(ms_result)) {
      warning("Markov-switching failed. Using constant regime probability = 0.5")
      df$pr_state <- 0.5
      avg_2024_pr <- 0.5
    } else {
      df <- df %>% dplyr::left_join(ms_result$prob_df, by = "date")
      avg_2024_pr <- ms_result$avg_2024
      df$pr_state[is.na(df$pr_state)] <- avg_2024_pr
    }
  }

  # Step 3: Filter to estimation period
  est_data <- df %>%
    dplyr::filter(date >= as.Date(model_start) & date <= as.Date(model_end))

  T_obs <- nrow(est_data)
  nw_lag <- round(T_obs^(1/4))  # Standard N^(1/4) rule for LP regressions

  # Step 4: Build formula and run local projections
  lag_vars <- paste0("L", 1:n_lags, "_ln_y", collapse = " + ")
  rhs <- lag_vars
  if (seasonal) rhs <- paste(rhs, "+ month_factor")
  if (indicators) rhs <- paste(rhs, "+ recession + pandemic")
  if (ms_method != "none") {
    rhs <- paste(rhs, "+ pr_state")
    if (!indicators) rhs <- paste(rhs, "+ pandemic")  # Always include with MS
  }

  req_cols <- c("y_h", paste0("L", 1:n_lags, "_ln_y"))
  if (seasonal) req_cols <- c(req_cols, "month_factor")
  if (indicators) req_cols <- c(req_cols, "recession", "pandemic")
  if (ms_method != "none") {
    req_cols <- c(req_cols, "pr_state")
    if (!indicators) req_cols <- c(req_cols, "pandemic")
  }

  horizon_results <- list()

  for (h in 0:horizon) {
    est_data_h <- est_data %>%
      dplyr::mutate(y_h = dplyr::lead(ln_y, h) - ln_y) %>%
      dplyr::filter(!is.na(y_h))

    complete_rows <- complete.cases(est_data_h[, req_cols])
    est_data_h <- est_data_h[complete_rows, ]

    if (nrow(est_data_h) < n_lags + 10) {
      warning(paste("Insufficient observations for horizon", h))
      next
    }

    formula_h <- as.formula(paste("y_h ~", rhs))
    model_h <- lm(formula_h, data = est_data_h)
    nw_vcov_h <- sandwich::NeweyWest(model_h, lag = nw_lag, prewhite = FALSE)

    horizon_results[[h + 1]] <- list(
      horizon = h,
      model = model_h,
      nw_vcov = nw_vcov_h,
      n_obs = nrow(est_data_h)
    )
  }

  # Step 5: Generate forecasts at base_date
  base_pred_row <- df %>%
    dplyr::filter(abs(as.numeric(date - as.Date(base_date))) ==
                    min(abs(as.numeric(date - as.Date(base_date))))) %>%
    dplyr::slice(1)

  output_dates <- df %>%
    dplyr::filter(date >= as.Date(model_start)) %>%
    dplyr::select(date, value_indexed, ln_y)

  output <- output_dates %>%
    dplyr::mutate(
      h = as.numeric(difftime(date, as.Date(base_date), units = "days")) / DAYS_PER_MONTH,
      h_int = round(h)
    )

  forecast_info <- lapply(horizon_results, function(hr) {
    if (is.null(hr)) return(NULL)

    h <- hr$horizon
    model <- hr$model

    pred <- predict(model, newdata = base_pred_row, se.fit = TRUE)
    f_ln_h <- pred$fit
    se_fit <- pred$se.fit

    z_val <- qnorm(1 - (1 - conf_level) / 2)

    if (h == 0) {
      se_adj <- 0
    } else {
      se_adj <- se_fit * z_val * sqrt(h * (1 + h / T_obs))
    }

    data.frame(horizon = h, f_ln = f_ln_h, se_ln = se_adj)
  })

  forecast_df <- do.call(rbind, forecast_info[!sapply(forecast_info, is.null)])

  if (is.null(forecast_df) || nrow(forecast_df) == 0) {
    stop("local_projection_trend_plain: all horizon regressions failed — no forecast data produced")
  }

  # Step 6: Compute trend and confidence intervals
  output <- output %>%
    dplyr::left_join(forecast_df, by = c("h_int" = "horizon"))

  if (nrow(forecast_df) >= 2) {
    extrap_model <- lm(f_ln ~ horizon, data = forecast_df)
    extrap_slope <- coef(extrap_model)[2]
    extrap_intercept <- coef(extrap_model)[1]

    avg_se_fit <- mean(forecast_df$se_ln / (qnorm(1 - (1 - conf_level) / 2) *
                         sqrt(pmax(forecast_df$horizon, 1) *
                                (1 + pmax(forecast_df$horizon, 1) / T_obs))), na.rm = TRUE)

    z_val <- qnorm(1 - (1 - conf_level) / 2)

    output <- output %>%
      dplyr::mutate(
        f_ln = ifelse(is.na(f_ln),
                      extrap_intercept + extrap_slope * h,
                      f_ln),
        se_ln = ifelse(is.na(se_ln) & h != 0,
                       avg_se_fit * z_val * sqrt(abs(h) * (1 + abs(h) / T_obs)),
                       se_ln),
        se_ln = ifelse(is.na(se_ln), 0, se_ln)
      )
  }

  output <- output %>%
    dplyr::mutate(
      trend = 100 * exp(f_ln),
      trend_upper = 100 * exp(f_ln + se_ln),
      trend_lower = 100 * exp(f_ln - se_ln),
      actual_idx = value_indexed,
      vs_trend = (actual_idx / trend - 1) * 100,
      is_forecast = date >= as.Date(forecast_start)
    )

  result <- output %>%
    dplyr::select(date, actual_idx, h, trend, se_ln, trend_lower, trend_upper,
                  vs_trend, is_forecast)

  result[[value_col]] <- df$value_indexed[match(result$date, df$date)] * base_value / 100

  model_summary <- list(
    method = paste0("local_projection_", ms_method),
    model_start = model_start,
    model_end = model_end,
    base_date = base_date,
    horizon = horizon,
    n_lags = n_lags,
    conf_level = conf_level,
    seasonal = seasonal,
    indicators = indicators,
    nw_lag = nw_lag,
    T_obs = T_obs,
    ms_method = ms_method,
    ms_result = if (!is.null(ms_result)) list(avg_2024 = ms_result$avg_2024,
                                               method = ms_result$method) else NULL,
    horizon_models = lapply(horizon_results, function(hr) {
      if (is.null(hr)) return(NULL)
      list(horizon = hr$horizon, n_obs = hr$n_obs, r_squared = summary(hr$model)$r.squared)
    })
  )

  attr(result, "model_summary") <- model_summary

  return(result)
}

# ==============================================================================
# HAMILTON FILTER TREND ESTIMATION (v2 — corrected)
# ==============================================================================
#
# Implements the Hamilton (2018) regression-based filter for detrending.
# See: "Why You Should Never Use the Hodrick-Prescott Filter"
# https://econweb.ucsd.edu/~jhamilto/hp.pdf
#
# CHANGES FROM v1:
#   1. Multi-horizon direct forecasts: estimates a separate regression for each
#      h = 1, 2, ..., max_h so the output contains a smooth month-by-month
#      forecast path (not just a single point at h = 24).
#   2. Jensen's inequality correction: exp(mu + sigma^2/2) is used when
#      converting log forecasts back to levels, removing systematic downward
#      bias in the point forecast.
#   3. Newey-West HAC standard errors with lag = h-1 to account for the
#      overlapping-residual MA(h-1) structure.
#   4. Robust date arithmetic via lubridate %m-% instead of manual
#      year-month integer math.
#   5. CI documentation clarified: "constant in log / percentage-of-trend
#      terms" rather than "constant" without qualification.
#
# The primary regression specification (for a given horizon h) is:
#
#   ln(y_{t+h}) = alpha + beta_0 * ln(y_t) + beta_1 * ln(y_{t-1})
#                 + ... + beta_{p-1} * ln(y_{t-p+1}) + epsilon_{t+h}
#
# Key properties:
#   - One-sided: uses only past information, avoiding end-of-sample bias
#   - Direct multi-step: no iterated forecasts, so no error accumulation
#   - HAC standard errors: correct for overlapping h-step residuals
#   - Jensen-corrected levels: unbiased retransformation from log to level
#
# ==============================================================================

#' Hamilton filter trend estimation (multi-horizon, HAC, Jensen-corrected)
#'
#' @param df           Data frame with a `date` column and at least one value column.
#' @param value_col    Name of the value column to model (character).
#' @param model_start  Start date for estimation (default: "1996-04-01").
#' @param model_end    End date for estimation (default: "2024-12-31").
#' @param base_date    Date whose value is set to 100 for indexing (default: "2024-12-01").
#' @param forecast_start  First date flagged as `is_forecast` (default: "2025-01-01").
#' @param max_h        Maximum forecast horizon in months (default: 24).
#'                     A separate regression is estimated for each h in 1:max_h.
#' @param p            Number of lags (regressors) in each regression (default: 12).
#' @param conf_level   Confidence level for intervals (default: 0.90).
#' @param use_hac      Logical; if TRUE (default) use Newey-West HAC SEs with
#'                     lag = h-1. If FALSE, use plain OLS sigma (Hamilton's
#'                     original specification).
#' @return A data frame with columns:
#'   \describe{
#'     \item{date}{Date}
#'     \item{actual_idx}{Observed index (base_date = 100)}
#'     \item{trend}{Point forecast in index space (Jensen-corrected)}
#'     \item{trend_lower}{Lower confidence bound}
#'     \item{trend_upper}{Upper confidence bound}
#'     \item{vs_trend}{Percent deviation of actual from trend}
#'     \item{is_forecast}{TRUE for dates >= forecast_start}
#'   }
#'   The returned object also carries a `"model_summary"` attribute with
#'   estimation details for each horizon.

hamilton_filter_trend <- function(df, value_col,
                                  model_start = "1996-04-01",
                                  model_end = "2024-12-31",
                                  base_date = "2024-12-01",
                                  forecast_start = "2025-01-01",
                                  max_h = 24,
                                  p = 12,
                                  conf_level = 0.90,
                                  use_hac = TRUE) {

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  if (use_hac) requireNamespace("sandwich", quietly = TRUE)

  # ==========================================================================
  # Step 1: Index data to base_date = 100, compute log
  # ==========================================================================
  base_row <- df %>%
    dplyr::mutate(.date_diff = abs(as.numeric(date - as.Date(base_date)))) %>%
    dplyr::filter(.date_diff == min(.date_diff)) %>%
    dplyr::select(-.date_diff) %>%
    dplyr::slice(1)

  base_value <- base_row[[value_col]]
  if (length(base_value) == 0 || is.na(base_value)) {
    stop(paste0("hamilton_filter_trend: base date value is missing or NA for '",
                value_col, "' at base_date=", base_date))
  }

  df <- df %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      value_indexed = .data[[value_col]] / base_value * 100,
      ln_y = log(value_indexed)
    )

  # ==========================================================================
  # Step 2: Create lagged regressors: y_t, y_{t-1}, ..., y_{t-p+1}
  # ==========================================================================
  for (i in 0:(p - 1)) {
    df[[paste0("L", i, "_ln_y")]] <- dplyr::lag(df$ln_y, i)
  }

  lag_cols <- paste0("L", 0:(p - 1), "_ln_y")

  # ==========================================================================
  # Step 3: Filter to estimation window
  # ==========================================================================
  est_data <- df %>%
    dplyr::filter(date >= as.Date(model_start) & date <= as.Date(model_end))

  # Drop rows where any lag is NA (beginning of sample)
  complete_lag_rows <- complete.cases(est_data[, lag_cols])
  est_data <- est_data[complete_lag_rows, ]
  T_obs <- nrow(est_data)

  # ==========================================================================
  # Step 4: Estimate a separate regression for each horizon h = 1 .. max_h
  # ==========================================================================
  formula_rhs <- paste(lag_cols, collapse = " + ")
  z_crit <- qnorm(1 - (1 - conf_level) / 2)

  # Store per-horizon model objects and sigma values
  horizon_models <- vector("list", max_h)

  for (h in 1:max_h) {
    # Create dependent variable: ln(y_{t+h})
    est_data_h <- est_data %>%
      dplyr::mutate(ln_y_h = dplyr::lead(ln_y, h)) %>%
      dplyr::filter(!is.na(ln_y_h))

    if (nrow(est_data_h) < p + 10) {
      warning(paste("Insufficient observations for horizon", h,
                    "- skipping"))
      next
    }

    model_h <- lm(as.formula(paste("ln_y_h ~", formula_rhs)),
                   data = est_data_h)

    # Standard error of regression
    sigma_ols <- summary(model_h)$sigma

    # Newey-West HAC standard errors (lag = h-1 for MA(h-1) overlap)
    if (use_hac && h > 1) {
      nw_vcov <- sandwich::NeweyWest(model_h,
                                      lag = h - 1,
                                      prewhite = FALSE,
                                      adjust = TRUE)
    } else {
      nw_vcov <- vcov(model_h)
    }

    horizon_models[[h]] <- list(
      h = h,
      model = model_h,
      coefs = coef(model_h),
      sigma = sigma_ols,
      nw_vcov = nw_vcov,
      r_squared = summary(model_h)$r.squared,
      n_obs = nrow(est_data_h)
    )
  }

  # ==========================================================================
  # Step 5: Build the output frame — in-sample trend + out-of-sample forecast
  # ==========================================================================
  #
  # For EACH date d in the data, we want a "trend" value.  The trend at date
  # d is: "what would we have predicted for d using information available at
  # the best available origin?"
  #
  # In-sample (d <= model_end):
  #   The origin is d %m-% months(h_best), where h_best is the horizon whose
  #   origin still falls inside the estimation window.  For the "primary"
  #   trend we use the *longest* horizon whose origin has complete data.
  #   This keeps the trend smooth and is closest to Hamilton's h = 24.
  #
  # Out-of-sample (d > model_end):
  #   For each target date d in the forecast period, horizon h is the number
  #   of months between base_date and d.  We predict using the base_date row
  #   as the origin.  This produces the smooth month-by-month path you need
  #   for plotting (one point per month, h = 1, 2, ..., max_h).
  # ==========================================================================

  forecast_start_date <- as.Date(forecast_start)
  base_date_parsed <- as.Date(base_date)

  # --- 5a. Out-of-sample forecast path from base_date ---
  # Get the regressor vector at the base date
  base_row_data <- df %>%
    dplyr::filter(
      abs(as.numeric(date - base_date_parsed)) ==
        min(abs(as.numeric(date - base_date_parsed)))
    ) %>%
    dplyr::slice(1)

  forecast_rows <- list()

  for (h in 1:max_h) {
    hm <- horizon_models[[h]]
    if (is.null(hm)) next

    # Construct the predictor vector (intercept + p lags)
    x_vec <- c(1, as.numeric(base_row_data[, lag_cols]))
    if (any(is.na(x_vec))) next

    # Point forecast in log space
    ln_y_hat <- sum(hm$coefs * x_vec)

    # Prediction SE: sqrt(x' * V * x) where V is (possibly HAC) vcov
    x_mat <- matrix(x_vec, ncol = 1)
    se_pred <- as.numeric(sqrt(t(x_mat) %*% hm$nw_vcov %*% x_mat))

    # The forecast target date
    target_date <- base_date_parsed %m+% months(h)
    target_date <- lubridate::floor_date(target_date, "month")

    # Jensen-corrected level forecast: exp(mu + sigma^2/2)
    sigma_h <- hm$sigma
    trend_level <- exp(ln_y_hat + sigma_h^2 / 2)
    trend_lower <- exp(ln_y_hat + sigma_h^2 / 2 - z_crit * se_pred)
    trend_upper <- exp(ln_y_hat + sigma_h^2 / 2 + z_crit * se_pred)

    forecast_rows[[h]] <- data.frame(
      date = target_date,
      ln_y_fitted = ln_y_hat,
      sigma_h = sigma_h,
      se_pred = se_pred,
      trend = trend_level,
      trend_lower = trend_lower,
      trend_upper = trend_upper,
      source = "forecast",
      stringsAsFactors = FALSE
    )
  }

  forecast_df <- do.call(rbind, forecast_rows)

  # --- 5b. In-sample trend using the h = max_h regression ---
  #
  # For each row in the estimation data, the fitted value from the h = max_h
  # regression tells us what the model predicted for date (t + max_h) given
  # info at t.  We align this so the "trend at date d" equals the fitted
  # value whose *target* is d.
  #
  # This means: for date d, the origin was d - max_h months, and we look up
  # the fitted value from that origin row.

  primary_h <- max_h
  hm_primary <- horizon_models[[primary_h]]

  insample_rows <- NULL
  if (!is.null(hm_primary)) {
    # Re-run the primary regression to get fitted values for alignment
    est_primary <- est_data %>%
      dplyr::mutate(ln_y_h = dplyr::lead(ln_y, primary_h)) %>%
      dplyr::filter(!is.na(ln_y_h))

    est_primary$ln_y_fitted <- fitted(hm_primary$model)
    sigma_primary <- hm_primary$sigma

    # The fitted value at row with origin date t corresponds to target
    # date t + max_h months
    insample_rows <- est_primary %>%
      dplyr::mutate(
        target_date = lubridate::floor_date(date %m+% months(primary_h), "month"),
        sigma_h = sigma_primary,
        trend = exp(ln_y_fitted + sigma_h^2 / 2),
        trend_lower = exp(ln_y_fitted + sigma_h^2 / 2 - z_crit * sigma_h),
        trend_upper = exp(ln_y_fitted + sigma_h^2 / 2 + z_crit * sigma_h),
        source = "insample"
      ) %>%
      dplyr::select(
        date = target_date, ln_y_fitted, sigma_h,
        se_pred = sigma_h,
        trend, trend_lower, trend_upper, source
      )
  }

  # ==========================================================================
  # Step 6: Merge in-sample trend and out-of-sample forecast onto the
  #         full date index, using year-month matching for robustness
  # ==========================================================================

  result <- df %>%
    dplyr::select(date, value_indexed, ln_y) %>%
    dplyr::mutate(
      ym = as.numeric(format(date, "%Y")) * 12 +
           as.numeric(format(date, "%m"))
    )

  # Combine in-sample and forecast, preferring forecast where both exist
  trend_data <- dplyr::bind_rows(insample_rows, forecast_df)
  trend_data <- trend_data %>%
    dplyr::mutate(
      ym = as.numeric(format(date, "%Y")) * 12 +
           as.numeric(format(date, "%m"))
    )

  # For dates where both in-sample and forecast exist, keep forecast
  trend_data <- trend_data %>%
    dplyr::arrange(ym, dplyr::desc(source == "forecast")) %>%
    dplyr::group_by(ym) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  result <- result %>%
    dplyr::left_join(
      trend_data %>% dplyr::select(ym, trend, trend_lower, trend_upper),
      by = "ym"
    ) %>%
    dplyr::select(-ym)

  # ==========================================================================
  # Step 7: Compute deviations and flags
  # ==========================================================================
  result <- result %>%
    dplyr::mutate(
      actual_idx = value_indexed,
      vs_trend = (actual_idx / trend - 1) * 100,
      is_forecast = date >= forecast_start_date
    ) %>%
    dplyr::select(date, actual_idx, trend, trend_lower, trend_upper,
                  vs_trend, is_forecast)

  # ==========================================================================
  # Step 8: Store model summary as attribute
  # ==========================================================================
  model_summary <- list(
    method = "hamilton_v2",
    max_h = max_h,
    p = p,
    T_obs = T_obs,
    use_hac = use_hac,
    conf_level = conf_level,
    jensen_corrected = TRUE,
    # Summary for the primary (max_h) horizon
    h = max_h,
    sigma = if (!is.null(hm_primary)) hm_primary$sigma else NA,
    r_squared = if (!is.null(hm_primary)) hm_primary$r_squared else NA,
    adj_r_squared = if (!is.null(hm_primary))
      summary(hm_primary$model)$adj.r.squared else NA,
    coefficients = if (!is.null(hm_primary)) hm_primary$coefs else NULL,
    # Full per-horizon detail
    horizon_detail = lapply(horizon_models, function(hm) {
      if (is.null(hm)) return(NULL)
      list(h = hm$h, sigma = hm$sigma, r_squared = hm$r_squared,
           n_obs = hm$n_obs)
    }),
    model = if (!is.null(hm_primary)) hm_primary$model else NULL
  )

  attr(result, "model_summary") <- model_summary

  return(result)
}
