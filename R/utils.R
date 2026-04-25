# R/utils.R
# Shared utility functions for the CSEI pipeline

.libPaths("~/R/libs")

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(forcats)
library(ggplot2)
library(lubridate)

#' Convert Brazilian comma-decimal strings to numeric
#' Handles -9999 as NA (INMET missing code)
parse_comma_numeric <- function(x) {
  x <- as.character(x)
  x[x == "-9999" | x == "-9999,0"] <- NA_character_
  x <- str_replace(x, ",", ".")
  as.numeric(x)
}

#' Compute apparent temperature (simplified Steadman formula)
#' Uses temperature (C), relative humidity (%), and wind speed (m/s)
#' Reference: Steadman (1984), adapted by ABM
compute_apparent_temp <- function(temp_c, rh_pct, wind_ms) {
  # Water vapor pressure in kPa (Steadman formula requires kPa)
  e_hpa <- (rh_pct / 100) * 6.105 * exp((17.27 * temp_c) / (237.7 + temp_c))
  e_kpa <- e_hpa / 10
  # Apparent temperature (Steadman 1984)
  at <- -2.7 + 1.04 * temp_c + 2.0 * e_kpa - 0.65 * wind_ms
  at
}

#' Identify heat wave events from a logical vector of exceedance days
#' Returns a tibble of events with start, end, duration
identify_hw_events <- function(dates, above_threshold, min_duration) {
  if (all(!above_threshold | is.na(above_threshold))) {
    return(tibble(
      start = as.Date(character(0)),
      end = as.Date(character(0)),
      duration = integer(0)
    ))
  }

  # Run-length encoding on the exceedance vector
  rle_result <- rle(as.integer(above_threshold & !is.na(above_threshold)))
  ends <- cumsum(rle_result$lengths)
  starts <- ends - rle_result$lengths + 1

  # Filter runs of consecutive days above threshold with min duration
  hw_mask <- rle_result$values == 1 & rle_result$lengths >= min_duration

  if (!any(hw_mask)) {
    return(tibble(
      start = as.Date(character(0)),
      end = as.Date(character(0)),
      duration = integer(0)
    ))
  }

  tibble(
    start = dates[starts[hw_mask]],
    end = dates[ends[hw_mask]],
    duration = as.integer(rle_result$lengths[hw_mask])
  )
}

#' Compute coverage fraction for a date window given available climate dates
compute_coverage <- function(window_start, window_end, climate_dates) {
  total_days <- as.numeric(window_end - window_start) + 1
  if (total_days <= 0) return(0)
  covered <- sum(climate_dates >= window_start & climate_dates <= window_end)
  covered / total_days
}

#' Compute percentile threshold for daily climate data.
#'
#' Two modes, both returning a tibble (doy = 1..366, threshold) so callers can
#' left_join on `doy` regardless of the mode chosen:
#'
#'   * seasonal = FALSE (default): a single city-fixed percentile of the entire
#'     daily series, broadcast to all 366 DOYs. Use this for absolute heat
#'     thresholds (the lay meaning of "heatwave" — physiologically intense).
#'
#'   * seasonal = TRUE: per-DOY rolling threshold built from a +/- half_window
#'     pooled sample across years. Use this for anomaly-based definitions
#'     where "hot for the time of year" matters (acclimatization framing).
compute_seasonal_threshold <- function(daily_data, temp_col, percentile,
                                       half_window = 7, seasonal = FALSE) {
  if (!seasonal) {
    fixed_thr <- quantile(daily_data[[temp_col]], probs = percentile, na.rm = TRUE)
    return(tibble(doy = 1:366, threshold = as.numeric(fixed_thr)))
  }

  daily_data <- daily_data %>%
    mutate(doy = yday(date))

  # For each DOY, gather values within the window across all years
  doy_thresholds <- map_dfr(1:366, function(d) {
    # Handle wrap-around at year boundaries
    window_doys <- ((d - half_window):(d + half_window) - 1) %% 366 + 1
    vals <- daily_data %>%
      filter(doy %in% window_doys) %>%
      pull(!!sym(temp_col))
    tibble(
      doy = d,
      threshold = quantile(vals, probs = percentile, na.rm = TRUE)
    )
  })

  doy_thresholds
}
