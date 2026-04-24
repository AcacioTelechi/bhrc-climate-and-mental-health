# R/02_clean_daily_climate.R
# Read hourly INMET data, aggregate to daily, apply QC, compute apparent temp.
# Input:  data/raw/{porto_alegre,sao_paulo}/*.CSV and *.xlsx
# Output: data/processed/daily_climate_poa.rds
#         data/processed/daily_climate_sp.rds

source("R/utils.R")
library(readxl)

# ---------------------------------------------------------------------------
# Column names (19 columns after skipping metadata rows)
# ---------------------------------------------------------------------------
COL_NAMES <- c(
  "date", "hour_utc",
  "precip_mm",
  "pressure_mbar", "pressure_max_mbar", "pressure_min_mbar",
  "radiation_kj",
  "temp_drybulb_c", "dewpoint_c",
  "tmax_c", "tmin_c",
  "dewpoint_max_c", "dewpoint_min_c",
  "rh_max_pct", "rh_min_pct", "rh_pct",
  "wind_dir_deg", "wind_gust_ms", "wind_speed_ms"
)

# ---------------------------------------------------------------------------
# Read a single CSV file (INMET hourly, semicolon-separated, Latin-1)
# ---------------------------------------------------------------------------
read_inmet_csv <- function(path) {
  raw <- read_delim(
    path,
    delim         = ";",
    skip          = 8,
    col_names     = FALSE,
    locale        = locale(encoding = "latin1", decimal_mark = ","),
    col_types     = cols(.default = col_character()),
    show_col_types = FALSE,
    trim_ws       = TRUE
  )

  # Drop the trailing empty column created by the trailing semicolon
  raw <- raw[, seq_len(min(19, ncol(raw)))]

  # Skip the header row (row 1 after skipping metadata)
  raw <- raw[-1, ]

  # Assign standard column names
  names(raw) <- COL_NAMES

  # Parse date: handle both YYYY-MM-DD and YYYY/MM/DD formats
  raw <- raw %>%
    mutate(
      date = as.Date(str_replace_all(date, "/", "-")),
      across(all_of(COL_NAMES[-(1:2)]), parse_comma_numeric)
    )

  raw
}

# ---------------------------------------------------------------------------
# Read all files in a city directory; 
# Strategy: read CSV for all years; combine and
# deduplicate by date + hour_utc keeping xlsx rows where both exist.
# ---------------------------------------------------------------------------
read_city_hourly <- function(dir_path) {
  csv_files  <- sort(list.files(dir_path, pattern = "\\.CSV$",  full.names = TRUE))
  xlsx_files <- sort(list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE))

  message(sprintf("  Reading %d CSV files and %d xlsx files from %s",
                  length(csv_files), length(xlsx_files), dir_path))

  csv_data <- map_dfr(csv_files, function(f) {
    message(sprintf("    CSV: %s", basename(f)))
    tryCatch(read_inmet_csv(f), error = function(e) {
      warning(sprintf("Failed to read %s: %s", f, conditionMessage(e)))
      NULL
    })
  })

  csv_data %>%
    filter(!is.na(date)) %>%
    arrange(date)
}

# ---------------------------------------------------------------------------
# Apply physical-range QC to hourly data
# ---------------------------------------------------------------------------
apply_hourly_qc <- function(df) {
  df %>% mutate(
    tmax_c          = if_else(tmax_c < -10 | tmax_c > 50,          NA_real_, tmax_c),
    tmin_c          = if_else(tmin_c < -10 | tmin_c > 50,          NA_real_, tmin_c),
    temp_drybulb_c  = if_else(temp_drybulb_c < -10 | temp_drybulb_c > 50, NA_real_, temp_drybulb_c),
    rh_pct          = if_else(rh_pct < 0 | rh_pct > 100,           NA_real_, rh_pct),
    rh_max_pct      = if_else(rh_max_pct < 0 | rh_max_pct > 100,   NA_real_, rh_max_pct),
    rh_min_pct      = if_else(rh_min_pct < 0 | rh_min_pct > 100,   NA_real_, rh_min_pct),
    precip_mm       = if_else(precip_mm < 0,                        NA_real_, precip_mm),
    wind_speed_ms   = if_else(wind_speed_ms < 0 | wind_speed_ms > 50, NA_real_, wind_speed_ms),
    wind_gust_ms    = if_else(wind_gust_ms < 0 | wind_gust_ms > 50, NA_real_, wind_gust_ms),
    radiation_kj    = if_else(radiation_kj < 0,                     NA_real_, radiation_kj),
    pressure_mbar   = if_else(pressure_mbar < 850 | pressure_mbar > 1100, NA_real_, pressure_mbar),
    pressure_max_mbar = if_else(pressure_max_mbar < 850 | pressure_max_mbar > 1100, NA_real_, pressure_max_mbar),
    pressure_min_mbar = if_else(pressure_min_mbar < 850 | pressure_min_mbar > 1100, NA_real_, pressure_min_mbar)
  )
}

# ---------------------------------------------------------------------------
# Aggregate hourly to daily
# ---------------------------------------------------------------------------
aggregate_to_daily <- function(hourly_df) {
  hourly_df %>%
    group_by(date) %>%
    summarise(
      tmax_daily      = max(tmax_c,         na.rm = TRUE),
      tmin_daily      = min(tmin_c,         na.rm = TRUE),
      tmean_daily     = mean(temp_drybulb_c, na.rm = TRUE),

      dewpoint_daily  = mean(dewpoint_c,    na.rm = TRUE),
      dewpoint_max_daily = max(dewpoint_max_c, na.rm = TRUE),
      dewpoint_min_daily = min(dewpoint_min_c, na.rm = TRUE),

      humidity_mean   = mean(rh_pct,        na.rm = TRUE),
      humidity_min    = min(rh_min_pct,         na.rm = TRUE),
      humidity_max    = max(rh_max_pct,         na.rm = TRUE),
      
      pressure_mean   = mean(pressure_mbar, na.rm = TRUE),
      pressure_max    = max(pressure_max_mbar, na.rm = TRUE),
      pressure_min    = min(pressure_min_mbar, na.rm = TRUE),
      
      radiation_total = sum(radiation_kj,   na.rm = TRUE),
      radiation_mean  = mean(radiation_kj,  na.rm = TRUE),
      radiation_max   = max(radiation_kj,   na.rm = TRUE),
      radiation_min   = min(radiation_kj,   na.rm = TRUE),
      
      wind_mean       = mean(wind_speed_ms, na.rm = TRUE),
      wind_max        = max(wind_speed_ms,  na.rm = TRUE), 
      wind_min        = min(wind_speed_ms,  na.rm = TRUE),

      wind_gust_max   = max(wind_gust_ms,   na.rm = TRUE),
      wind_gust_mean  = mean(wind_gust_ms,  na.rm = TRUE),
      wind_gust_min   = min(wind_gust_ms,   na.rm = TRUE),
      
      precip_total    = sum(precip_mm,      na.rm = TRUE),

      n_obs           = n(),
      n_tmax_valid    = sum(!is.na(tmax_c)),
      .groups         = "drop"
    ) %>%
    # Replace Inf/-Inf from all-NA groups with NA
    mutate(across(where(is.numeric), ~ if_else(is.infinite(.x), NA_real_, .x)))
}

# ---------------------------------------------------------------------------
# Ensure complete date sequence and interpolate short gaps
# ---------------------------------------------------------------------------
fill_and_interpolate <- function(daily_df) {
  full_dates <- tibble(date = seq(min(daily_df$date), max(daily_df$date), by = "day"))

  df <- full_dates %>%
    left_join(daily_df, by = "date") %>%
    arrange(date)

  # Interpolate numeric columns for gaps <= 3 days, flag longer gaps
  numeric_cols <- names(df)[sapply(df, is.numeric) & names(df) != "long_gap"]

  # Build a logical vector of rows that were missing (NA date originally → missing n_obs)
  df <- df %>% mutate(was_missing = is.na(n_obs))

  # Fill n_obs=0 for newly inserted rows before flagging
  df <- df %>% mutate(n_obs = if_else(is.na(n_obs), 0L, as.integer(n_obs)))

  # Identify long gap starts: runs of missing days > 3
  missing_rle <- rle(df$was_missing)
  long_gap_rows <- logical(nrow(df))
  pos <- 1
  for (i in seq_along(missing_rle$lengths)) {
    len <- missing_rle$lengths[i]
    val <- missing_rle$values[i]
    if (val && len > 3) {
      long_gap_rows[pos:(pos + len - 1)] <- TRUE
    }
    pos <- pos + len
  }
  df <- df %>% mutate(long_gap = long_gap_rows)

  # Linear interpolation for gaps <= 3 consecutive missing days
  interp_cols <- setdiff(numeric_cols, c("n_obs", "n_tmax_valid"))
  for (col in interp_cols) {
    vals <- df[[col]]
    # Only interpolate where was_missing & !long_gap
    na_idx <- which(df$was_missing & !df$long_gap)
    if (length(na_idx) == 0) next
    # Use approx for linear interpolation across entire series
    valid_idx <- which(!is.na(vals))
    if (length(valid_idx) < 2) next
    interp_vals <- approx(valid_idx, vals[valid_idx], xout = seq_along(vals), rule = 1)$y
    # Only fill in the target positions
    vals[na_idx] <- interp_vals[na_idx]
    df[[col]] <- vals
  }

  df %>% select(-was_missing)
}

# ---------------------------------------------------------------------------
# Full pipeline for one city
# ---------------------------------------------------------------------------
process_city <- function(dir_path, city_name) {
  message(sprintf("\n=== Processing %s ===", city_name))

  hourly <- read_city_hourly(dir_path)
  message(sprintf("  Hourly rows read: %d  (dates: %s to %s)",
                  nrow(hourly), min(hourly$date), max(hourly$date)))

  hourly_qc <- apply_hourly_qc(hourly)

  daily <- aggregate_to_daily(hourly_qc)
  message(sprintf("  Daily rows (before gap fill): %d", nrow(daily)))

  daily_full <- fill_and_interpolate(daily)
  message(sprintf("  Daily rows (after gap fill): %d", nrow(daily_full)))

  # Compute apparent temperature using Tmax
  daily_full <- daily_full %>%
    mutate(
      apparent_temp = compute_apparent_temp(tmax_daily, humidity_mean, wind_mean)
    )

  daily_full
}

# ---------------------------------------------------------------------------
# Run both cities
# ---------------------------------------------------------------------------
daily_poa <- process_city("data/raw/porto_alegre", "Porto Alegre")
daily_sp  <- process_city("data/raw/sao_paulo",    "Sao Paulo")

# ---------------------------------------------------------------------------
# Save outputs
# ---------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(daily_poa, "data/processed/daily_climate_poa.rds")
saveRDS(daily_sp,  "data/processed/daily_climate_sp.rds")

message("\n=== Saved outputs ===")
message("  data/processed/daily_climate_poa.rds")
message("  data/processed/daily_climate_sp.rds")

# ---------------------------------------------------------------------------
# Verification summary
# ---------------------------------------------------------------------------
summarise_output <- function(df, label) {
  message(sprintf("\n--- %s ---", label))
  message(sprintf("  Rows: %d  |  Date range: %s to %s",
                  nrow(df), min(df$date), max(df$date)))
  message(sprintf("  Columns: %s", paste(names(df), collapse = ", ")))
  message(sprintf("  Tmax range: %.1f to %.1f  (NA: %d)",
                  min(df$tmax_daily, na.rm = TRUE),
                  max(df$tmax_daily, na.rm = TRUE),
                  sum(is.na(df$tmax_daily))))
  message(sprintf("  Tmin range: %.1f to %.1f  (NA: %d)",
                  min(df$tmin_daily, na.rm = TRUE),
                  max(df$tmin_daily, na.rm = TRUE),
                  sum(is.na(df$tmin_daily))))
  message(sprintf("  Long-gap days: %d",    sum(df$long_gap, na.rm = TRUE)))
  message(sprintf("  n_obs=0 days: %d",     sum(df$n_obs == 0, na.rm = TRUE)))
  message(sprintf("  Apparent temp (mean): %.1f  (NA: %d)",
                  mean(df$apparent_temp, na.rm = TRUE),
                  sum(is.na(df$apparent_temp))))
}

message("\n=== Verification ===")
summarise_output(daily_poa, "Porto Alegre")
summarise_output(daily_sp,  "Sao Paulo")

message("\nDone.")
