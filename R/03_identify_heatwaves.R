# R/03_identify_heatwaves.R
# Identify heat wave events across 18 definition variants for Porto Alegre and São Paulo
# Input:  data/processed/daily_climate_poa.rds, data/processed/daily_climate_sp.rds
# Output: data/processed/heatwaves_poa.rds, data/processed/heatwaves_sp.rds

source("R/utils.R")

# ── Parameters ───────────────────────────────────────────────────────────────

PERCENTILES  <- c(0.90, 0.925, 0.95)
DURATIONS    <- c(2L, 3L, 5L)
METRICS      <- list(
  tmax     = "tmax_daily",
  apparent = "apparent_temp"
)

# ── Helper: label from parameters ────────────────────────────────────────────

hw_def_label <- function(metric, percentile, duration) {
  pct_str <- gsub("\\.", "", as.character(percentile * 100))  # "90", "925", "95"
  sprintf("%s_p%s_d%d", metric, pct_str, duration)
}

# ── Core: process one city ────────────────────────────────────────────────────

process_city <- function(daily_data, city_label) {
  message(sprintf("\n=== Processing %s ===", city_label))

  # Add city label for output
  daily_data <- daily_data %>%
    mutate(
      doy  = yday(date),
      city = city_label
    )

  # Pre-compute all 6 threshold sets (3 percentiles × 2 metrics) once
  message("  Computing thresholds (6 sets) ...")
  threshold_cache <- list()
  for (metric_name in names(METRICS)) {
    col_name <- METRICS[[metric_name]]
    for (pct in PERCENTILES) {
      cache_key <- sprintf("%s_%.3f", metric_name, pct)
      message(sprintf("    %s p%.1f%%", metric_name, pct * 100))
      thr <- compute_seasonal_threshold(daily_data, col_name, pct)
      threshold_cache[[cache_key]] <- thr
    }
  }

  # Iterate over 18 variants
  all_events <- list()
  for (metric_name in names(METRICS)) {
    col_name <- METRICS[[metric_name]]
    for (pct in PERCENTILES) {
      cache_key  <- sprintf("%s_%.3f", metric_name, pct)
      thr_lookup <- threshold_cache[[cache_key]]

      # Join threshold to daily data by DOY
      data_with_thr <- daily_data %>%
        left_join(thr_lookup, by = "doy") %>%
        mutate(above = .data[[col_name]] > threshold & !is.na(.data[[col_name]]))

      for (dur in DURATIONS) {
        def_label <- hw_def_label(metric_name, pct, dur)
        message(sprintf("    Detecting %s ...", def_label))

        events <- identify_hw_events(
          dates           = data_with_thr$date,
          above_threshold = data_with_thr$above,
          min_duration    = dur
        )

        if (nrow(events) == 0) {
          all_events[[def_label]] <- tibble(
            start         = as.Date(character(0)),
            end           = as.Date(character(0)),
            duration      = integer(0),
            hw_def        = character(0),
            peak_temp     = numeric(0),
            peak_apparent = numeric(0),
            mean_excess   = numeric(0),
            degree_days   = numeric(0),
            mean_humidity = numeric(0),
            mean_precip   = numeric(0),
            city          = character(0)
          )
          next
        }

        # Enrich each event
        enriched <- events %>%
          mutate(
            hw_def        = def_label,
            peak_temp     = NA_real_,
            peak_apparent = NA_real_,
            mean_excess   = NA_real_,
            degree_days   = NA_real_,
            mean_humidity = NA_real_,
            mean_precip   = NA_real_,
            city          = city_label
          )

        for (i in seq_len(nrow(enriched))) {
          ev_start <- enriched$start[i]
          ev_end   <- enriched$end[i]

          hw_days <- data_with_thr %>%
            filter(date >= ev_start, date <= ev_end)

          enriched$peak_temp[i]     <- max(hw_days$tmax_daily,   na.rm = TRUE)
          enriched$peak_apparent[i] <- max(hw_days$apparent_temp, na.rm = TRUE)
          enriched$mean_humidity[i] <- mean(hw_days$humidity_mean, na.rm = TRUE)
          enriched$mean_precip[i]   <- mean(hw_days$precip_total,  na.rm = TRUE)

          excess <- hw_days[[col_name]] - hw_days$threshold
          enriched$mean_excess[i]  <- mean(excess, na.rm = TRUE)
          enriched$degree_days[i]  <- sum(excess,  na.rm = TRUE)
        }

        all_events[[def_label]] <- enriched
      }
    }
  }

  bind_rows(all_events) %>%
    select(start, end, duration, hw_def, peak_temp, peak_apparent,
           mean_excess, degree_days, mean_humidity, mean_precip, city)
}

# ── Run for both cities ───────────────────────────────────────────────────────

daily_poa <- readRDS("data/processed/daily_climate_poa.rds")
daily_sp  <- readRDS("data/processed/daily_climate_sp.rds")

heatwaves_poa <- process_city(daily_poa, "Porto Alegre")
heatwaves_sp  <- process_city(daily_sp,  "São Paulo")

# ── Save outputs ─────────────────────────────────────────────────────────────

saveRDS(heatwaves_poa, "data/processed/heatwaves_poa.rds")
saveRDS(heatwaves_sp,  "data/processed/heatwaves_sp.rds")

message("\nSaved: data/processed/heatwaves_poa.rds")
message("Saved: data/processed/heatwaves_sp.rds")

# ── Verification ─────────────────────────────────────────────────────────────

message("\n=== Event counts per definition ===")

for (city_label in c("Porto Alegre", "São Paulo")) {
  hw <- if (city_label == "Porto Alegre") heatwaves_poa else heatwaves_sp
  cat(sprintf("\n%s:\n", city_label))
  counts <- hw %>%
    count(hw_def) %>%
    arrange(hw_def)
  print(counts, n = Inf)
}

message("\n=== Sanity check: relaxed (p90_d2) vs strict (p95_d5) ===")
for (city_label in c("Porto Alegre", "São Paulo")) {
  hw <- if (city_label == "Porto Alegre") heatwaves_poa else heatwaves_sp
  n_relaxed <- nrow(filter(hw, hw_def == "tmax_p90_d2"))
  n_strict  <- nrow(filter(hw, hw_def == "tmax_p95_d5"))
  ok <- if (n_relaxed >= n_strict) "OK" else "WARNING: relaxed < strict!"
  cat(sprintf("%s — tmax_p90_d2: %d events, tmax_p95_d5: %d events  [%s]\n",
              city_label, n_relaxed, n_strict, ok))
}

message("\nDone.")
