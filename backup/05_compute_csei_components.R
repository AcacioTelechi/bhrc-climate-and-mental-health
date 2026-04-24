# R/05_compute_csei_components.R
# Compute CSEI component metrics per participant × developmental window × HW definition
# Input:  data/processed/daily_climate_poa.rds, daily_climate_sp.rds
#         data/processed/heatwaves_poa.rds, heatwaves_sp.rds
#         data/processed/participant_birthdates.rds
#         data/Santoro_climate_BHRC_2025_12_19.rds
# Output: data/processed/csei_components.rds

source("R/utils.R")

# ── Load inputs ───────────────────────────────────────────────────────────────

message("Loading data...")

daily_poa  <- readRDS("data/processed/daily_climate_poa.rds") %>%
  mutate(city = "porto_alegre", doy = yday(date))
daily_sp   <- readRDS("data/processed/daily_climate_sp.rds") %>%
  mutate(city = "sao_paulo", doy = yday(date))

hw_poa <- readRDS("data/processed/heatwaves_poa.rds") %>%
  mutate(city = "porto_alegre")
hw_sp  <- readRDS("data/processed/heatwaves_sp.rds") %>%
  mutate(city = "sao_paulo")

participants <- readRDS("data/processed/participant_birthdates.rds")

santoro <- readRDS("data/Santoro_climate_BHRC_2025_12_19.rds")

# ── Assessment dates: latest d_date per participant ───────────────────────────

latest_assessment <- santoro %>%
  filter(!is.na(d_date)) %>%
  group_by(ext_genid) %>%
  summarise(latest_d_date = max(d_date), .groups = "drop")

# Join to participants
participants <- participants %>%
  left_join(latest_assessment, by = "ext_genid")

message(sprintf("Participants: %d", nrow(participants)))
message(sprintf("  With latest_d_date: %d", sum(!is.na(participants$latest_d_date))))

# ── HW definitions ────────────────────────────────────────────────────────────

hw_defs <- unique(c(hw_poa$hw_def, hw_sp$hw_def))
message(sprintf("HW definitions: %d", length(hw_defs)))

# ── Pre-compute DOY thresholds per city (for days_above_p90) ─────────────────

message("Pre-computing DOY thresholds for tmax p90...")

thr_poa <- compute_seasonal_threshold(daily_poa, "tmax_daily", 0.90)
thr_sp  <- compute_seasonal_threshold(daily_sp,  "tmax_daily", 0.90)

# Attach thresholds to daily data
daily_poa <- daily_poa %>% left_join(thr_poa, by = "doy") %>% rename(thr_p90 = threshold)
daily_sp  <- daily_sp  %>% left_join(thr_sp,  by = "doy") %>% rename(thr_p90 = threshold)

# Combined daily climate (by city)
daily_all <- bind_rows(daily_poa, daily_sp)

# Combined HW events (by city)
hw_all <- bind_rows(hw_poa, hw_sp)

# ── Climatological mean precip by DOY per city (for precip_deficit) ───────────

clim_precip_poa <- daily_poa %>%
  group_by(doy) %>%
  summarise(clim_precip = mean(precip_total, na.rm = TRUE), .groups = "drop")

clim_precip_sp <- daily_sp %>%
  group_by(doy) %>%
  summarise(clim_precip = mean(precip_total, na.rm = TRUE), .groups = "drop")

# ── Helper functions ──────────────────────────────────────────────────────────

# Determine city-specific data
get_city_data <- function(city_label) {
  if (city_label == "porto_alegre") {
    list(
      daily     = daily_poa,
      hw        = hw_poa,
      clim_prec = clim_precip_poa
    )
  } else {
    list(
      daily     = daily_sp,
      hw        = hw_sp,
      clim_prec = clim_precip_sp
    )
  }
}

# Compute general climate indicators for a window
compute_general_climate <- function(daily, win_start, win_end) {
  wd <- daily %>% filter(date >= win_start, date <= win_end)
  if (nrow(wd) == 0) {
    return(list(
      mean_tmax      = NA_real_,
      mean_tmean     = NA_real_,
      days_above_p90 = NA_integer_,
      mean_humidity  = NA_real_,
      total_precip   = NA_real_,
      tmax_sd        = NA_real_
    ))
  }
  list(
    mean_tmax      = mean(wd$tmax_daily,   na.rm = TRUE),
    mean_tmean     = mean(wd$tmean_daily,  na.rm = TRUE),
    days_above_p90 = as.integer(sum(wd$tmax_daily > wd$thr_p90, na.rm = TRUE)),
    mean_humidity  = mean(wd$humidity_mean, na.rm = TRUE),
    total_precip   = sum(wd$precip_total,  na.rm = TRUE),
    tmax_sd        = sd(wd$tmax_daily,     na.rm = TRUE)
  )
}

# Compute HW metrics for one window × one hw_def
compute_hw_metrics <- function(hw_events, daily, clim_prec, win_start, win_end, hw_def_name) {
  # Filter events overlapping window
  evs <- hw_events %>%
    filter(hw_def == hw_def_name,
           start <= win_end,
           end   >= win_start)

  if (nrow(evs) == 0) {
    return(list(
      hw_count          = 0L,
      hw_mean_excess    = NA_real_,
      hw_max_excess     = NA_real_,
      hw_total_days     = 0L,
      hw_mean_duration  = NA_real_,
      hw_degree_days    = NA_real_,
      hw_mean_humidity  = NA_real_,
      precip_deficit    = NA_real_,
      hw_apparent_excess = NA_real_
    ))
  }

  # Clip events to window boundaries
  evs <- evs %>%
    mutate(
      clipped_start    = pmax(start, win_start),
      clipped_end      = pmin(end,   win_end),
      clipped_duration = as.integer(clipped_end - clipped_start) + 1L,
      duration_ratio   = clipped_duration / duration
    )

  hw_count         <- nrow(evs)
  hw_total_days    <- sum(evs$clipped_duration)
  hw_mean_duration <- mean(evs$clipped_duration)
  hw_mean_excess   <- mean(evs$mean_excess,   na.rm = TRUE)
  hw_max_excess    <- max(evs$mean_excess,    na.rm = TRUE)
  hw_degree_days   <- sum(evs$degree_days * evs$duration_ratio, na.rm = TRUE)
  hw_mean_humidity <- mean(evs$mean_humidity, na.rm = TRUE)

  # precip_deficit: mean daily precip during HW days minus climatological mean for same DOYs
  # Collect all HW days in the window
  hw_day_dates <- unlist(lapply(seq_len(nrow(evs)), function(i) {
    seq(evs$clipped_start[i], evs$clipped_end[i], by = "day")
  }))
  hw_day_dates <- as.Date(hw_day_dates, origin = "1970-01-01")

  if (length(hw_day_dates) > 0) {
    hw_daily <- daily %>%
      filter(date %in% hw_day_dates) %>%
      mutate(doy = yday(date))

    if (nrow(hw_daily) > 0) {
      mean_hw_precip  <- mean(hw_daily$precip_total, na.rm = TRUE)
      hw_doys         <- unique(hw_daily$doy)
      clim_mean_precip <- clim_prec %>%
        filter(doy %in% hw_doys) %>%
        summarise(m = mean(clim_precip, na.rm = TRUE)) %>%
        pull(m)
      precip_deficit <- mean_hw_precip - clim_mean_precip
    } else {
      precip_deficit <- NA_real_
    }
  } else {
    precip_deficit <- NA_real_
  }

  # hw_apparent_excess: approximate using degree_days * duration_ratio for apparent defs,
  # NA for tmax defs
  is_apparent_def <- grepl("^apparent_", hw_def_name)
  if (is_apparent_def) {
    hw_apparent_excess <- sum(evs$degree_days * evs$duration_ratio, na.rm = TRUE)
  } else {
    hw_apparent_excess <- NA_real_
  }

  list(
    hw_count           = as.integer(hw_count),
    hw_mean_excess     = hw_mean_excess,
    hw_max_excess      = hw_max_excess,
    hw_total_days      = as.integer(hw_total_days),
    hw_mean_duration   = hw_mean_duration,
    hw_degree_days     = hw_degree_days,
    hw_mean_humidity   = hw_mean_humidity,
    precip_deficit     = precip_deficit,
    hw_apparent_excess = hw_apparent_excess
  )
}

# ── Main loop ─────────────────────────────────────────────────────────────────

message("Starting main computation loop...")
t_start <- proc.time()

all_rows <- vector("list", nrow(participants))

for (p_idx in seq_len(nrow(participants))) {

  if (p_idx %% 100 == 0) {
    elapsed <- (proc.time() - t_start)["elapsed"]
    message(sprintf("  Participant %d / %d  (%.1f s elapsed)", p_idx, nrow(participants), elapsed))
  }

  pid      <- participants$ext_genid[p_idx]
  bdate    <- participants$birth_date[p_idx]
  city     <- participants$city[p_idx]
  lat_date <- participants$latest_d_date[p_idx]

  if (is.na(bdate)) {
    next
  }

  city_data <- get_city_data(city)
  daily_c   <- city_data$daily
  hw_c      <- city_data$hw
  clim_prec <- city_data$clim_prec

  # Climate data date range for coverage
  climate_dates <- daily_c$date

  # Define developmental windows
  windows <- list(
    list(name = "0_5",       start = bdate,                   end = bdate + years(5) + days(-1)),
    list(name = "6_10",      start = bdate + years(6),        end = bdate + years(10) + days(-1)),
    list(name = "11_18",     start = bdate + years(11),       end = bdate + years(18) + days(-1)),
    list(name = "cumulative", start = bdate,                  end = if (!is.na(lat_date)) lat_date else as.Date(NA))
  )

  p_rows <- vector("list", length(windows) * length(hw_defs))
  row_idx <- 1L

  for (win in windows) {
    win_name  <- win$name
    win_start <- win$start
    win_end   <- win$end

    # Skip if window end is NA (cumulative with no assessment date)
    if (is.na(win_end)) {
      # Still produce rows with NA
      coverage <- 0
    } else {
      coverage <- compute_coverage(win_start, win_end, climate_dates)
    }

    # General climate indicators (same for all HW defs within this window)
    if (!is.na(win_end)) {
      gen_clim <- compute_general_climate(daily_c, win_start, win_end)
    } else {
      gen_clim <- list(
        mean_tmax      = NA_real_,
        mean_tmean     = NA_real_,
        days_above_p90 = NA_integer_,
        mean_humidity  = NA_real_,
        total_precip   = NA_real_,
        tmax_sd        = NA_real_
      )
    }

    for (hd in hw_defs) {
      if (!is.na(win_end)) {
        hw_metrics <- compute_hw_metrics(hw_c, daily_c, clim_prec, win_start, win_end, hd)
      } else {
        hw_metrics <- list(
          hw_count           = NA_integer_,
          hw_mean_excess     = NA_real_,
          hw_max_excess      = NA_real_,
          hw_total_days      = NA_integer_,
          hw_mean_duration   = NA_real_,
          hw_degree_days     = NA_real_,
          hw_mean_humidity   = NA_real_,
          precip_deficit     = NA_real_,
          hw_apparent_excess = NA_real_
        )
      }

      p_rows[[row_idx]] <- list(
        ext_genid          = pid,
        city               = city,
        window             = win_name,
        window_start       = win_start,
        window_end         = win_end,
        coverage_frac      = coverage,
        hw_def             = hd,
        hw_count           = hw_metrics$hw_count,
        hw_mean_excess     = hw_metrics$hw_mean_excess,
        hw_max_excess      = hw_metrics$hw_max_excess,
        hw_total_days      = hw_metrics$hw_total_days,
        hw_mean_duration   = hw_metrics$hw_mean_duration,
        hw_degree_days     = hw_metrics$hw_degree_days,
        hw_mean_humidity   = hw_metrics$hw_mean_humidity,
        precip_deficit     = hw_metrics$precip_deficit,
        hw_apparent_excess = hw_metrics$hw_apparent_excess,
        mean_tmax          = gen_clim$mean_tmax,
        mean_tmean         = gen_clim$mean_tmean,
        days_above_p90     = gen_clim$days_above_p90,
        mean_humidity      = gen_clim$mean_humidity,
        total_precip       = gen_clim$total_precip,
        tmax_sd            = gen_clim$tmax_sd
      )
      row_idx <- row_idx + 1L
    }
  }

  all_rows[[p_idx]] <- bind_rows(p_rows)
}

message("Binding all rows...")
csei_components <- bind_rows(all_rows)

# Ensure proper column types
csei_components <- csei_components %>%
  mutate(
    window_start  = as.Date(window_start, origin = "1970-01-01"),
    window_end    = as.Date(window_end,   origin = "1970-01-01"),
    hw_count      = as.integer(hw_count),
    hw_total_days = as.integer(hw_total_days),
    days_above_p90 = as.integer(days_above_p90)
  )

# ── Save output ───────────────────────────────────────────────────────────────

saveRDS(csei_components, "data/processed/csei_components.rds")
message("Saved: data/processed/csei_components.rds")

# ── Verification ──────────────────────────────────────────────────────────────

message("\n=== Verification ===")
message(sprintf("Total rows:           %d", nrow(csei_components)))
message(sprintf("Unique participants:  %d", length(unique(csei_components$ext_genid))))
message(sprintf("HW definitions:       %d", length(unique(csei_components$hw_def))))
message(sprintf("Windows:              %s", paste(unique(csei_components$window), collapse = ", ")))

message("\nCoverage stats by window:")
cov_stats <- csei_components %>%
  distinct(ext_genid, city, window, coverage_frac) %>%
  group_by(window) %>%
  summarise(
    n_participants = n(),
    mean_coverage  = round(mean(coverage_frac, na.rm = TRUE), 3),
    pct_zero_cov   = round(mean(coverage_frac == 0, na.rm = TRUE) * 100, 1),
    pct_full_cov   = round(mean(coverage_frac >= 0.95, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(cov_stats, n = Inf)

message("\nHW count distribution by window (tmax_p90_d2):")
hw_dist <- csei_components %>%
  filter(hw_def == "tmax_p90_d2") %>%
  group_by(window) %>%
  summarise(
    mean_hw_count  = round(mean(hw_count, na.rm = TRUE), 2),
    median_hw_count = median(hw_count, na.rm = TRUE),
    max_hw_count    = max(hw_count, na.rm = TRUE),
    .groups = "drop"
  )
print(hw_dist, n = Inf)

elapsed_total <- (proc.time() - t_start)["elapsed"]
message(sprintf("\nTotal runtime: %.1f seconds (%.1f minutes)", elapsed_total, elapsed_total / 60))
message("\nDone.")
