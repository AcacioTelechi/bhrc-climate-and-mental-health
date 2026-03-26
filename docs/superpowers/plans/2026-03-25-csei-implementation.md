# CSEI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Climate Stress Exposure Index (CSEI) linking INMET weather station data to BHRC cohort participants across developmental windows.

**Architecture:** Sequential R pipeline (01–07 scripts) where each script reads the previous step's output. Utility functions in `R/utils.R`. Climate data extracted from nested zips, aggregated hourly→daily, used to identify heat waves across 18 definitions, then linked to participants via derived birth dates and developmental windows. CSEI built via PCA (primary) and z-score mean (sensitivity).

**Tech Stack:** R with tidyverse, readxl, lubridate, janitor, psych, weathermetrics

---

## File Map

| File | Responsibility |
|------|---------------|
| `R/utils.R` | Shared helpers: comma→numeric parser, apparent temp, HW detection, coverage calc |
| `R/01_extract_inmet.R` | Extract A801/A701 CSVs from nested zips into `data/raw/` |
| `R/02_clean_daily_climate.R` | Parse hourly CSVs → daily aggregates, QC, gap-fill, save to `data/processed/` |
| `R/03_identify_heatwaves.R` | Compute percentile thresholds, detect HW events across 18 definitions |
| `R/04_derive_birth_dates.R` | Derive birth dates from age + d_date, cross-validate across waves |
| `R/05_compute_csei_components.R` | Per-participant × window × HW-def: compute all HW and general climate metrics |
| `R/06_build_csei.R` | PCA and z-score index construction, scree plots, loadings |
| `R/07_merge_analytical.R` | Merge CSEI into cohort data, produce final analytical dataset |
| `R/run_all.R` | Master script sourcing 01–07 in order |

---

### Task 0: Environment Setup

**Files:**
- Create: `R/utils.R`
- Create: `.Rprofile` (optional, for library paths)

- [ ] **Step 1: Install R and required packages**

R must be installed on the system. Run:

```bash
sudo apt-get update && sudo apt-get install -y r-base r-base-dev libcurl4-openssl-dev libssl-dev libxml2-dev
```

Then install R packages:

```bash
Rscript -e 'install.packages(c("tidyverse", "readxl", "lubridate", "janitor", "psych", "weathermetrics", "zip"), repos="https://cloud.r-project.org")'
```

Expected: packages install without errors.

- [ ] **Step 2: Create directory structure**

```bash
mkdir -p data/raw/porto_alegre data/raw/sao_paulo data/processed data/analytical output/figures output/tables R
```

- [ ] **Step 3: Update .gitignore to exclude raw data and processed data**

Add to `.gitignore`:

```
data/raw/
data/processed/
data/analytical/
output/
*.zip
*.rds
*.xlsx
```

- [ ] **Step 4: Create R/utils.R with shared helpers**

```r
# R/utils.R
# Shared utility functions for the CSEI pipeline

library(tidyverse)
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
  # Water vapor pressure (hPa)
  e <- (rh_pct / 100) * 6.105 * exp((17.27 * temp_c) / (237.7 + temp_c))
  # Apparent temperature
  at <- -2.7 + 1.04 * temp_c + 2.0 * e - 0.65 * wind_ms
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

#' Compute rolling calendar-day percentile threshold
#' For each day-of-year, pools observations within +/- half_window days across all years
compute_seasonal_threshold <- function(daily_data, temp_col, percentile, half_window = 7) {
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
```

- [ ] **Step 5: Verify R loads utils.R without errors**

```bash
Rscript -e 'source("R/utils.R"); cat("utils.R loaded successfully\n")'
```

Expected: "utils.R loaded successfully"

- [ ] **Step 6: Commit**

```bash
git add R/utils.R .gitignore
git commit -m "feat: add project structure and shared R utilities"
```

---

### Task 1: Extract INMET Station Data

**Files:**
- Create: `R/01_extract_inmet.R`

- [ ] **Step 1: Write R/01_extract_inmet.R**

This script extracts station A801 (Porto Alegre) and A701 (São Paulo) CSV files from the nested zip structure.

```r
# R/01_extract_inmet.R
# Extract INMET station data for Porto Alegre (A801) and São Paulo (A701)
# from the nested zip archive.
#
# Input:  data/Base de dados Clima-20260325T214746Z-1-001.zip
# Output: data/raw/porto_alegre/*.CSV and data/raw/sao_paulo/*.CSV

source("R/utils.R")
library(zip)

main_zip <- "data/Base de dados Clima-20260325T214746Z-1-001.zip"
temp_dir <- tempdir()

# Station patterns
stations <- list(
  porto_alegre = "A801",
  sao_paulo    = "A701"
)

# List contents of main zip
main_contents <- zip_list(main_zip)

# --- Extract yearly zips (2010-2025) ---
yearly_zips <- main_contents %>%
  filter(str_detect(filename, "INMET/\\d{4}\\.zip$"))

cat("Found", nrow(yearly_zips), "yearly zip files\n")

for (i in seq_len(nrow(yearly_zips))) {
  yearly_file <- yearly_zips$filename[i]
  year <- str_extract(yearly_file, "\\d{4}")
  cat("Processing year:", year, "\n")

  # Extract yearly zip to temp
  zip::unzip(main_zip, files = yearly_file, exdir = temp_dir, overwrite = TRUE)
  yearly_zip_path <- file.path(temp_dir, yearly_file)

  # List files in yearly zip
  yearly_contents <- zip_list(yearly_zip_path)

  for (station_name in names(stations)) {
    station_code <- stations[[station_name]]
    # Find the matching CSV
    pattern <- paste0(station_code, ".*\\.CSV$")
    station_files <- yearly_contents %>%
      filter(str_detect(filename, regex(pattern, ignore_case = TRUE)))

    if (nrow(station_files) == 0) {
      cat("  WARNING: No file found for", station_name, "in", year, "\n")
      next
    }

    # Extract to data/raw/{city}/
    out_dir <- file.path("data/raw", station_name)
    zip::unzip(yearly_zip_path, files = station_files$filename[1],
               exdir = temp_dir, overwrite = TRUE)

    # Copy to output directory with standardized name
    src <- file.path(temp_dir, station_files$filename[1])
    dst <- file.path(out_dir, paste0("INMET_", station_code, "_", year, ".CSV"))
    file.copy(src, dst, overwrite = TRUE)
    cat("  Extracted:", dst, "\n")
  }
}

# --- Extract standalone xlsx files (2021) ---
xlsx_files <- main_contents %>%
  filter(str_detect(filename, "INMET_.*\\.(xlsx|XLSX)$"))

cat("\nFound", nrow(xlsx_files), "standalone xlsx files\n")

for (i in seq_len(nrow(xlsx_files))) {
  xlsx_file <- xlsx_files$filename[i]

  for (station_name in names(stations)) {
    station_code <- stations[[station_name]]
    if (str_detect(xlsx_file, station_code)) {
      out_dir <- file.path("data/raw", station_name)
      zip::unzip(main_zip, files = xlsx_file, exdir = temp_dir, overwrite = TRUE)
      src <- file.path(temp_dir, xlsx_file)
      dst <- file.path(out_dir, paste0("INMET_", station_code, "_2021.xlsx"))
      file.copy(src, dst, overwrite = TRUE)
      cat("  Extracted xlsx:", dst, "\n")
    }
  }
}

cat("\nExtraction complete.\n")
cat("Porto Alegre files:", length(list.files("data/raw/porto_alegre")), "\n")
cat("São Paulo files:", length(list.files("data/raw/sao_paulo")), "\n")
```

- [ ] **Step 2: Run the extraction**

```bash
Rscript R/01_extract_inmet.R
```

Expected: 16 files per city (2010–2025 CSVs + 2021 xlsx), printed to console. Verify:

```bash
ls data/raw/porto_alegre/ | wc -l
ls data/raw/sao_paulo/ | wc -l
```

Expected: 17 each (16 CSVs + 1 xlsx; 2021 has both formats).

- [ ] **Step 3: Commit**

```bash
git add R/01_extract_inmet.R
git commit -m "feat: add INMET station data extraction script"
```

---

### Task 2: Clean and Aggregate Daily Climate Data

**Files:**
- Create: `R/02_clean_daily_climate.R`

- [ ] **Step 1: Write R/02_clean_daily_climate.R**

```r
# R/02_clean_daily_climate.R
# Parse hourly INMET data, aggregate to daily, QC, compute apparent temperature.
#
# Input:  data/raw/{porto_alegre,sao_paulo}/*.CSV and *.xlsx
# Output: data/processed/daily_climate_poa.rds
#         data/processed/daily_climate_sp.rds

source("R/utils.R")
library(readxl)
library(janitor)

#' Read a single INMET CSV file (semicolon-separated, 8 header rows, comma decimals)
read_inmet_csv <- function(filepath) {
  # Read with 8 metadata rows as skip
  df <- read_delim(
    filepath,
    delim = ";",
    skip = 8,
    locale = locale(encoding = "latin1"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  # Remove trailing empty column (from trailing semicolon)
  df <- df %>% select(where(~ !all(is.na(.))))

  # Standardize column names
  names(df) <- c(
    "date", "hour_utc", "precip_mm", "pressure_mbar",
    "pressure_max_mbar", "pressure_min_mbar", "radiation_kj",
    "temp_drybulb_c", "dewpoint_c", "tmax_c", "tmin_c",
    "dewpoint_max_c", "dewpoint_min_c",
    "rh_max_pct", "rh_min_pct", "rh_pct",
    "wind_dir_deg", "wind_gust_ms", "wind_speed_ms"
  )

  df
}

#' Read a single INMET xlsx file (header at row 9)
read_inmet_xlsx <- function(filepath) {
  df <- read_excel(filepath, skip = 8, col_types = "text")
  df <- df %>% select(where(~ !all(is.na(.))))

  names(df) <- c(
    "date", "hour_utc", "precip_mm", "pressure_mbar",
    "pressure_max_mbar", "pressure_min_mbar", "radiation_kj",
    "temp_drybulb_c", "dewpoint_c", "tmax_c", "tmin_c",
    "dewpoint_max_c", "dewpoint_min_c",
    "rh_max_pct", "rh_min_pct", "rh_pct",
    "wind_dir_deg", "wind_gust_ms", "wind_speed_ms"
  )

  df
}

#' Process a city's raw files into a clean daily dataset
process_city <- function(city_dir, city_name) {
  files <- list.files(city_dir, full.names = TRUE)
  cat("Processing", city_name, "-", length(files), "files\n")

  # Read all files
  all_hourly <- map_dfr(files, function(f) {
    cat("  Reading:", basename(f), "\n")
    if (str_detect(f, "\\.xlsx$")) {
      read_inmet_xlsx(f)
    } else {
      read_inmet_csv(f)
    }
  })

  # Parse date
  all_hourly <- all_hourly %>%
    mutate(date = as.Date(str_replace_all(date, "/", "-")))

  # Parse all numeric columns (comma decimal, -9999 = NA)
  numeric_cols <- setdiff(names(all_hourly), c("date", "hour_utc"))
  all_hourly <- all_hourly %>%
    mutate(across(all_of(numeric_cols), parse_comma_numeric))

  # Physical range QC
  all_hourly <- all_hourly %>%
    mutate(
      tmax_c       = if_else(tmax_c > 50 | tmax_c < -10, NA_real_, tmax_c),
      tmin_c       = if_else(tmin_c > 50 | tmin_c < -10, NA_real_, tmin_c),
      temp_drybulb_c = if_else(temp_drybulb_c > 50 | temp_drybulb_c < -10, NA_real_, temp_drybulb_c),
      dewpoint_c   = if_else(dewpoint_c > 50 | dewpoint_c < -10, NA_real_, dewpoint_c),
      rh_pct       = if_else(rh_pct > 100 | rh_pct < 0, NA_real_, rh_pct),
      precip_mm    = if_else(precip_mm < 0, NA_real_, precip_mm),
      wind_speed_ms = if_else(wind_speed_ms < 0 | wind_speed_ms > 50, NA_real_, wind_speed_ms),
      radiation_kj = if_else(radiation_kj < 0, NA_real_, radiation_kj),
      pressure_mbar = if_else(pressure_mbar < 850 | pressure_mbar > 1100, NA_real_, pressure_mbar)
    )

  # Aggregate hourly -> daily
  daily <- all_hourly %>%
    group_by(date) %>%
    summarise(
      tmax_daily     = max(tmax_c, na.rm = TRUE),
      tmin_daily     = min(tmin_c, na.rm = TRUE),
      tmean_daily    = mean(temp_drybulb_c, na.rm = TRUE),
      dewpoint_daily = mean(dewpoint_c, na.rm = TRUE),
      humidity_mean  = mean(rh_pct, na.rm = TRUE),
      humidity_min   = min(rh_pct, na.rm = TRUE),
      pressure_mean  = mean(pressure_mbar, na.rm = TRUE),
      radiation_total = sum(radiation_kj, na.rm = TRUE),
      wind_mean      = mean(wind_speed_ms, na.rm = TRUE),
      precip_total   = sum(precip_mm, na.rm = TRUE),
      n_obs          = n(),
      n_tmax_valid   = sum(!is.na(tmax_c)),
      .groups = "drop"
    ) %>%
    # Replace Inf/-Inf from all-NA groups
    mutate(across(where(is.numeric), ~ if_else(is.infinite(.), NA_real_, .)))

  # Compute apparent temperature
  daily <- daily %>%
    mutate(
      apparent_temp = compute_apparent_temp(tmax_daily, humidity_mean, wind_mean)
    )

  # Ensure complete date sequence
  full_dates <- tibble(date = seq(min(daily$date, na.rm = TRUE),
                                   max(daily$date, na.rm = TRUE),
                                   by = "day"))
  daily <- full_dates %>% left_join(daily, by = "date")

  # Linear interpolation for gaps <= 3 days
  # Flag gaps > 3 days
  daily <- daily %>%
    mutate(
      is_gap = is.na(tmax_daily),
      gap_group = cumsum(!is_gap | lag(is_gap, default = FALSE) != is_gap)
    )

  # Compute gap lengths
  gap_lengths <- daily %>%
    filter(is_gap) %>%
    group_by(gap_group) %>%
    summarise(gap_len = n(), .groups = "drop")

  short_gap_groups <- gap_lengths %>% filter(gap_len <= 3) %>% pull(gap_group)

  # Interpolate short gaps for key variables
  interp_vars <- c("tmax_daily", "tmin_daily", "tmean_daily", "dewpoint_daily",
                    "humidity_mean", "humidity_min", "pressure_mean", "wind_mean",
                    "apparent_temp")

  for (v in interp_vars) {
    daily[[v]] <- approx(
      x = which(!is.na(daily[[v]])),
      y = daily[[v]][!is.na(daily[[v]])],
      xout = seq_len(nrow(daily)),
      rule = 1  # NA outside range
    )$y
  }

  # Mark long gaps
  daily <- daily %>%
    mutate(
      long_gap = is_gap & !(gap_group %in% short_gap_groups)
    ) %>%
    select(-is_gap, -gap_group)

  # Add city identifier
  daily <- daily %>% mutate(city = city_name)

  cat("  Date range:", as.character(min(daily$date)), "to", as.character(max(daily$date)), "\n")
  cat("  Total days:", nrow(daily), "\n")
  cat("  Missing Tmax after interpolation:", sum(is.na(daily$tmax_daily)), "\n")

  daily
}

# --- Process both cities ---
daily_poa <- process_city("data/raw/porto_alegre", "porto_alegre")
daily_sp  <- process_city("data/raw/sao_paulo", "sao_paulo")

# Save
saveRDS(daily_poa, "data/processed/daily_climate_poa.rds")
saveRDS(daily_sp,  "data/processed/daily_climate_sp.rds")

cat("\nDaily climate data saved.\n")
```

- [ ] **Step 2: Run the cleaning script**

```bash
Rscript R/02_clean_daily_climate.R
```

Expected: Two RDS files created, console shows date range 2010–2025, ~5844 days each, low missing count after interpolation.

- [ ] **Step 3: Quick sanity check**

```bash
Rscript -e '
  poa <- readRDS("data/processed/daily_climate_poa.rds")
  cat("POA rows:", nrow(poa), "\n")
  cat("POA Tmax range:", range(poa$tmax_daily, na.rm=TRUE), "\n")
  cat("POA apparent temp range:", range(poa$apparent_temp, na.rm=TRUE), "\n")
  sp <- readRDS("data/processed/daily_climate_sp.rds")
  cat("SP rows:", nrow(sp), "\n")
  cat("SP Tmax range:", range(sp$tmax_daily, na.rm=TRUE), "\n")
'
```

Expected: Tmax ranges roughly 5–42°C for POA, 8–38°C for SP. Row counts ~5800+.

- [ ] **Step 4: Commit**

```bash
git add R/02_clean_daily_climate.R
git commit -m "feat: add hourly-to-daily climate data cleaning pipeline"
```

---

### Task 3: Identify Heat Waves

**Files:**
- Create: `R/03_identify_heatwaves.R`

- [ ] **Step 1: Write R/03_identify_heatwaves.R**

```r
# R/03_identify_heatwaves.R
# Compute seasonal percentile thresholds and identify heat wave events
# across 18 definition variants (3 thresholds x 3 durations x 2 metrics).
#
# Input:  data/processed/daily_climate_poa.rds
#         data/processed/daily_climate_sp.rds
# Output: data/processed/heatwaves_poa.rds
#         data/processed/heatwaves_sp.rds

source("R/utils.R")

# --- Configuration ---
percentiles <- c(0.90, 0.925, 0.95)
min_durations <- c(2, 3, 5)
thermal_metrics <- c("tmax_daily", "apparent_temp")

# --- Function to process one city ---
process_heatwaves <- function(daily_data, city_name) {
  cat("Processing heat waves for:", city_name, "\n")

  # Compute thresholds for each metric x percentile combination
  thresholds_list <- list()

  for (metric in thermal_metrics) {
    for (pctile in percentiles) {
      label <- paste0(metric, "_p", pctile * 100)
      cat("  Computing threshold:", label, "\n")

      thresh <- compute_seasonal_threshold(daily_data, metric, pctile, half_window = 7)
      thresholds_list[[label]] <- thresh
    }
  }

  # Identify heat wave events across all 18 definitions
  all_events <- tibble()

  for (metric in thermal_metrics) {
    for (pctile in percentiles) {
      thresh_label <- paste0(metric, "_p", pctile * 100)
      thresh_df <- thresholds_list[[thresh_label]]

      # Merge daily threshold into daily data
      daily_with_thresh <- daily_data %>%
        mutate(doy = yday(date)) %>%
        left_join(thresh_df, by = "doy") %>%
        mutate(above = !!sym(metric) > threshold)

      for (min_dur in min_durations) {
        hw_def <- paste0(
          str_replace(metric, "_daily", ""),
          "_p", pctile * 100,
          "_d", min_dur
        )
        cat("  Definition:", hw_def, "\n")

        events <- identify_hw_events(
          dates = daily_with_thresh$date,
          above_threshold = daily_with_thresh$above,
          min_duration = min_dur
        )

        if (nrow(events) > 0) {
          # Enrich events with intensity metrics using a loop for clarity
          enriched <- map_dfr(seq_len(nrow(events)), function(j) {
            evt <- events[j, ]
            evt_data <- daily_with_thresh %>%
              filter(date >= evt$start & date <= evt$end)
            evt %>% mutate(
              peak_temp = max(evt_data[[metric]], na.rm = TRUE),
              peak_apparent = max(evt_data$apparent_temp, na.rm = TRUE),
              mean_excess = mean(evt_data[[metric]] - evt_data$threshold, na.rm = TRUE),
              degree_days = sum(evt_data[[metric]] - evt_data$threshold, na.rm = TRUE),
              mean_humidity = mean(evt_data$humidity_mean, na.rm = TRUE),
              mean_precip = mean(evt_data$precip_total, na.rm = TRUE)
            )
          })
          events <- enriched %>% mutate(hw_def = hw_def)
        } else {
          events <- events %>%
            mutate(hw_def = character(0), peak_temp = numeric(0),
                   peak_apparent = numeric(0), mean_excess = numeric(0),
                   degree_days = numeric(0), mean_humidity = numeric(0),
                   mean_precip = numeric(0))
        }

        all_events <- bind_rows(all_events, events)
      }
    }
  }

  cat("  Total events across all definitions:", nrow(all_events), "\n")
  all_events <- all_events %>% mutate(city = city_name)
  all_events
}

# --- Process both cities ---
daily_poa <- readRDS("data/processed/daily_climate_poa.rds")
daily_sp  <- readRDS("data/processed/daily_climate_sp.rds")

hw_poa <- process_heatwaves(daily_poa, "porto_alegre")
hw_sp  <- process_heatwaves(daily_sp, "sao_paulo")

# Save
saveRDS(hw_poa, "data/processed/heatwaves_poa.rds")
saveRDS(hw_sp,  "data/processed/heatwaves_sp.rds")

# Summary
cat("\nHeat wave summary:\n")
cat("Porto Alegre:\n")
print(hw_poa %>% count(hw_def) %>% arrange(desc(n)))
cat("\nSão Paulo:\n")
print(hw_sp %>% count(hw_def) %>% arrange(desc(n)))
```

- [ ] **Step 2: Run the heat wave identification**

```bash
Rscript R/03_identify_heatwaves.R
```

Expected: Console prints event counts per definition. More relaxed definitions (p90, d2) should have more events than strict (p95, d5).

- [ ] **Step 3: Sanity check**

```bash
Rscript -e '
  hw <- readRDS("data/processed/heatwaves_poa.rds")
  cat("Definitions:", length(unique(hw$hw_def)), "\n")
  cat("Events per definition:\n")
  print(table(hw$hw_def))
  cat("Mean duration:", mean(hw$duration), "\n")
  cat("Mean excess:", mean(hw$mean_excess, na.rm=TRUE), "\n")
'
```

Expected: 18 unique definitions. Mean duration > min_duration for each def.

- [ ] **Step 4: Commit**

```bash
git add R/03_identify_heatwaves.R
git commit -m "feat: add heat wave identification across 18 definitions"
```

---

### Task 4: Derive Participant Birth Dates

**Files:**
- Create: `R/04_derive_birth_dates.R`

- [ ] **Step 1: Write R/04_derive_birth_dates.R**

```r
# R/04_derive_birth_dates.R
# Derive birth dates from age + d_date assessment date.
# Cross-validate across waves.
#
# Input:  data/Santoro_climate_BHRC_2025_12_19.rds
# Output: data/processed/participant_birthdates.rds

source("R/utils.R")

cohort <- readRDS("data/Santoro_climate_BHRC_2025_12_19.rds")

cat("Cohort dimensions:", dim(cohort), "\n")
cat("Waves:", unique(cohort$redcap_event_name), "\n")

# --- Derive birth date per observation ---
# birth_date = d_date - (age_years * 365.25)
birth_estimates <- cohort %>%
  select(ext_genid, redcap_event_name, site, gender, d_date, age) %>%
  filter(!is.na(d_date) & !is.na(age)) %>%
  mutate(
    d_date = as.Date(d_date),
    birth_date_est = d_date - days(round(age * 365.25))
  )

cat("Observations with both d_date and age:", nrow(birth_estimates), "\n")

# --- Select best estimate per participant ---
# Prefer wave0, then earliest available wave
birth_per_subject <- birth_estimates %>%
  arrange(ext_genid, redcap_event_name) %>%
  group_by(ext_genid) %>%
  summarise(
    birth_date = first(birth_date_est),  # wave0 comes first alphabetically
    n_estimates = n(),
    birth_date_sd_days = if (n() > 1) sd(as.numeric(birth_date_est)) else 0,
    site = first(site),
    gender = first(gender),
    .groups = "drop"
  )

cat("Unique participants with birth date:", nrow(birth_per_subject), "\n")

# Cross-validation: flag participants with high variability across waves
# (SD > 30 days suggests inconsistent age/date recording)
inconsistent <- birth_per_subject %>%
  filter(birth_date_sd_days > 30)
cat("Participants with inconsistent birth date (SD > 30 days):", nrow(inconsistent), "\n")

if (nrow(inconsistent) > 0) {
  cat("  Max SD:", max(inconsistent$birth_date_sd_days), "days\n")
}

# --- Add birth year and city mapping ---
birth_per_subject <- birth_per_subject %>%
  mutate(
    birth_year = year(birth_date),
    city = case_when(
      site == 1 ~ "porto_alegre",
      site == 2 ~ "sao_paulo",
      TRUE ~ NA_character_
    )
  )

cat("Birth year range:", range(birth_per_subject$birth_year), "\n")
cat("Site distribution:\n")
print(table(birth_per_subject$city, useNA = "always"))

# Save
saveRDS(birth_per_subject, "data/processed/participant_birthdates.rds")
cat("\nParticipant birth dates saved.\n")
```

- [ ] **Step 2: Run the birth date derivation**

```bash
Rscript R/04_derive_birth_dates.R
```

Expected: ~2500 participants with birth dates. Birth years roughly 1997–2013. Few inconsistent cases.

- [ ] **Step 3: Commit**

```bash
git add R/04_derive_birth_dates.R
git commit -m "feat: derive participant birth dates from age and assessment date"
```

---

### Task 5: Compute CSEI Component Metrics

**Files:**
- Create: `R/05_compute_csei_components.R`

- [ ] **Step 1: Write R/05_compute_csei_components.R**

```r
# R/05_compute_csei_components.R
# Compute per-participant x window x HW-definition metrics.
# Also compute general climate stress indicators per window.
#
# Input:  data/processed/daily_climate_poa.rds
#         data/processed/daily_climate_sp.rds
#         data/processed/heatwaves_poa.rds
#         data/processed/heatwaves_sp.rds
#         data/processed/participant_birthdates.rds
#         data/Santoro_climate_BHRC_2025_12_19.rds (for assessment dates per wave)
# Output: data/processed/csei_components.rds

source("R/utils.R")

# --- Load data ---
daily_climate <- list(
  porto_alegre = readRDS("data/processed/daily_climate_poa.rds"),
  sao_paulo    = readRDS("data/processed/daily_climate_sp.rds")
)

heatwaves <- list(
  porto_alegre = readRDS("data/processed/heatwaves_poa.rds"),
  sao_paulo    = readRDS("data/processed/heatwaves_sp.rds")
)

participants <- readRDS("data/processed/participant_birthdates.rds")

# Load cohort for assessment dates per wave (for cumulative window)
cohort <- readRDS("data/Santoro_climate_BHRC_2025_12_19.rds")
assessment_dates <- cohort %>%
  select(ext_genid, redcap_event_name, d_date) %>%
  filter(!is.na(d_date)) %>%
  mutate(d_date = as.Date(d_date))

# --- Define developmental windows ---
define_windows <- function(birth_date, assessment_date = NULL) {
  windows <- tibble(
    window = c("0_5", "6_10", "11_18"),
    start = birth_date + years(c(0, 6, 11)),
    end   = birth_date + years(c(5, 10, 18))
  )
  # Add cumulative window if assessment date provided
  if (!is.null(assessment_date) && !is.na(assessment_date)) {
    windows <- bind_rows(windows, tibble(
      window = "cumulative",
      start = birth_date,
      end = assessment_date
    ))
  }
  windows
}

# --- Compute HW metrics for a participant x window x definition ---
compute_hw_metrics <- function(hw_events, daily_data, window_start, window_end, hw_def_name) {
  # Filter events overlapping with this window
  events_in_window <- hw_events %>%
    filter(hw_def == hw_def_name,
           start <= window_end,
           end >= window_start) %>%
    # Clip events to window boundaries
    mutate(
      start_clipped = pmax(start, window_start),
      end_clipped = pmin(end, window_end),
      duration_clipped = as.integer(end_clipped - start_clipped) + 1L
    )

  if (nrow(events_in_window) == 0) {
    return(tibble(
      hw_count = 0L, hw_mean_excess = NA_real_, hw_max_excess = NA_real_,
      hw_total_days = 0L, hw_mean_duration = NA_real_,
      hw_degree_days = 0, hw_mean_humidity = NA_real_,
      precip_deficit = NA_real_, hw_apparent_excess = NA_real_
    ))
  }

  # Get daily data during HW days in window
  hw_dates <- map2(events_in_window$start_clipped, events_in_window$end_clipped,
                    ~ seq(.x, .y, by = "day")) %>% unlist() %>% as.Date(origin = "1970-01-01")
  hw_daily <- daily_data %>% filter(date %in% hw_dates)

  # Climatological mean precip for this calendar period (for deficit calc)
  doy_range <- yday(hw_dates)
  clim_precip <- daily_data %>%
    filter(yday(date) %in% doy_range) %>%
    pull(precip_total) %>%
    mean(na.rm = TRUE)
  actual_precip <- mean(hw_daily$precip_total, na.rm = TRUE)

  # Determine which metric column to use (tmax or apparent)
  metric_col <- if (str_detect(hw_def_name, "^tmax")) "tmax_daily" else "apparent_temp"

  # Get the threshold for excess calculation
  # Reconstruct from daily data with seasonal threshold
  pctile_str <- str_extract(hw_def_name, "p[0-9.]+")
  pctile_val <- as.numeric(str_remove(pctile_str, "p")) / 100
  thresh_df <- compute_seasonal_threshold(daily_data, metric_col, pctile_val)

  hw_daily_with_thresh <- hw_daily %>%
    mutate(doy = yday(date)) %>%
    left_join(thresh_df, by = "doy") %>%
    mutate(excess = !!sym(metric_col) - threshold)

  tibble(
    hw_count = nrow(events_in_window),
    hw_mean_excess = mean(hw_daily_with_thresh$excess, na.rm = TRUE),
    hw_max_excess = max(hw_daily_with_thresh$excess, na.rm = TRUE),
    hw_total_days = sum(events_in_window$duration_clipped),
    hw_mean_duration = mean(events_in_window$duration_clipped),
    hw_degree_days = sum(hw_daily_with_thresh$excess, na.rm = TRUE),
    hw_mean_humidity = mean(hw_daily$humidity_mean, na.rm = TRUE),
    precip_deficit = actual_precip - clim_precip,
    hw_apparent_excess = sum(
      hw_daily$apparent_temp - (hw_daily %>% mutate(doy = yday(date)) %>%
        left_join(compute_seasonal_threshold(daily_data, "apparent_temp", pctile_val), by = "doy") %>%
        pull(threshold)),
      na.rm = TRUE
    )
  )
}

# --- Compute general climate indicators for a window ---
compute_general_climate <- function(daily_data, window_start, window_end) {
  window_data <- daily_data %>%
    filter(date >= window_start & date <= window_end)

  if (nrow(window_data) == 0) {
    return(tibble(
      mean_tmax = NA_real_, mean_tmean = NA_real_,
      days_above_p90 = NA_integer_,
      mean_humidity = NA_real_, total_precip = NA_real_, tmax_sd = NA_real_
    ))
  }

  # Use 90th percentile threshold for days_above count
  thresh_90 <- compute_seasonal_threshold(daily_data, "tmax_daily", 0.90)
  window_with_thresh <- window_data %>%
    mutate(doy = yday(date)) %>%
    left_join(thresh_90, by = "doy")

  tibble(
    mean_tmax = mean(window_data$tmax_daily, na.rm = TRUE),
    mean_tmean = mean(window_data$tmean_daily, na.rm = TRUE),
    days_above_p90 = sum(window_with_thresh$tmax_daily > window_with_thresh$threshold, na.rm = TRUE),
    mean_humidity = mean(window_data$humidity_mean, na.rm = TRUE),
    total_precip = sum(window_data$precip_total, na.rm = TRUE),
    tmax_sd = sd(window_data$tmax_daily, na.rm = TRUE)
  )
}

# --- Main computation loop ---
hw_defs <- unique(c(heatwaves$porto_alegre$hw_def, heatwaves$sao_paulo$hw_def))
cat("Heat wave definitions:", length(hw_defs), "\n")
cat("Participants:", nrow(participants), "\n")

all_results <- list()
counter <- 0

for (i in seq_len(nrow(participants))) {
  p <- participants[i, ]
  pid <- p$ext_genid
  city <- p$city

  if (is.na(city)) next

  city_daily <- daily_climate[[city]]
  city_hw <- heatwaves[[city]]
  climate_dates <- city_daily$date

  # Get assessment dates for cumulative windows
  p_assessments <- assessment_dates %>% filter(ext_genid == pid)

  # Define windows (use latest assessment for cumulative)
  latest_assessment <- if (nrow(p_assessments) > 0) max(p_assessments$d_date) else NULL
  windows <- define_windows(p$birth_date, latest_assessment)

  for (w in seq_len(nrow(windows))) {
    win <- windows[w, ]

    # Coverage
    cov_frac <- compute_coverage(win$start, win$end, climate_dates)

    # General climate indicators
    gen_climate <- compute_general_climate(city_daily, win$start, win$end)

    # HW metrics per definition
    for (hw_def in hw_defs) {
      hw_metrics <- compute_hw_metrics(city_hw, city_daily, win$start, win$end, hw_def)

      counter <- counter + 1
      all_results[[counter]] <- bind_cols(
        tibble(
          ext_genid = pid,
          city = city,
          window = win$window,
          window_start = win$start,
          window_end = win$end,
          coverage_frac = cov_frac,
          hw_def = hw_def
        ),
        hw_metrics,
        gen_climate
      )
    }
  }

  if (i %% 100 == 0) cat("  Processed participant", i, "of", nrow(participants), "\n")
}

csei_components <- bind_rows(all_results)
cat("\nCSEI components computed:", nrow(csei_components), "rows\n")
cat("Columns:", ncol(csei_components), "\n")

saveRDS(csei_components, "data/processed/csei_components.rds")
cat("Saved to data/processed/csei_components.rds\n")
```

- [ ] **Step 2: Run the computation**

```bash
Rscript R/05_compute_csei_components.R
```

Expected: ~2500 participants × 4 windows × 18 HW definitions = ~180,000 rows. This will take several minutes due to threshold recomputation. Console shows progress every 100 participants.

**Note:** If runtime is too long (>30 min), optimize by pre-computing all thresholds once and passing them in, rather than recomputing inside `compute_hw_metrics`. The code is correct but can be optimized after first run confirms correctness.

- [ ] **Step 3: Sanity check**

```bash
Rscript -e '
  comp <- readRDS("data/processed/csei_components.rds")
  cat("Rows:", nrow(comp), "\n")
  cat("Participants:", length(unique(comp$ext_genid)), "\n")
  cat("Windows:", unique(comp$window), "\n")
  cat("HW defs:", length(unique(comp$hw_def)), "\n")
  cat("Coverage by window:\n")
  print(comp %>% dplyr::group_by(window) %>% dplyr::summarise(mean_cov = mean(coverage_frac, na.rm=TRUE)))
'
```

Expected: Mean coverage near 0 for 0_5 window (most births before 2005), increasing for later windows.

- [ ] **Step 4: Commit**

```bash
git add R/05_compute_csei_components.R
git commit -m "feat: compute per-participant CSEI component metrics across windows"
```

---

### Task 6: Build CSEI (PCA + Z-score)

**Files:**
- Create: `R/06_build_csei.R`

- [ ] **Step 1: Write R/06_build_csei.R**

```r
# R/06_build_csei.R
# Construct CSEI via PCA (primary) and z-score mean (sensitivity).
# Produce scree plots and loading tables.
#
# Input:  data/processed/csei_components.rds
# Output: data/processed/csei_scores.rds
#         output/figures/scree_plot_primary.pdf
#         output/tables/pca_loadings.csv

source("R/utils.R")
library(psych)

components <- readRDS("data/processed/csei_components.rds")

# Core metrics for CSEI construction
core_metrics <- c("hw_count", "hw_mean_excess", "hw_total_days",
                   "hw_degree_days", "hw_mean_humidity", "hw_apparent_excess")

# --- Build CSEI per window x HW definition ---
build_csei <- function(df, window_name, hw_def_name) {
  sub <- df %>%
    filter(window == window_name, hw_def == hw_def_name) %>%
    filter(coverage_frac >= 0.5)  # Minimum 50% coverage

  if (nrow(sub) < 20) {
    cat("    Skipping", window_name, hw_def_name, "- too few observations (", nrow(sub), ")\n")
    return(NULL)
  }

  # Extract core metrics matrix
  mat <- sub %>% select(all_of(core_metrics)) %>% as.matrix()

  # Replace NA with 0 for hw_count and hw_total_days (no events = 0)
  mat[, "hw_count"][is.na(mat[, "hw_count"])] <- 0
  mat[, "hw_total_days"][is.na(mat[, "hw_total_days"])] <- 0
  mat[, "hw_degree_days"][is.na(mat[, "hw_degree_days"])] <- 0

  # For intensity metrics, replace NA with 0 (no events = no excess)
  mat[is.na(mat)] <- 0

  # Z-score standardization
  z_mat <- scale(mat)

  # Handle zero-variance columns (all same value)
  zero_var <- apply(z_mat, 2, function(x) all(is.na(x)))
  if (any(zero_var)) {
    cat("    Warning: zero-variance columns in", window_name, hw_def_name, ":",
        core_metrics[zero_var], "\n")
    z_mat[, zero_var] <- 0
  }

  # --- PCA ---
  pca_result <- tryCatch({
    prcomp(z_mat, center = FALSE, scale. = FALSE)  # already z-scored
  }, error = function(e) {
    cat("    PCA failed for", window_name, hw_def_name, ":", e$message, "\n")
    NULL
  })

  if (is.null(pca_result)) return(NULL)

  pc1_score <- pca_result$x[, 1]
  var_explained <- summary(pca_result)$importance[2, 1]  # proportion of variance
  loadings <- pca_result$rotation[, 1]

  # --- Z-score mean ---
  zsum_score <- rowMeans(z_mat, na.rm = TRUE)

  # Assemble results
  sub %>%
    mutate(
      csei_pca = pc1_score,
      csei_zsum = zsum_score,
      pca_var_explained = var_explained
    ) %>%
    select(ext_genid, window, hw_def, coverage_frac, csei_pca, csei_zsum, pca_var_explained)
}

# --- Run across all combinations ---
windows <- unique(components$window)
hw_defs <- unique(components$hw_def)

cat("Building CSEI across", length(windows), "windows x", length(hw_defs), "definitions\n")

all_scores <- list()
all_loadings <- list()
counter <- 0

for (win in windows) {
  for (hw_def in hw_defs) {
    cat("  Processing:", win, "/", hw_def, "\n")

    result <- build_csei(components, win, hw_def)
    if (!is.null(result)) {
      counter <- counter + 1
      all_scores[[counter]] <- result

      # Save loadings for this combination
      sub <- components %>%
        filter(window == win, hw_def == hw_def, coverage_frac >= 0.5)
      mat <- sub %>% select(all_of(core_metrics)) %>% as.matrix()
      mat[is.na(mat)] <- 0
      z_mat <- scale(mat)
      zero_var <- apply(z_mat, 2, function(x) all(is.na(x)))
      z_mat[, zero_var] <- 0
      pca <- prcomp(z_mat, center = FALSE, scale. = FALSE)
      all_loadings[[counter]] <- tibble(
        window = win,
        hw_def = hw_def,
        metric = core_metrics,
        loading = pca$rotation[, 1],
        var_explained = summary(pca)$importance[2, 1]
      )
    }
  }
}

csei_scores <- bind_rows(all_scores)
loadings_df <- bind_rows(all_loadings)

cat("\nCSEI scores computed:", nrow(csei_scores), "rows\n")

# --- Save ---
saveRDS(csei_scores, "data/processed/csei_scores.rds")
write_csv(loadings_df, "output/tables/pca_loadings.csv")

# --- Scree plot for primary definition (tmax_p90_d3) ---
primary_def <- "tmax_p90_d3"
for (win in windows) {
  sub <- components %>%
    filter(window == win, hw_def == primary_def, coverage_frac >= 0.5)
  if (nrow(sub) < 20) next
  mat <- sub %>% select(all_of(core_metrics)) %>% as.matrix()
  mat[is.na(mat)] <- 0
  z_mat <- scale(mat)
  zero_var <- apply(z_mat, 2, function(x) all(is.na(x)))
  z_mat[, zero_var] <- 0
  pca <- prcomp(z_mat, center = FALSE, scale. = FALSE)

  pdf(paste0("output/figures/scree_plot_", win, "_primary.pdf"), width = 6, height = 4)
  var_pct <- summary(pca)$importance[2, ] * 100
  barplot(var_pct, names.arg = paste0("PC", seq_along(var_pct)),
          main = paste("Scree Plot -", win, "(", primary_def, ")"),
          ylab = "Variance Explained (%)", col = "steelblue")
  dev.off()
}

# --- Spearman correlation between PCA and z-score ---
cor_results <- csei_scores %>%
  group_by(window, hw_def) %>%
  summarise(
    spearman_rho = cor(csei_pca, csei_zsum, method = "spearman", use = "complete.obs"),
    n = n(),
    .groups = "drop"
  )

write_csv(cor_results, "output/tables/pca_vs_zscore_correlation.csv")
cat("\nPCA vs z-score Spearman rho:\n")
print(cor_results %>% summarise(mean_rho = mean(spearman_rho, na.rm = TRUE),
                                 min_rho = min(spearman_rho, na.rm = TRUE)))

cat("\nCSEI construction complete.\n")
```

- [ ] **Step 2: Run the CSEI construction**

```bash
Rscript R/06_build_csei.R
```

Expected: CSEI scores computed for most window × definition combos. Early windows (0_5) may be skipped due to low coverage. Spearman rho between PCA and z-score should be high (>0.8).

- [ ] **Step 3: Verify outputs**

```bash
Rscript -e '
  scores <- readRDS("data/processed/csei_scores.rds")
  cat("Score rows:", nrow(scores), "\n")
  cat("Windows with scores:", unique(scores$window), "\n")
  cat("PCA variance explained (primary def):\n")
  print(scores %>% dplyr::filter(hw_def == "tmax_p90_d3") %>%
    dplyr::group_by(window) %>%
    dplyr::summarise(var_expl = first(pca_var_explained), n = dplyr::n()))
'
```

- [ ] **Step 4: Commit**

```bash
git add R/06_build_csei.R
git commit -m "feat: build CSEI via PCA and z-score with scree plots and loadings"
```

---

### Task 7: Merge Analytical Dataset

**Files:**
- Create: `R/07_merge_analytical.R`

- [ ] **Step 1: Write R/07_merge_analytical.R**

```r
# R/07_merge_analytical.R
# Merge CSEI scores and components into the cohort data.
# Produce final analytical dataset.
#
# Input:  data/Santoro_climate_BHRC_2025_12_19.rds
#         data/processed/csei_scores.rds
#         data/processed/csei_components.rds
#         data/processed/participant_birthdates.rds
# Output: data/analytical/bhrc_csei_analytical.rds
#         output/tables/descriptive_stats.csv

source("R/utils.R")

cohort <- readRDS("data/Santoro_climate_BHRC_2025_12_19.rds")
scores <- readRDS("data/processed/csei_scores.rds")
components <- readRDS("data/processed/csei_components.rds")
birthdates <- readRDS("data/processed/participant_birthdates.rds")

# --- Pivot CSEI scores to wide format ---
# Primary definition for the main wide columns
primary_def <- "tmax_p90_d3"

scores_primary <- scores %>%
  filter(hw_def == primary_def) %>%
  select(ext_genid, window, csei_pca, csei_zsum, coverage_frac) %>%
  pivot_wider(
    names_from = window,
    values_from = c(csei_pca, csei_zsum, coverage_frac),
    names_glue = "{.value}_{window}"
  )

cat("Primary CSEI scores (wide):", nrow(scores_primary), "participants\n")

# --- Pivot key component metrics for primary definition ---
components_primary <- components %>%
  filter(hw_def == primary_def) %>%
  select(ext_genid, window, hw_count, hw_mean_excess, hw_total_days,
         hw_degree_days, hw_mean_humidity, hw_apparent_excess,
         mean_tmax, mean_tmean, days_above_p90, mean_humidity, total_precip, tmax_sd) %>%
  pivot_wider(
    names_from = window,
    values_from = c(hw_count, hw_mean_excess, hw_total_days, hw_degree_days,
                    hw_mean_humidity, hw_apparent_excess,
                    mean_tmax, mean_tmean, days_above_p90, mean_humidity, total_precip, tmax_sd),
    names_glue = "{.value}_{window}"
  )

# --- Add birth date info ---
birth_info <- birthdates %>%
  select(ext_genid, birth_date, birth_year, city)

# --- Merge into cohort ---
analytical <- cohort %>%
  left_join(birth_info, by = "ext_genid") %>%
  left_join(scores_primary, by = "ext_genid") %>%
  left_join(components_primary, by = "ext_genid")

cat("Analytical dataset dimensions:", dim(analytical), "\n")
cat("Participants with CSEI:", sum(!is.na(analytical$csei_pca_cumulative)), "\n")

# --- Save ---
saveRDS(analytical, "data/analytical/bhrc_csei_analytical.rds")

# --- Also save full scores (all definitions) as separate file ---
saveRDS(scores, "data/analytical/csei_all_definitions.rds")

# --- Descriptive statistics ---
desc_stats <- analytical %>%
  filter(redcap_event_name == "wave0_arm_1") %>%
  summarise(
    n = n(),
    n_with_csei = sum(!is.na(csei_pca_cumulative)),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    pct_female = mean(gender == 2, na.rm = TRUE) * 100,
    n_poa = sum(site == 1, na.rm = TRUE),
    n_sp = sum(site == 2, na.rm = TRUE),
    csei_pca_mean = mean(csei_pca_cumulative, na.rm = TRUE),
    csei_pca_sd = sd(csei_pca_cumulative, na.rm = TRUE),
    coverage_cumul_mean = mean(coverage_frac_cumulative, na.rm = TRUE)
  )

write_csv(desc_stats, "output/tables/descriptive_stats.csv")
cat("\nDescriptive stats:\n")
print(desc_stats)

cat("\nAnalytical dataset saved to data/analytical/bhrc_csei_analytical.rds\n")
```

- [ ] **Step 2: Run the merge**

```bash
Rscript R/07_merge_analytical.R
```

Expected: Analytical dataset has 10,044 rows (all waves), with CSEI columns added. Descriptive stats printed.

- [ ] **Step 3: Commit**

```bash
git add R/07_merge_analytical.R
git commit -m "feat: merge CSEI scores into cohort analytical dataset"
```

---

### Task 8: Master Script and Final Verification

**Files:**
- Create: `R/run_all.R`

- [ ] **Step 1: Write R/run_all.R**

```r
# R/run_all.R
# Master script: run the full CSEI pipeline from extraction to analytical dataset.

cat("=== CSEI Pipeline ===\n\n")

steps <- c(
  "R/01_extract_inmet.R",
  "R/02_clean_daily_climate.R",
  "R/03_identify_heatwaves.R",
  "R/04_derive_birth_dates.R",
  "R/05_compute_csei_components.R",
  "R/06_build_csei.R",
  "R/07_merge_analytical.R"
)

for (step in steps) {
  cat("\n========================================\n")
  cat("Running:", step, "\n")
  cat("========================================\n\n")
  source(step, local = new.env())
  cat("\n", step, "completed.\n")
}

cat("\n=== Pipeline complete ===\n")
cat("Final dataset: data/analytical/bhrc_csei_analytical.rds\n")
```

- [ ] **Step 2: Verify all scripts exist**

```bash
ls -la R/*.R
```

Expected: 9 files (utils.R, 01–07, run_all.R).

- [ ] **Step 3: Run the full pipeline**

```bash
Rscript R/run_all.R
```

Expected: All 7 steps run sequentially without errors. Final analytical dataset created.

- [ ] **Step 4: Final data verification**

```bash
Rscript -e '
  df <- readRDS("data/analytical/bhrc_csei_analytical.rds")
  cat("Final dataset: ", nrow(df), "rows x", ncol(df), "cols\n")
  csei_cols <- grep("csei_", names(df), value = TRUE)
  cat("CSEI columns:", length(csei_cols), "\n")
  cat(csei_cols, sep = "\n")
  cat("\nCoverage columns:\n")
  cov_cols <- grep("coverage_", names(df), value = TRUE)
  cat(cov_cols, sep = "\n")
'
```

- [ ] **Step 5: Commit**

```bash
git add R/run_all.R
git commit -m "feat: add master pipeline script and complete CSEI implementation"
```
