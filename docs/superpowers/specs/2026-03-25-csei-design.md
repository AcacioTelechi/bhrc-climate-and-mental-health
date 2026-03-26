# Climate Stress Exposure Index (CSEI) — Design Spec

## Overview

Construct a Climate Stress Exposure Index (CSEI) for participants in the Brazilian High-Risk Cohort (BHRC) study, linking meteorological data from INMET weather stations to participant developmental windows. The CSEI captures cumulative heat wave exposure and general climate stress across childhood and adolescence.

## Data Sources

### Cohort data
- **File:** `data/Santoro_climate_BHRC_2025_12_19.rds`
- **Structure:** 2,511 participants × 4 waves (wave0–wave3), long format (10,044 rows × 4,122 columns)
- **Sites:** Porto Alegre (site=1) and São Paulo (site=2)
- **Neuroimaging:** 2,436 observations with FreeSurfer parcellation (area, thickness, volume, curvature) and subcortical volumes (hippocampus, amygdala, thalamus)
- **Key covariates:** `age`, `gender`, `site`, `abepscore`/`abepstrat`, `ni_scanner`
- **Assessment dates:** `d_date` (2010-08-15 to 2019-12-21), `ni_acquisition_date` (2010-10-17 to 2025-07-07)

### Climate data
- **Source:** INMET automatic weather stations
  - Porto Alegre: station A801
  - São Paulo: station A701 (Mirante de Santana)
- **Files:** Nested zips in `data/Base de dados Clima-20260325T214746Z-1-001.zip`
  - Yearly zips (2010–2025), each containing per-station CSVs with hourly data
  - Additional xlsx files for 2021 (both cities)
- **Hourly variables:**
  - Temperature: dry bulb (instantaneous), Tmax/Tmin (previous hour, automatic)
  - Dew point temperature
  - Relative humidity (instantaneous, max, min)
  - Atmospheric pressure
  - Solar radiation (Kj/m2)
  - Wind speed and direction
  - Precipitation (mm)
- **Format notes:** Comma decimal separator, header at row 9 in xlsx files, CSV format varies by year

## Step 1: Data Extraction & Cleaning

### 1.1 Extract INMET station data
- Extract station A801 and A701 files from each yearly zip (2010–2025)
- Handle both CSV (2010–2020) and XLSX (2021) formats
- Standardize column names across years

### 1.2 Hourly to daily aggregation
- `tmax_daily`: max of hourly Tmax readings
- `tmin_daily`: min of hourly Tmin readings
- `tmean_daily`: mean of hourly dry bulb temperature
- `dewpoint_daily`: mean of hourly dew point
- `humidity_mean`: mean of hourly relative humidity
- `humidity_min`: min of hourly relative humidity
- `pressure_mean`: mean of hourly atmospheric pressure
- `radiation_total`: sum of hourly solar radiation
- `wind_mean`: mean of hourly wind speed
- `precip_total`: sum of hourly precipitation

### 1.3 Quality control
- Convert comma decimal separators to numeric
- Flag values outside physical range (e.g., Tmax > 50°C or < -10°C for these latitudes)
- Interpolate gaps ≤ 3 days (linear interpolation)
- Flag gaps > 3 days (retain as NA, track missingness)
- Compute daily apparent temperature / heat index from Tmax + humidity + wind

### 1.4 Output
- `data/processed/daily_climate_poa.rds` — clean daily timeseries for Porto Alegre
- `data/processed/daily_climate_sp.rds` — clean daily timeseries for São Paulo

## Step 2: Heat Wave Identification

### 2.1 Threshold computation
- Compute percentile thresholds per city based on full 2010–2025 baseline
- Use 15-day rolling calendar window (e.g., for Jan 15, pool all Jan 8–22 observations across all years) to account for seasonality
- Thresholds: 90th, 92.5th, 95th percentile
- Apply to both raw Tmax and apparent temperature

### 2.2 Heat wave definitions (18 variants)
Grid of 3 thresholds × 3 minimum durations × 2 thermal metrics:

| Threshold | Min consecutive days | Thermal metric |
|-----------|---------------------|----------------|
| 90th percentile | 2, 3, 5 days | Tmax |
| 92.5th percentile | 2, 3, 5 days | Tmax |
| 95th percentile | 2, 3, 5 days | Tmax |
| 90th percentile | 2, 3, 5 days | Apparent temp |
| 92.5th percentile | 2, 3, 5 days | Apparent temp |
| 95th percentile | 2, 3, 5 days | Apparent temp |

### 2.3 Per heat wave event, record:
- Start/end dates
- Duration (days)
- Peak temperature (and peak apparent temperature)
- Mean excess above threshold
- Cumulative degree-days above threshold

### 2.4 Output
- `data/processed/heatwaves_poa.rds` — all HW events for Porto Alegre
- `data/processed/heatwaves_sp.rds` — all HW events for São Paulo

## Step 3: Participant Linkage & Developmental Windows

### 3.1 Birth date derivation
- `birth_date = d_date - (age * 365.25)`
- Use wave0 assessment where both `d_date` and `age` are available
- Cross-validate across waves where multiple assessments exist

### 3.2 Site linkage
- site = 1 → Porto Alegre (A801)
- site = 2 → São Paulo (A701)

### 3.3 Developmental windows
Per participant, define four exposure windows:
- **Window 0–5:** birth_date to birth_date + 5 years
- **Window 6–10:** birth_date + 6 years to birth_date + 10 years
- **Window 11–18:** birth_date + 11 years to birth_date + 18 years
- **Cumulative:** birth_date to assessment date (per wave)

### 3.4 Coverage tracking
Climate data spans 2010–2025. Many participants will have partial or no coverage for early windows.
- Compute `coverage_frac` = proportion of days in window with available climate data
- Birth years likely range ~1997–2013; earliest windows (0–5 for births before 2005) will have 0% coverage
- Downstream analyses should filter by minimum coverage (e.g., ≥ 80%) or adjust

## Step 4: CSEI Component Metrics

### 4.1 Heat wave metrics (per participant × window × HW definition)

| Component | Variable | Description |
|-----------|----------|-------------|
| Frequency | `hw_count` | Number of heat wave events |
| Intensity (mean) | `hw_mean_excess` | Mean temperature excess above threshold across all HW days |
| Intensity (peak) | `hw_max_excess` | Maximum single-day excess above threshold |
| Duration (total) | `hw_total_days` | Total days spent in heat waves |
| Duration (mean) | `hw_mean_duration` | Mean duration per heat wave event |
| Cumulative thermal load | `hw_degree_days` | Sum of (temp - threshold) across all HW days |
| Humidity load | `hw_mean_humidity` | Mean relative humidity during HW days |
| Precipitation deficit | `precip_deficit` | Cumulative precipitation deficit vs. climatological mean during HW periods |
| Apparent temp load | `hw_apparent_excess` | Degree-days using apparent temperature |

### 4.2 General climate stress indicators (per window, non-HW-specific)

| Variable | Description |
|----------|-------------|
| `mean_tmax` | Mean daily Tmax in window |
| `mean_tmean` | Mean daily Tmean in window |
| `days_above_thresh` | Number of days above threshold (not necessarily consecutive) |
| `mean_humidity` | Mean daily humidity in window |
| `total_precip` | Total precipitation in window |
| `tmax_sd` | Standard deviation of daily Tmax (climate variability) |

## Step 5: CSEI Construction

### 5.1 Primary: PCA-based CSEI
1. Select core HW metrics per participant × window: `hw_count`, `hw_mean_excess`, `hw_total_days`, `hw_degree_days`, `hw_mean_humidity`, `hw_apparent_excess`
2. Standardize (z-score) each metric
3. Run PCA, extract PC1 as the CSEI score
4. Report: variance explained, loadings, scree plot
5. Repeat for each of the 18 heat wave definition variants
6. **Primary definition for main analyses:** 90th percentile Tmax, 3+ consecutive days

### 5.2 Sensitivity: Equal-weight z-score index
1. Z-score the same core metrics
2. `csei_zsum` = mean of z-scores
3. Compare Spearman rank correlation with PCA-CSEI

### 5.3 Output variables per participant (wide format)
- `csei_pca_{window}_{hw_def}` — PCA-based score
- `csei_zsum_{window}_{hw_def}` — z-score sum
- `coverage_frac_{window}` — fraction of window with climate data
- All individual component metrics retained

## Step 6: Analytical Dataset

Merge CSEI scores and components back into the cohort data:
- Join on `ext_genid` (participant ID)
- Retain all covariates: `age`, `gender`, `site`, `abepscore`, `abepstrat`, `ni_scanner`
- Retain all neuroimaging outcomes (hippocampus, amygdala, medialorbitofrontal volumes/thickness)
- Output: `data/analytical/bhrc_csei_analytical.rds`

## Project Structure

```
bhrc-climate-and-mental-health/
├── data/
│   ├── raw/                          # Extracted INMET CSVs (gitignored)
│   │   ├── porto_alegre/
│   │   └── sao_paulo/
│   ├── processed/
│   │   ├── daily_climate_poa.rds
│   │   ├── daily_climate_sp.rds
│   │   ├── heatwaves_poa.rds
│   │   ├── heatwaves_sp.rds
│   │   └── csei_components.rds
│   └── analytical/
│       └── bhrc_csei_analytical.rds
├── R/
│   ├── 01_extract_inmet.R
│   ├── 02_clean_daily_climate.R
│   ├── 03_identify_heatwaves.R
│   ├── 04_derive_birth_dates.R
│   ├── 05_compute_csei_components.R
│   ├── 06_build_csei.R
│   ├── 07_merge_analytical.R
│   ├── run_all.R
│   └── utils.R
├── docs/
│   └── superpowers/specs/
└── output/
    ├── figures/
    └── tables/
```

## R Dependencies

`tidyverse`, `readxl`, `lubridate`, `janitor`, `psych`, `weathermetrics`

## Key Decisions

1. **Language:** R
2. **CSEI approach:** PCA (primary) + equal-weight z-score (sensitivity)
3. **Heat wave definitions:** 18 variants (3 thresholds × 3 durations × 2 thermal metrics)
4. **Primary HW definition:** 90th percentile Tmax, 3+ consecutive days
5. **Percentile baseline:** Full 2010–2025 period, 15-day rolling calendar window
6. **Birth date:** Derived from `age` + `d_date` at wave0
7. **Coverage constraint:** Climate data starts 2010; early windows for older participants will lack data
8. **Scope:** CSEI construction and analytical dataset only; neuroimaging analyses are out of scope
