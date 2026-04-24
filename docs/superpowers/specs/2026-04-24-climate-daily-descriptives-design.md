# Climate Daily Descriptives — Design

**Date:** 2026-04-24
**Scope:** Descriptive-statistics table for the processed daily climate data (POA + SP). Tables only, no images (per brainstorming decision).

## Goal

Produce a single tidy CSV summarising the daily climate variables used by the CSEI pipeline, with per-city descriptive statistics. Output is intended for manuscript Table 1 / supplementary material.

## Inputs

- `data/processed/daily_climate_poa.rds` — daily climate, Porto Alegre, 2010-01-01 through 2025-12-31 (~5844 rows × 15 cols).
- `data/processed/daily_climate_sp.rds` — daily climate, São Paulo, same coverage.

Both files are produced by `R/02_clean_daily_climate.R`.

## Output

- `output/tables/climate_daily_descriptives.csv`

Long/tidy shape: one row per `(city, variable)` pair.

| column      | type    | notes                                      |
|-------------|---------|--------------------------------------------|
| `city`      | chr     | `"poa"` or `"sp"`                          |
| `variable`  | chr     | variable name (see list below)             |
| `n_total`   | int     | total daily rows for that city             |
| `n_nonmiss` | int     | non-NA rows for that variable              |
| `mean`      | dbl     | `mean(x, na.rm = TRUE)`                    |
| `sd`        | dbl     | `sd(x, na.rm = TRUE)`                      |
| `median`    | dbl     | `median(x, na.rm = TRUE)`                  |
| `p25`       | dbl     | `quantile(x, 0.25, na.rm = TRUE)`          |
| `p75`       | dbl     | `quantile(x, 0.75, na.rm = TRUE)`          |
| `min`       | dbl     | `min(x, na.rm = TRUE)`                     |
| `max`       | dbl     | `max(x, na.rm = TRUE)`                     |

Expected row count: 2 cities × 11 variables = **22 rows**.

## Variables summarised

Continuous climate measurements only. Ordered semantically in the output (temperature → humidity → other atmospheric → precipitation → derived), not alphabetically:

1. `tmax_daily`
2. `tmin_daily`
3. `tmean_daily`
4. `dewpoint_daily`
5. `humidity_mean`
6. `humidity_min`
7. `pressure_mean`
8. `radiation_total`
9. `wind_mean`
10. `precip_total`
11. `apparent_temp` (pre-computed upstream in `02_clean_daily_climate.R` via `compute_apparent_temp()` from `utils.R`)

**Explicitly excluded** (not physical measurements): `date`, `n_obs`, `n_tmax_valid`, `long_gap`.

## Architecture

Single script: `R/08_describe_climate.R`. Follows the 01–07 convention:

- `source("R/utils.R")` for shared libraries (dplyr, tidyr, readr, tibble, purrr).
- Header block with short description and a list of inputs/outputs.
- Numbered sections with `# ── Section … ─────` divider comments.
- Console progress via `cat()` at each stage (loading, summarising, writing).
- No function exports; script is run top-to-bottom.

Structure:

1. I/O paths and constants (variable list + order).
2. Load both RDS files, bind with a `city` column.
3. Helper `summary_row(var_name, x)` returning a one-row tibble (same shape used in `07_merge_analytical.R` — copy, don't share, to keep scripts independent).
4. For each `city`, map over the variable list to build the summary tibble; `bind_rows` across cities.
5. Enforce semantic variable order via a factor or explicit `arrange()`.
6. Write CSV.
7. Verification block: print row count, show `head(desc, 22)`.

## Error handling

- `stopifnot(file.exists(poa_file), file.exists(sp_file))` at top.
- `dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)`.
- No special handling for all-NA columns; `mean`/`sd` returning `NaN`/`NA` would be an acceptable signal that the column is empty (it shouldn't be — `02_clean_daily_climate.R` guarantees non-empty columns for this date range).

## Integration

Add one line to `R/run_all.R` after step 07:

```r
source("R/08_describe_climate.R")
```

The script is pure derived output — no downstream step depends on its CSV — so placement after 07 is for run-order clarity, not dependency.

## Verification checklist

After running:

- [ ] CSV has exactly 22 rows.
- [ ] `n_total` is 5844 for both cities (or matches `nrow()` of each input — exact number verified at runtime).
- [ ] No `NaN` or `Inf` in any numeric column.
- [ ] Variable order matches the semantic list above.
- [ ] Tmax means are in plausible ranges (POA ~25°C, SP ~24°C annual mean Tmax ballpark).
- [ ] `precip_total` min is ≥ 0.

## Out of scope

- Plots / images (explicitly ruled out).
- Stratification by year, season, or month.
- Heatwave event descriptives (separate dataset, not requested).
- Publication formatting (`gt`, `flextable`, Markdown tables) — downstream reporting will format the CSV.
