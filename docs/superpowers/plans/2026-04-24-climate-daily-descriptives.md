# Climate Daily Descriptives Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `R/08_describe_climate.R` that writes `output/tables/climate_daily_descriptives.csv` — a tidy per-(city, variable) summary table of the processed daily climate data for Porto Alegre and São Paulo.

**Architecture:** Single self-contained R script that reads the two processed daily-climate RDS files, computes per-variable descriptive statistics for each city, writes one long/tidy CSV, and is added as step 08 to `R/run_all.R`. Follows the 01–07 script convention: `source("R/utils.R")`, numbered section dividers, `cat()` progress, `stopifnot()` verification at the end. No new package dependencies. No separate test framework (project has none) — verification is an in-script assertion block.

**Tech Stack:** R 4.3.3, libraries already loaded via `R/utils.R` (dplyr, tidyr, readr, tibble, purrr). R packages installed at `~/R/libs`.

**Spec:** `docs/superpowers/specs/2026-04-24-climate-daily-descriptives-design.md`

---

## File Structure

- **Create:** `R/08_describe_climate.R` — the new descriptives script.
- **Modify:** `R/run_all.R` — append the new step to the pipeline.
- **Produced at runtime:** `output/tables/climate_daily_descriptives.csv` — not committed (this project commits its CSVs under `output/tables/`; see e.g. `descriptive_stats.csv`). Commit the produced CSV to match convention.

Each file has one responsibility: `08_describe_climate.R` writes exactly one CSV; `run_all.R` orchestrates pipeline order.

---

### Task 1: Create the descriptives script (skeleton + summary logic)

**Files:**
- Create: `R/08_describe_climate.R`

- [ ] **Step 1: Create the script with the full implementation**

Create `R/08_describe_climate.R` with this exact content:

```r
source("R/utils.R")

# ── 08_describe_climate.R ─────────────────────────────────────────────────────
# Descriptive statistics for processed daily climate data (POA + SP).
#
# Inputs:
#   data/processed/daily_climate_poa.rds
#   data/processed/daily_climate_sp.rds
#
# Output:
#   output/tables/climate_daily_descriptives.csv
#     One row per (city, variable) — 2 cities × 11 variables = 22 rows.
# ──────────────────────────────────────────────────────────────────────────────

cat("=== 08_describe_climate.R ===\n")

# ── I/O paths ─────────────────────────────────────────────────────────────────
poa_file <- "data/processed/daily_climate_poa.rds"
sp_file  <- "data/processed/daily_climate_sp.rds"
out_dir  <- "output/tables"
out_csv  <- file.path(out_dir, "climate_daily_descriptives.csv")

stopifnot(file.exists(poa_file), file.exists(sp_file))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Constants: variables (in semantic order) ──────────────────────────────────
# Temperature → humidity → other atmospheric → precipitation → derived.
CLIMATE_VARS <- c(
  "tmax_daily", "tmin_daily", "tmean_daily", "dewpoint_daily",
  "humidity_mean", "humidity_min",
  "pressure_mean", "radiation_total", "wind_mean",
  "precip_total",
  "apparent_temp"
)

# ── Load and bind ─────────────────────────────────────────────────────────────
cat("Loading daily climate …\n")
poa <- readRDS(poa_file) %>% mutate(city = "poa")
sp  <- readRDS(sp_file)  %>% mutate(city = "sp")

cat(sprintf("POA: %d rows × %d cols\n", nrow(poa), ncol(poa)))
cat(sprintf("SP:  %d rows × %d cols\n", nrow(sp),  ncol(sp)))

stopifnot(all(CLIMATE_VARS %in% names(poa)))
stopifnot(all(CLIMATE_VARS %in% names(sp)))

daily <- bind_rows(poa, sp)

# ── Helper: one-row summary tibble for a variable × city ──────────────────────
summary_row <- function(city_name, var_name, x) {
  tibble(
    city      = city_name,
    variable  = var_name,
    n_total   = length(x),
    n_nonmiss = sum(!is.na(x)),
    mean      = mean(x, na.rm = TRUE),
    sd        = sd(x, na.rm = TRUE),
    median    = median(x, na.rm = TRUE),
    p25       = unname(quantile(x, 0.25, na.rm = TRUE)),
    p75       = unname(quantile(x, 0.75, na.rm = TRUE)),
    min       = min(x, na.rm = TRUE),
    max       = max(x, na.rm = TRUE)
  )
}

# ── Build per-(city × variable) summaries ─────────────────────────────────────
cat("Computing summaries …\n")

desc <- map_dfr(c("poa", "sp"), function(cty) {
  sub <- filter(daily, city == cty)
  map_dfr(CLIMATE_VARS, function(v) summary_row(cty, v, sub[[v]]))
})

# Enforce semantic variable order and city order.
desc <- desc %>%
  mutate(
    city     = factor(city,     levels = c("poa", "sp")),
    variable = factor(variable, levels = CLIMATE_VARS)
  ) %>%
  arrange(city, variable) %>%
  mutate(
    city     = as.character(city),
    variable = as.character(variable)
  )

# ── Write CSV ─────────────────────────────────────────────────────────────────
write_csv(desc, out_csv)
cat("Saved:", out_csv, "\n")

# ── Verification ──────────────────────────────────────────────────────────────
cat("\n--- Verification ---\n")
stopifnot(nrow(desc) == length(CLIMATE_VARS) * 2)  # 22 rows
stopifnot(!anyNA(desc$n_total), !anyNA(desc$n_nonmiss))
num_cols <- c("mean", "sd", "median", "p25", "p75", "min", "max")
for (col in num_cols) {
  vals <- desc[[col]]
  stopifnot(!any(is.nan(vals)), all(is.finite(vals)))
}
stopifnot(all(desc$n_total[desc$city == "poa"] == nrow(poa)))
stopifnot(all(desc$n_total[desc$city == "sp"]  == nrow(sp)))
stopifnot(min(desc$min[desc$variable == "precip_total"]) >= 0)

cat(sprintf("Rows: %d (expected %d)\n", nrow(desc), length(CLIMATE_VARS) * 2))
print(desc, n = Inf)

cat("\n=== 08_describe_climate.R complete ===\n")
```

- [ ] **Step 2: Run the script and confirm it succeeds**

Run from the project root:

```bash
Rscript R/08_describe_climate.R
```

Expected: the console shows `=== 08_describe_climate.R ===`, the loading lines, `Saved: output/tables/climate_daily_descriptives.csv`, the `--- Verification ---` block with `Rows: 22 (expected 22)`, a printed 22-row tibble, and `=== 08_describe_climate.R complete ===`. Exit status 0.

If any `stopifnot()` fails, fix the script — do NOT loosen the assertions. Likely causes: the RDS schema changed (unlikely for this session) or a variable contains all-NA slices.

- [ ] **Step 3: Inspect the CSV**

Run:

```bash
wc -l output/tables/climate_daily_descriptives.csv
head -1 output/tables/climate_daily_descriptives.csv
head -3 output/tables/climate_daily_descriptives.csv | tail -2
```

Expected:
- `wc -l` → `23` (22 data rows + 1 header).
- Header → `city,variable,n_total,n_nonmiss,mean,sd,median,p25,p75,min,max`
- First two data rows start with `poa,tmax_daily,...` and `poa,tmin_daily,...`.

- [ ] **Step 4: Sanity-check plausible values**

Run:

```bash
Rscript --vanilla -e '.libPaths("~/R/libs"); library(readr); d <- read_csv("output/tables/climate_daily_descriptives.csv", show_col_types=FALSE); print(subset(d, variable=="tmax_daily"))'
```

Expected: two rows (POA and SP) with `mean` in the 22–30 °C range and `max` ≤ 45 °C. If values are wildly off, STOP and investigate — the bug is in the script, not the assertions.

- [ ] **Step 5: Commit**

```bash
git add R/08_describe_climate.R output/tables/climate_daily_descriptives.csv
git commit -m "feat: add 08_describe_climate — per-city daily climate descriptives

Writes output/tables/climate_daily_descriptives.csv (22 rows = 2 cities ×
11 climate variables) with n_total, n_nonmiss, mean, sd, median, p25, p75,
min, max. Variables ordered semantically (temp → humidity → other → precip
→ derived). In-script stopifnot() verifies row count, non-NaN/Inf numerics,
precip non-negativity, and n_total matching input row counts.

Spec: docs/superpowers/specs/2026-04-24-climate-daily-descriptives-design.md"
```

---

### Task 2: Integrate into run_all.R

**Files:**
- Modify: `R/run_all.R` — append step 08 to the `steps` vector.

- [ ] **Step 1: Show the current run_all.R for reference**

Current content of `R/run_all.R` (lines 6–14):

```r
steps <- c(
  "R/01_extract_inmet.R",
  "R/02_clean_daily_climate.R",
  "R/03_identify_heatwaves.R",
  "R/04_derive_birth_dates.R",
  "R/05_compute_csei_components.R",
  "R/06_build_csei.R",
  "R/07_merge_analytical.R"
)
```

- [ ] **Step 2: Add step 08 after step 07**

Edit `R/run_all.R` — change the line `  "R/07_merge_analytical.R"` to include the new step after it. Target content of the `steps` vector:

```r
steps <- c(
  "R/01_extract_inmet.R",
  "R/02_clean_daily_climate.R",
  "R/03_identify_heatwaves.R",
  "R/04_derive_birth_dates.R",
  "R/05_compute_csei_components.R",
  "R/06_build_csei.R",
  "R/07_merge_analytical.R",
  "R/08_describe_climate.R"
)
```

Use the Edit tool: replace the exact string `  "R/07_merge_analytical.R"\n)` with `  "R/07_merge_analytical.R",\n  "R/08_describe_climate.R"\n)`.

- [ ] **Step 3: Verify step 08 alone still runs standalone (sanity)**

```bash
Rscript R/08_describe_climate.R
```

Expected: same successful output as Task 1 Step 2. This confirms the integration change didn't break standalone execution (trivial here — nothing in 08 depends on run_all.R — but worth a quick pass).

- [ ] **Step 4: Commit**

```bash
git add R/run_all.R
git commit -m "chore: add 08_describe_climate to run_all.R pipeline"
```

---

## Self-Review

**Spec coverage:**
- Inputs (POA + SP RDS): Task 1 Step 1, lines loading `poa_file`, `sp_file`. ✓
- Output CSV path and column schema: Task 1 Step 1, `summary_row()` + `write_csv()`. ✓
- 11 variables in semantic order: Task 1 Step 1, `CLIMATE_VARS` constant + factor-based `arrange()`. ✓
- 22-row expected count: Task 1 Step 1 verification + Task 1 Step 3 `wc -l`. ✓
- Excluded columns (`date`, `n_obs`, `n_tmax_valid`, `long_gap`): enforced by `CLIMATE_VARS` allow-list rather than explicit exclusion. ✓
- `stopifnot` on file existence: Task 1 Step 1. ✓
- `dir.create` on output dir: Task 1 Step 1. ✓
- Script follows 01–07 convention (`source("R/utils.R")`, `cat()` progress, dividers, final verification): Task 1 Step 1. ✓
- `run_all.R` integration: Task 2. ✓
- Verification checklist (22 rows, no NaN/Inf, variable order, plausible tmax, precip ≥ 0): Task 1 Step 2 (in-script) + Step 3 + Step 4. ✓
- `n_total` matches input `nrow()`: Task 1 Step 1 verification block (stopifnot). ✓

No gaps.

**Placeholder scan:** No TBD/TODO/"implement later" strings. All steps contain either complete code or exact commands with expected output.

**Type consistency:** `CLIMATE_VARS` referenced in both the allow-list check, the summary map, and the factor-levels ordering — one source of truth. `summary_row()` signature matches its two call sites. Column names in `write_csv` output match the spec's table schema exactly.
