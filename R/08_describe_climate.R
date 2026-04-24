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
