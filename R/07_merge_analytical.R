source("R/utils.R")

# ── 07_merge_analytical.R ──────────────────────────────────────────────────────
# Merges CSEI scores and component metrics into the BHRC analytical dataset.
#
# Outputs:
#   data/analytical/bhrc_csei_analytical.rds  — wide-format analytical dataset
#   data/analytical/csei_all_definitions.rds  — long-format scores (all 18 defs)
#   output/tables/descriptive_stats.csv       — wave0 descriptive statistics
# ──────────────────────────────────────────────────────────────────────────────

cat("=== 07_merge_analytical.R ===\n")

# ── I/O paths ─────────────────────────────────────────────────────────────────
cohort_file     <- "data/Santoro_climate_BHRC_2025_12_19.rds"
scores_file     <- "data/processed/csei_scores.rds"
components_file <- "data/processed/csei_components.rds"
births_file     <- "data/processed/participant_birthdates.rds"

out_dir         <- "data/analytical"
out_analytical  <- file.path(out_dir, "bhrc_csei_analytical.rds")
out_all_defs    <- file.path(out_dir, "csei_all_definitions.rds")
out_desc        <- "output/tables/descriptive_stats.csv"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ── Constants ─────────────────────────────────────────────────────────────────
WINDOWS_TO_PIVOT <- c("6_10", "11_18", "cumulative")

COMPONENT_COLS <- c(
  "hw_count", "hw_mean_excess", "hw_total_days",
  "hw_degree_days", "hw_mean_humidity", "hw_apparent_excess",
  "mean_tmax", "mean_tmean", "days_above_p90",
  "mean_humidity", "total_precip", "tmax_sd"
)

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading data …\n")
cohort     <- readRDS(cohort_file)
scores     <- readRDS(scores_file)
components <- readRDS(components_file)
births     <- readRDS(births_file)

cat(sprintf("Cohort:     %d rows × %d cols\n",       nrow(cohort),     ncol(cohort)))
cat(sprintf("Scores:     %d rows × %d cols\n",       nrow(scores),     ncol(scores)))
cat(sprintf("Components: %d rows × %d cols\n",       nrow(components), ncol(components)))
cat(sprintf("Births:     %d rows × %d cols\n",       nrow(births),     ncol(births)))

# ── Detect primary definition ─────────────────────────────────────────────────
all_defs <- unique(scores$hw_def)
cat("HW definitions in scores:\n"); print(all_defs)

PRIMARY_DEF <- if ("tmax_p90_d3" %in% all_defs) "tmax_p90_d3" else {
  # fall back to any tmax_p90 variant
  candidates <- grep("tmax_p90", all_defs, value = TRUE)
  if (length(candidates) > 0) candidates[1] else all_defs[1]
}
cat(sprintf("Primary HW definition: %s\n", PRIMARY_DEF))

# ── Section 1: Pivot CSEI scores (primary def) to wide ────────────────────────
cat("\nPivoting CSEI scores (primary def) to wide …\n")

scores_primary <- scores %>%
  filter(hw_def == PRIMARY_DEF, window %in% WINDOWS_TO_PIVOT) %>%
  select(ext_genid, window, coverage_frac, csei_pca, csei_zsum)

# Pivot csei_pca
csei_pca_wide <- scores_primary %>%
  select(ext_genid, window, csei_pca) %>%
  pivot_wider(
    names_from  = window,
    values_from = csei_pca,
    names_prefix = "csei_pca_"
  )

# Pivot csei_zsum
csei_zsum_wide <- scores_primary %>%
  select(ext_genid, window, csei_zsum) %>%
  pivot_wider(
    names_from  = window,
    values_from = csei_zsum,
    names_prefix = "csei_zsum_"
  )

# Pivot coverage_frac
coverage_wide <- scores_primary %>%
  select(ext_genid, window, coverage_frac) %>%
  pivot_wider(
    names_from  = window,
    values_from = coverage_frac,
    names_prefix = "coverage_frac_"
  )

csei_wide <- csei_pca_wide %>%
  left_join(csei_zsum_wide, by = "ext_genid") %>%
  left_join(coverage_wide,  by = "ext_genid")

cat(sprintf("CSEI wide: %d rows × %d cols\n", nrow(csei_wide), ncol(csei_wide)))
cat("CSEI columns:", paste(names(csei_wide)[-1], collapse = ", "), "\n")

# ── Section 2: Pivot key component metrics (primary def) to wide ──────────────
cat("\nPivoting component metrics (primary def) to wide …\n")

comps_primary <- components %>%
  filter(hw_def == PRIMARY_DEF, window %in% WINDOWS_TO_PIVOT) %>%
  select(all_of(c("ext_genid", "window", COMPONENT_COLS)))

comps_wide <- comps_primary %>%
  pivot_wider(
    names_from  = window,
    values_from = all_of(COMPONENT_COLS),
    names_glue  = "{.value}_{window}"
  )

cat(sprintf("Components wide: %d rows × %d cols\n", nrow(comps_wide), ncol(comps_wide)))

# ── Section 3: Birth date info ────────────────────────────────────────────────
cat("\nPreparing birth date info …\n")

births_slim <- births %>%
  select(ext_genid, birth_date, birth_year, city,
         birth_site = site, birth_gender = gender)

cat(sprintf("Birth date records: %d\n", nrow(births_slim)))

# ── Section 4: Merge into cohort ──────────────────────────────────────────────
cat("\nMerging into cohort (left join, preserving all 10,044 rows) …\n")

analytical <- cohort %>%
  left_join(births_slim,  by = "ext_genid") %>%
  left_join(csei_wide,    by = "ext_genid") %>%
  left_join(comps_wide,   by = "ext_genid")

cat(sprintf("Analytical dataset: %d rows × %d cols\n", nrow(analytical), ncol(analytical)))
stopifnot(nrow(analytical) == nrow(cohort))

# ── Section 5: Save outputs ───────────────────────────────────────────────────
cat("\nSaving analytical dataset …\n")
saveRDS(analytical, out_analytical)
cat("Saved:", out_analytical, "\n")

# Save full scores (all definitions, long format)
cat("Saving all-definitions scores (long) …\n")
saveRDS(scores, out_all_defs)
cat("Saved:", out_all_defs, "\n")

# ── Section 6: Descriptive statistics (wave0) ─────────────────────────────────
cat("\nComputing descriptive stats for wave0 …\n")

# Wave0 subset
w0 <- analytical %>%
  filter(redcap_event_name == "wave0_arm_1")

cat(sprintf("Wave0 rows: %d\n", nrow(w0)))

# CSEI columns (primary def, all windows)
csei_cols <- grep("^csei_pca_|^csei_zsum_", names(w0), value = TRUE)
cov_cols  <- grep("^coverage_frac_", names(w0), value = TRUE)

cat("CSEI score columns:", paste(csei_cols, collapse = ", "), "\n")
cat("Coverage columns:  ", paste(cov_cols,  collapse = ", "), "\n")

# Helper: build a row of summary stats
summary_row <- function(var_name, x) {
  tibble(
    variable  = var_name,
    n_total   = length(x),
    n_nonmiss = sum(!is.na(x)),
    mean      = mean(x, na.rm = TRUE),
    sd        = sd(x, na.rm = TRUE),
    median    = median(x, na.rm = TRUE),
    p25       = quantile(x, 0.25, na.rm = TRUE),
    p75       = quantile(x, 0.75, na.rm = TRUE),
    min       = min(x, na.rm = TRUE),
    max       = max(x, na.rm = TRUE)
  )
}

# Numeric summaries
numeric_stats <- bind_rows(
  summary_row("age_wave0", as.numeric(w0$age)),
  map(csei_cols, ~ summary_row(.x, as.numeric(w0[[.x]]))),
  map(cov_cols,  ~ summary_row(.x, as.numeric(w0[[.x]])))
)

# Categorical: site (from cohort, haven_labelled → numeric)
site_tab <- w0 %>%
  mutate(site_num = as.numeric(haven::as_factor(site))) %>%
  count(site_num) %>%
  mutate(
    variable  = paste0("site_", site_num),
    n_total   = nrow(w0),
    n_nonmiss = n,
    pct       = round(100 * n / nrow(w0), 1),
    mean      = NA_real_, sd = NA_real_, median = NA_real_,
    p25       = NA_real_, p75 = NA_real_,
    min       = NA_real_, max = NA_real_
  ) %>%
  select(variable, n_total, n_nonmiss, mean, sd, median, p25, p75, min, max)

# Categorical: gender (from cohort, haven_labelled → factor label)
gender_tab <- w0 %>%
  mutate(gender_lbl = as.character(haven::as_factor(gender))) %>%
  count(gender_lbl) %>%
  mutate(
    variable  = paste0("gender_", gender_lbl),
    n_total   = nrow(w0),
    n_nonmiss = n,
    pct       = round(100 * n / nrow(w0), 1),
    mean      = NA_real_, sd = NA_real_, median = NA_real_,
    p25       = NA_real_, p75 = NA_real_,
    min       = NA_real_, max = NA_real_
  ) %>%
  select(variable, n_total, n_nonmiss, mean, sd, median, p25, p75, min, max)

# N with any CSEI score
n_with_csei <- sum(!is.na(w0$csei_pca_6_10) |
                   !is.na(w0$csei_pca_11_18) |
                   !is.na(w0$csei_pca_cumulative))

cat(sprintf("Wave0 participants with any CSEI score: %d / %d (%.1f%%)\n",
            n_with_csei, nrow(w0), 100 * n_with_csei / nrow(w0)))

# Build overview row
overview_row <- tibble(
  variable  = "n_with_any_csei",
  n_total   = nrow(w0),
  n_nonmiss = n_with_csei,
  mean      = NA_real_, sd = NA_real_, median = NA_real_,
  p25       = NA_real_, p75 = NA_real_,
  min       = NA_real_, max = NA_real_
)

desc_stats <- bind_rows(overview_row, numeric_stats, site_tab, gender_tab)

write_csv(desc_stats, out_desc)
cat("Saved:", out_desc, "\n")

# ── Final verification ─────────────────────────────────────────────────────────
cat("\n--- Final Verification ---\n")
cat(sprintf("Analytical dataset:  %d rows × %d cols\n", nrow(analytical), ncol(analytical)))
cat(sprintf("All-defs scores:     %d rows × %d cols\n", nrow(scores), ncol(scores)))
cat(sprintf("Descriptive stats:   %d rows × %d cols\n", nrow(desc_stats), ncol(desc_stats)))

# CSEI column count in analytical
n_csei_cols <- length(grep("^csei_", names(analytical)))
n_cov_cols  <- length(grep("^coverage_frac_", names(analytical)))
cat(sprintf("CSEI score columns in analytical: %d\n", n_csei_cols))
cat(sprintf("Coverage columns in analytical:   %d\n", n_cov_cols))

# Non-NA counts for CSEI columns (wave0 rows only)
w0_check <- analytical %>% filter(redcap_event_name == "wave0_arm_1")
for (col in sort(grep("^csei_pca_|^csei_zsum_", names(w0_check), value = TRUE))) {
  n_ok <- sum(!is.na(w0_check[[col]]))
  cat(sprintf("  %-35s  non-NA: %4d / %d\n", col, n_ok, nrow(w0_check)))
}

cat("\n=== 07_merge_analytical.R complete ===\n")
