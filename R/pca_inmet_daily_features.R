source("R/utils.R")

# ── pca_inmet_daily_features.R ──────────────────────────────────────────────
# Build rolling-window and lagged exposure features from the daily PCA scores.
# Output is a per-(city, date) predictor matrix ready to merge with mental-
# health observations for time-series / distributed-lag modelling.
#
# Rolling windows are right-aligned (lookback only) — window for date D
# covers [D - N + 1, D]. Per-city groups so windows don't bleed across cities.
# Date sequences are reindexed to be complete per city before rolling, so a
# 7-day window means seven calendar days, not seven adjacent rows.
#
# Input:
#   output/tables/pca_inmet_daily_scores.csv   (from R/pca_inmet_daily.R)
#
# Output:
#   output/tables/pca_inmet_daily_features.csv
#     city, date,
#     PC1..PC5 (instantaneous),
#     above_p90, above_p95, hw_day, excess_p90,
#     n_above_p90_{3,7,14,30}d, n_above_p95_{3,7,14,30}d,
#     n_hw_days_{7,14,30}d,
#     sum_excess_p90_{7,14,30}d, sum_excess_p95_{7,14,30}d,
#     mean_PC{1..4}_{7,14,30}d, max_PC{1..4}_{7,14,30}d,
#     PC{1..4}_lag{1,3,7,14}
# ──────────────────────────────────────────────────────────────────────────────

cat("=== Building rolling-window climate exposure features ===\n")

WINDOWS_COUNT <- c(3L, 7L, 14L, 30L)   # for n_above_p90, n_above_p95
WINDOWS_HW    <- c(7L, 14L, 30L)       # for n_hw_days, sum_excess_p90
WINDOWS_PC    <- c(7L, 14L, 30L)       # for mean/max of PC scores
LAGS          <- c(1L, 3L, 7L, 14L)
PCS_TO_AGG    <- paste0("PC", 1:4)

table_dir   <- "output/tables"
scores_path <- file.path(table_dir, "pca_inmet_daily_scores.csv")
out_path    <- file.path(table_dir, "pca_inmet_daily_features.csv")

# ── Right-aligned rolling helpers (pure base R) ─────────────────────────────
# Window of length n ending at position i covers x[i - n + 1 : i].
# Positions before the n-th are filled with NA.
roll_sum_right <- function(x, n) {
  x_filled <- ifelse(is.na(x), 0, x)
  as.numeric(stats::filter(x_filled, rep(1, n), sides = 1))
}

roll_mean_right <- function(x, n) {
  is_present <- as.integer(!is.na(x))
  x_filled   <- ifelse(is.na(x), 0, x)
  sums   <- as.numeric(stats::filter(x_filled,   rep(1, n), sides = 1))
  counts <- as.numeric(stats::filter(is_present, rep(1, n), sides = 1))
  ifelse(!is.na(counts) & counts > 0, sums / counts, NA_real_)
}

roll_max_right <- function(x, n) {
  L <- length(x)
  out <- rep(NA_real_, L)
  if (L < n) return(out)
  m <- embed(x, n)  # rows align with positions n..L
  vals <- apply(m, 1, function(w) if (all(is.na(w))) NA_real_ else max(w, na.rm = TRUE))
  out[n:L] <- vals
  out
}

# ── Load PCA scores ─────────────────────────────────────────────────────────
scores <- read.csv(scores_path, stringsAsFactors = FALSE)
scores$date <- as.Date(scores$date)
# CSV may have parsed booleans as "TRUE"/"FALSE" strings or logical — coerce.
for (col in c("above_p90", "above_p95", "hw_day")) {
  scores[[col]] <- as.logical(scores[[col]])
}
cat(sprintf("Loaded %d rows (%d PoA / %d SP)\n",
            nrow(scores),
            sum(scores$city == "Porto Alegre"),
            sum(scores$city == "São Paulo")))

# ── Build features per city ──────────────────────────────────────────────────
build_features_for_city <- function(df) {
  df <- df %>% arrange(date)

  # Reindex to complete date sequence so rolling windows are calendar-aligned.
  # (group_modify strips the grouping column from .x and re-adds it on bind.)
  full_dates <- tibble(date = seq(min(df$date), max(df$date), by = "day"))
  df <- full_dates %>% left_join(df, by = "date") %>% arrange(date)

  # 1) Counts of P90/P95 days
  for (n in WINDOWS_COUNT) {
    df[[paste0("n_above_p90_", n, "d")]] <- roll_sum_right(as.integer(df$above_p90), n)
    df[[paste0("n_above_p95_", n, "d")]] <- roll_sum_right(as.integer(df$above_p95), n)
  }

  # 2) Heatwave-day counts and cumulative excess heat (over P90 and P95)
  for (n in WINDOWS_HW) {
    df[[paste0("n_hw_days_", n, "d")]]      <- roll_sum_right(as.integer(df$hw_day), n)
    df[[paste0("sum_excess_p90_", n, "d")]] <- roll_sum_right(df$excess_p90, n)
    df[[paste0("sum_excess_p95_", n, "d")]] <- roll_sum_right(df$excess_p95, n)
  }

  # 3) Rolling mean/max of selected PCs
  for (pc in PCS_TO_AGG) {
    for (n in WINDOWS_PC) {
      df[[paste0("mean_", pc, "_", n, "d")]] <- roll_mean_right(df[[pc]], n)
      df[[paste0("max_",  pc, "_", n, "d")]] <- roll_max_right(df[[pc]], n)
    }
  }

  # 4) Lagged PCs
  for (pc in PCS_TO_AGG) {
    for (k in LAGS) {
      df[[paste0(pc, "_lag", k)]] <- dplyr::lag(df[[pc]], n = k)
    }
  }

  df
}

cat("Computing features per city...\n")
features <- scores %>%
  group_by(city) %>%
  group_modify(~ build_features_for_city(.x)) %>%
  ungroup()

# ── Reorder columns: identifiers, instantaneous, derived ────────────────────
key_cols  <- c("city", "date")
instant   <- intersect(c(paste0("PC", 1:5),
                         "above_p90", "above_p95", "hw_day",
                         "excess_p90", "excess_p95"),
                       names(features))
derived   <- setdiff(names(features), c(key_cols, instant))
features  <- features[, c(key_cols, instant, derived)]

write.csv(features, out_path, row.names = FALSE)

cat(sprintf("Saved: %s  (%d rows × %d cols)\n",
            out_path, nrow(features), ncol(features)))

# ── Quick coverage summary ──────────────────────────────────────────────────
cat("\n--- Feature coverage on a representative summer day (2024-01-15) ---\n")
demo <- features %>%
  filter(date == as.Date("2024-01-15")) %>%
  select(city, date, above_p90, above_p95, hw_day, excess_p90,
         starts_with("n_above_p90_"), starts_with("n_hw_days_"),
         starts_with("sum_excess_p90_"))
print(demo)

cat("\n--- Column groups ---\n")
cat("  Counts:    ", paste(grep("^n_above_|^n_hw_", names(features), value = TRUE), collapse = ", "), "\n")
cat("  Cum excess:", paste(grep("^sum_excess_",     names(features), value = TRUE), collapse = ", "), "\n")
cat("  PC means:  ", paste(grep("^mean_PC",         names(features), value = TRUE), collapse = ", "), "\n")
cat("  PC maxes:  ", paste(grep("^max_PC",          names(features), value = TRUE), collapse = ", "), "\n")
cat("  PC lags:   ", paste(grep("_lag\\d+$",        names(features), value = TRUE), collapse = ", "), "\n")

cat("\nDone!\n")
