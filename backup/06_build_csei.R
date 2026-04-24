source("R/utils.R")

# ── 06_build_csei.R ────────────────────────────────────────────────────────────
# Constructs the CSEI via two methods:
#   1. PCA-based CSEI  (primary)  – PC1 from standardised core HW metrics
#   2. Z-score mean CSEI (sensitivity) – simple mean of z-scored metrics
# ──────────────────────────────────────────────────────────────────────────────

cat("=== 06_build_csei.R ===\n")

# ── I/O paths ─────────────────────────────────────────────────────────────────
input_file   <- "data/processed/csei_components.rds"
output_scores  <- "data/processed/csei_scores.rds"
output_loadings <- "output/tables/pca_loadings.csv"
output_corr     <- "output/tables/pca_vs_zscore_correlation.csv"
fig_dir <- "output/figures"

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ── Constants ─────────────────────────────────────────────────────────────────
CORE_METRICS  <- c("hw_count", "hw_mean_excess", "hw_total_days",
                   "hw_degree_days", "hw_mean_humidity", "hw_apparent_excess")
COUNT_METRICS <- c("hw_count", "hw_total_days", "hw_degree_days")
MIN_PART      <- 20
COV_THRESH    <- 0.5
PRIMARY_DEF   <- "tmax_p90_d3"

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading components …\n")
components <- readRDS(input_file)
cat("Rows:", nrow(components), "| Cols:", ncol(components), "\n")
cat("Windows:", paste(unique(components$window), collapse = ", "), "\n")
cat("HW defs:\n"); print(unique(components$hw_def))

# ── PCA / Z-score function ────────────────────────────────────────────────────
build_csei_one <- function(df_combo, window_label, hw_def_label) {
  # 1. Filter to sufficient coverage
  df_filt <- df_combo %>% filter(coverage_frac >= COV_THRESH)

  if (nrow(df_filt) < MIN_PART) {
    message(sprintf("  SKIP %s / %s — only %d participants after coverage filter",
                    window_label, hw_def_label, nrow(df_filt)))
    return(NULL)
  }

  # 2. Extract core metric matrix
  mat <- as.matrix(df_filt[, CORE_METRICS])

  # 3. Impute count/day metrics: no events = 0
  for (m in COUNT_METRICS) {
    col <- which(colnames(mat) == m)
    mat[is.na(mat[, col]), col] <- 0
  }

  # 4. Impute remaining NAs: intensity metrics — no events = 0 excess
  mat[is.na(mat)] <- 0

  # 5. Z-score standardise each column
  col_means <- colMeans(mat)
  col_sds   <- apply(mat, 2, sd)

  z_mat <- mat
  for (j in seq_len(ncol(mat))) {
    if (col_sds[j] > 0) {
      z_mat[, j] <- (mat[, j] - col_means[j]) / col_sds[j]
    } else {
      # 6. Zero-variance column → set to 0 after z-scoring
      z_mat[, j] <- 0
    }
  }

  # 7. PCA (already centred/scaled, so center=FALSE, scale.=FALSE)
  pca_fit <- prcomp(z_mat, center = FALSE, scale. = FALSE)

  # 8. PC1 as csei_pca
  # Sign convention: PC1 should point in the direction of higher heat stress.
  # Strategy: use the sum of loadings across non-zero-variance columns.
  # All 6 core metrics are positive indicators of heat stress, so the sum of
  # PC1 loadings should be non-negative. Flip if the sum is negative.
  nonzero_cols <- which(col_sds > 0)
  if (length(nonzero_cols) > 0) {
    loading_sum <- sum(pca_fit$rotation[nonzero_cols, 1])
    if (loading_sum < 0) {
      pca_fit$rotation[, 1] <- -pca_fit$rotation[, 1]
      pca_fit$x[, 1]        <- -pca_fit$x[, 1]
    }
  }
  pc1_scores <- pca_fit$x[, 1]

  # Variance explained by PC1
  var_exp <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
  pc1_var <- var_exp[1]

  # 9. Z-score mean
  csei_zsum <- rowMeans(z_mat)

  # Assemble result tibble
  result <- df_filt %>%
    select(ext_genid, window, hw_def, coverage_frac) %>%
    mutate(
      csei_pca       = pc1_scores,
      csei_zsum      = csei_zsum,
      pca_var_explained = pc1_var
    )

  # Loadings table (PC1)
  loadings_tbl <- tibble(
    window       = window_label,
    hw_def       = hw_def_label,
    metric       = CORE_METRICS,
    loading      = pca_fit$rotation[, 1],
    var_explained = pc1_var
  )

  # Scree data (all PCs)
  scree_tbl <- tibble(
    pc            = seq_along(var_exp),
    var_explained = var_exp
  )

  list(scores = result, loadings = loadings_tbl, scree = scree_tbl,
       n_part = nrow(df_filt))
}

# ── Main loop ─────────────────────────────────────────────────────────────────
windows  <- setdiff(unique(components$window), "0_5")   # skip 0_5 (0% coverage)
hw_defs  <- unique(components$hw_def)

cat(sprintf("\nProcessing %d windows × %d HW defs …\n", length(windows), length(hw_defs)))

all_scores   <- list()
all_loadings <- list()
all_scree    <- list()   # for primary def only

for (win in windows) {
  for (hwd in hw_defs) {
    df_combo <- components %>%
      filter(window == win, hw_def == hwd)

    res <- build_csei_one(df_combo, win, hwd)

    if (!is.null(res)) {
      key <- paste0(win, "__", hwd)
      all_scores[[key]]   <- res$scores
      all_loadings[[key]] <- res$loadings

      if (hwd == PRIMARY_DEF) {
        all_scree[[win]] <- res$scree
      }

      cat(sprintf("  [%s / %-20s] n=%4d  PC1_var=%.3f\n",
                  win, hwd, res$n_part, res$scores$pca_var_explained[1]))
    }
  }
}

# ── Combine & save scores ──────────────────────────────────────────────────────
cat("\nCombining scores …\n")
csei_scores <- bind_rows(all_scores)
cat("Total score rows:", nrow(csei_scores), "\n")
saveRDS(csei_scores, output_scores)
cat("Saved:", output_scores, "\n")

# ── Save PCA loadings ──────────────────────────────────────────────────────────
pca_loadings <- bind_rows(all_loadings)
write_csv(pca_loadings, output_loadings)
cat("Saved:", output_loadings, "\n")

# ── Scree plots (primary def only, one per window) ────────────────────────────
cat("\nGenerating scree plots …\n")
for (win in names(all_scree)) {
  scree_df <- all_scree[[win]]
  p <- ggplot(scree_df, aes(x = pc, y = var_explained)) +
    geom_col(fill = "#2c7bb6", width = 0.6) +
    geom_line(colour = "#d7191c", linewidth = 0.8) +
    geom_point(colour = "#d7191c", size = 2) +
    scale_x_continuous(breaks = scree_df$pc) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = sprintf("PCA Scree Plot — Window %s | %s", win, PRIMARY_DEF),
      subtitle = sprintf("PC1 variance explained: %.1f%%",
                         scree_df$var_explained[1] * 100),
      x = "Principal Component",
      y = "Variance Explained"
    ) +
    theme_bw(base_size = 12)

  fig_path <- file.path(fig_dir, sprintf("scree_plot_%s_primary.pdf", win))
  ggsave(fig_path, plot = p, width = 6, height = 4)
  cat("Saved:", fig_path, "\n")
}

# ── PCA vs Z-score correlation ────────────────────────────────────────────────
cat("\nComputing PCA vs Z-score Spearman correlations …\n")
corr_tbl <- csei_scores %>%
  group_by(window, hw_def) %>%
  summarize(
    spearman_r = cor(csei_pca, csei_zsum, method = "spearman", use = "complete.obs"),
    n          = n(),
    .groups    = "drop"
  )

write_csv(corr_tbl, output_corr)
cat("Saved:", output_corr, "\n")

cat("\n--- Spearman correlations (PCA vs Z-score) ---\n")
print(corr_tbl, n = Inf)

cat("\n--- Summary of variance explained (PC1) ---\n")
var_summary <- csei_scores %>%
  distinct(window, hw_def, pca_var_explained) %>%
  group_by(window) %>%
  summarize(
    mean_var = mean(pca_var_explained),
    min_var  = min(pca_var_explained),
    max_var  = max(pca_var_explained),
    .groups  = "drop"
  )
print(var_summary)

cat("\n=== 06_build_csei.R complete ===\n")
