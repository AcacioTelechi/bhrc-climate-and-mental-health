source("R/utils.R")

# ── viz_pca_loadings.R ────────────────────────────────────────────────────────
# Visualises PCA loadings organised BY COMPONENT (variable), showing how each
# metric contributes to PC1 across heatwave definitions and time windows.
# ──────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

fig_dir <- "output/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load PCA loadings ─────────────────────────────────────────────────────────
loadings <- read.csv("output/tables/pca_loadings.csv", stringsAsFactors = FALSE)

# ── Nice labels ───────────────────────────────────────────────────────────────
metric_labels <- c(
  hw_count           = "HW count",
  hw_total_days      = "Total HW days",
  hw_mean_excess     = "Mean excess temp",
  hw_degree_days     = "Degree-days",
  hw_mean_humidity   = "Mean humidity",
  hw_apparent_excess = "Apparent temp excess"
)

window_labels <- c(
  "6_10"       = "6\u201310 months",
  "11_18"      = "11\u201318 months",
  "cumulative" = "Cumulative"
)

loadings <- loadings %>%
  mutate(
    metric_label = factor(metric_labels[metric], levels = metric_labels),
    window_label = factor(window_labels[window], levels = window_labels),
    temp_type    = ifelse(grepl("^apparent", hw_def), "Apparent temp", "Tmax"),
    threshold    = sub(".*_(p\\d+)_.*", "\\1", hw_def),
    threshold    = case_when(
      threshold == "p90"  ~ "90th",
      threshold == "p925" ~ "92.5th",
      threshold == "p95"  ~ "95th"
    ),
    duration = paste0("\u2265", sub(".*_d(\\d+)$", "\\1", hw_def), "d"),
    hw_short = paste0(temp_type, " ", threshold, " ", duration)
  )

# ── Plot 1: Faceted by component — dot plot of loadings ───────────────────────
# Each panel = one variable; x-axis = HW definitions; colour = time window
p1 <- ggplot(loadings,
             aes(x = hw_short, y = loading, colour = window_label)) +
  geom_point(size = 2.8, alpha = 0.85,
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = mean(loadings$loading[loadings$loading > 0]),
             linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  facet_wrap(~ metric_label, ncol = 2, scales = "free_y") +
  scale_colour_brewer(palette = "Set1", name = "Time window") +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "PC1 Loadings by Component",
    subtitle = "Each panel shows one variable\u2019s contribution across HW definitions and windows\nDashed line = grand mean of non-zero loadings",
    x = NULL, y = "Loading on PC1"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 55, hjust = 1, size = 6.5),
    strip.text   = element_text(face = "bold", size = 10),
    legend.position = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )

ggsave(file.path(fig_dir, "pca_loadings_by_component.png"), p1,
       width = 13, height = 10, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_loadings_by_component.png"), "\n")


# ── Plot 2: Radar/polar-style — mean loading per component across all defs ───
avg_by_metric <- loadings %>%
  group_by(metric_label, window_label) %>%
  summarise(mean_loading = mean(loading, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(avg_by_metric,
             aes(x = metric_label, y = mean_loading,
                 fill = window_label, group = window_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.2f", mean_loading)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 2.8) +
  scale_fill_brewer(palette = "Set1", name = "Time window") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Average PC1 Loading per Component",
    subtitle = "Averaged across all 18 heatwave definitions per time window",
    x = NULL, y = "Mean loading on PC1"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "pca_mean_loading_by_component.png"), p2,
       width = 9, height = 5.5, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_mean_loading_by_component.png"), "\n")


# ── Plot 3: Heatmap — component × window, averaged across HW defs ────────────
avg_wide <- loadings %>%
  group_by(metric_label, window_label) %>%
  summarise(mean_loading = mean(loading, na.rm = TRUE),
            sd_loading   = sd(loading, na.rm = TRUE),
            .groups = "drop")

p3 <- ggplot(avg_wide, aes(x = window_label, y = metric_label,
                            fill = mean_loading)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f\n\u00b1%.3f", mean_loading, sd_loading)),
            size = 3.5, lineheight = 0.9) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
    midpoint = 0.35, limits = c(0, 0.5),
    name = "Mean PC1\nloading"
  ) +
  labs(
    title    = "Component Contributions to CSEI (PC1)",
    subtitle = "Mean \u00b1 SD of loadings across 18 heatwave definitions",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_text(size = 10),
    legend.position = "right"
  )

ggsave(file.path(fig_dir, "pca_component_heatmap.png"), p3,
       width = 7, height = 5, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_component_heatmap.png"), "\n")


# ── Plot 4: Variance explained across all definitions ─────────────────────────
var_expl <- loadings %>%
  distinct(window_label, hw_short, var_explained, temp_type, threshold, duration)

p4 <- ggplot(var_expl, aes(x = threshold, y = var_explained * 100,
                           colour = duration, shape = temp_type)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.3)) +
  facet_wrap(~ window_label, ncol = 3) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_colour_brewer(palette = "Dark2", name = "Min. duration") +
  labs(
    title    = "Variance Explained by PC1 Across Heatwave Definitions",
    subtitle = "Higher = more unidimensional (single-component CSEI is adequate)",
    x = "Temperature threshold", y = "% variance explained by PC1",
    shape = "Temperature type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "pca_variance_explained.png"), p4,
       width = 10, height = 5, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_variance_explained.png"), "\n")

cat("\nDone! Check output/figures/ for the plots.\n")
