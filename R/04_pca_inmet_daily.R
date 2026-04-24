source("R/utils.R")

# ── pca_inmet_daily.R ────────────────────────────────────────────────────────
# Daily-resolution joint PCA on INMET climate features for Porto Alegre and
# São Paulo. Uses within-city z-scored variables; per-city heatwave flags
# (above_p90, above_p95, hw_day, excess_p90) are included both as PCA
# inputs (so the basis explicitly carries an extreme-heat axis) and as raw
# side columns in the scores CSV for downstream filtering.
#
# Sibling to pca_inmet_features.R (monthly). Daily PCs can be aggregated
# downstream to whatever timescale the outcome demands (week, month, lag),
# while monthly PCA captures aggregate features (counts, SDs) directly.
#
# Input:
#   data/processed/daily_climate_poa.rds
#   data/processed/daily_climate_sp.rds
#   data/processed/heatwaves_poa.rds
#   data/processed/heatwaves_sp.rds
#
# Output:
#   output/tables/pca_inmet_daily_loadings.csv
#   output/tables/pca_inmet_daily_variance.csv
#   output/tables/pca_inmet_daily_scores.csv     (city, date, flags, PC1..PCk)
#   output/figures/pca_daily_scree.png
#   output/figures/pca_daily_loadings_bars.png
#   output/figures/pca_daily_loadings_heatmap.png
#   output/figures/pca_daily_biplot.png          (faceted by city)
#   output/figures/pca_daily_correlation_circle.png
# ──────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(ggrepel, lib.loc = Sys.getenv("R_LIBS_USER"))

cat("=== Daily joint PCA on INMET climate ===\n")

fig_dir   <- "output/figures"
table_dir <- "output/tables"
dir.create(fig_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

HW_DEF <- "tmax_p90_d3"

# ── Daily PCA variable definitions ──────────────────────────────────────────
pca_vars <- c(
  # Heat intensity
  "tmax_daily", "tmin_daily", "tmean_daily", "apparent_temp",
  # Extreme heat (per-city seasonal thresholds + canonical heatwave flag)
  "above_p90", "above_p95", "hw_day", "excess_p90",
  # Within-day variability
  "tsd_daily", "dtr",
  # Moisture
  "dewpoint_daily", "dewpoint_range_daily",
  "humidity_mean", "humidity_min", "humidity_max",
  # Atmospheric
  "pressure_mean", "pressure_range_daily",
  "radiation_total", "radiation_max",
  "wind_mean", "wind_max", "wind_gust_max",
  # Precipitation
  "precip_total"
)

nice_names <- c(
  tmax_daily            = "Tmax",
  tmin_daily            = "Tmin",
  tmean_daily           = "Tmean",
  apparent_temp         = "Apparent temp",
  above_p90             = "Above P90",
  above_p95             = "Above P95",
  hw_day                = "Heatwave day",
  excess_p90            = "Excess over P90",
  tsd_daily             = "Within-day temp SD",
  dtr                   = "Diurnal temp range",
  dewpoint_daily        = "Dewpoint",
  dewpoint_range_daily  = "Dewpoint range",
  humidity_mean         = "Mean humidity",
  humidity_min          = "Min humidity",
  humidity_max          = "Max humidity",
  pressure_mean         = "Mean pressure",
  pressure_range_daily  = "Pressure range",
  radiation_total       = "Total radiation",
  radiation_max         = "Peak radiation",
  wind_mean             = "Mean wind",
  wind_max              = "Peak wind",
  wind_gust_max         = "Peak gust",
  precip_total          = "Precipitation"
)

var_group <- c(
  tmax_daily = "Heat intensity", tmin_daily = "Heat intensity",
  tmean_daily = "Heat intensity", apparent_temp = "Heat intensity",
  above_p90 = "Extreme heat", above_p95 = "Extreme heat",
  hw_day = "Extreme heat", excess_p90 = "Extreme heat",
  tsd_daily = "Variability", dtr = "Variability",
  dewpoint_daily = "Moisture", dewpoint_range_daily = "Moisture",
  humidity_mean = "Moisture", humidity_min = "Moisture", humidity_max = "Moisture",
  pressure_mean = "Atmospheric", pressure_range_daily = "Atmospheric",
  radiation_total = "Atmospheric", radiation_max = "Atmospheric",
  wind_mean = "Atmospheric", wind_max = "Atmospheric", wind_gust_max = "Atmospheric",
  precip_total = "Precipitation"
)

group_colours <- c(
  "Heat intensity" = "#D6604D",
  "Extreme heat"   = "#B2182B",
  "Variability"    = "#F4A582",
  "Moisture"       = "#4393C3",
  "Precipitation"  = "#2166AC",
  "Atmospheric"    = "#878787"
)

# ── Helper: expand heatwave events to a daily TRUE/FALSE flag ───────────────
heatwave_days_from_events <- function(events, dates) {
  if (nrow(events) == 0) return(rep(FALSE, length(dates)))
  hw_dates <- do.call(c, Map(function(s, e) seq(s, e, by = "day"),
                             events$start, events$end))
  dates %in% hw_dates
}

# ── Build daily feature frame for one city ──────────────────────────────────
build_daily_features <- function(daily_path, hw_path, city_label) {
  cat(sprintf("\n--- Building daily features for %s ---\n", city_label))

  daily <- readRDS(daily_path) %>%
    mutate(
      doy   = yday(date),
      dtr   = tmax_daily - tmin_daily,
      dewpoint_range_daily = dewpoint_max_daily - dewpoint_min_daily,
      pressure_range_daily = pressure_max - pressure_min,
      city  = city_label
    ) %>%
    arrange(date)

  cat("  Daily rows:", nrow(daily), " (", as.character(min(daily$date)),
      "to", as.character(max(daily$date)), ")\n")

  # Per-city seasonal P90/P95 thresholds for the side flags
  thr_p90 <- compute_seasonal_threshold(daily, "tmax_daily", 0.90)
  thr_p95 <- compute_seasonal_threshold(daily, "tmax_daily", 0.95)

  daily <- daily %>%
    left_join(thr_p90 %>% rename(thr_p90 = threshold), by = "doy") %>%
    left_join(thr_p95 %>% rename(thr_p95 = threshold), by = "doy") %>%
    mutate(
      above_p90  = tmax_daily > thr_p90 & !is.na(tmax_daily),
      above_p95  = tmax_daily > thr_p95 & !is.na(tmax_daily),
      excess_p90 = pmax(tmax_daily - thr_p90, 0, na.rm = TRUE)
    )

  hw_events    <- readRDS(hw_path) %>% filter(hw_def == HW_DEF)
  daily$hw_day <- heatwave_days_from_events(hw_events, daily$date)
  cat(sprintf("  %d heatwave events / %d heatwave days (%s)\n",
              nrow(hw_events), sum(daily$hw_day), HW_DEF))

  daily
}

daily_poa <- build_daily_features(
  "data/processed/daily_climate_poa.rds",
  "data/processed/heatwaves_poa.rds",
  "Porto Alegre"
)
daily_sp <- build_daily_features(
  "data/processed/daily_climate_sp.rds",
  "data/processed/heatwaves_sp.rds",
  "São Paulo"
)

daily_all <- bind_rows(daily_poa, daily_sp)
cat("\nTotal stacked daily rows:", nrow(daily_all),
    "(", sum(daily_all$city == "Porto Alegre"), "PoA /",
    sum(daily_all$city == "São Paulo"), "SP)\n")

# Preserve raw heatwave flags before z-scoring — they're now PCA inputs but
# we still want raw versions as side columns in the scores CSV.
daily_all <- daily_all %>%
  mutate(
    above_p90_raw  = as.integer(above_p90),
    above_p95_raw  = as.integer(above_p95),
    hw_day_raw     = as.integer(hw_day),
    excess_p90_raw = excess_p90
  )

# ── Within-city z-score, drop incomplete rows ───────────────────────────────
cat("\nZ-scoring features within each city...\n")
daily_z <- daily_all %>%
  group_by(city) %>%
  mutate(across(all_of(pca_vars), ~ as.numeric(scale(as.numeric(.))))) %>%
  ungroup()

df_pca <- daily_z %>%
  select(city, date, all_of(pca_vars),
         above_p90_raw, above_p95_raw, hw_day_raw, excess_p90_raw) %>%
  drop_na(all_of(pca_vars))

cat("Complete days for joint PCA:", nrow(df_pca),
    "(", sum(df_pca$city == "Porto Alegre"), "PoA /",
    sum(df_pca$city == "São Paulo"), "SP)\n")

# ── PCA ──────────────────────────────────────────────────────────────────────
pca_fit <- prcomp(as.matrix(df_pca[, pca_vars]), center = TRUE, scale. = TRUE)

var_imp <- summary(pca_fit)$importance
cat("\nVariance explained (first 6 PCs):\n")
print(round(var_imp[, 1:min(6, ncol(var_imp))], 3))

# ── Save tables ──────────────────────────────────────────────────────────────
loadings_wide <- as.data.frame(pca_fit$rotation) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(label = nice_names[variable], group = var_group[variable]) %>%
  select(variable, label, group, everything())
write.csv(loadings_wide,
          file.path(table_dir, "pca_inmet_daily_loadings.csv"),
          row.names = FALSE)

var_df <- data.frame(
  component  = paste0("PC", seq_along(pca_fit$sdev)),
  eigenvalue = pca_fit$sdev^2,
  var_prop   = var_imp["Proportion of Variance", ],
  var_cumul  = var_imp["Cumulative Proportion", ]
) %>% mutate(component = factor(component, levels = component))
write.csv(var_df,
          file.path(table_dir, "pca_inmet_daily_variance.csv"),
          row.names = FALSE)

scores_df <- as.data.frame(pca_fit$x) %>%
  mutate(
    city       = df_pca$city,
    date       = df_pca$date,
    above_p90  = as.logical(df_pca$above_p90_raw),
    above_p95  = as.logical(df_pca$above_p95_raw),
    hw_day     = as.logical(df_pca$hw_day_raw),
    excess_p90 = df_pca$excess_p90_raw
  ) %>%
  select(city, date, above_p90, above_p95, hw_day, excess_p90, everything())
write.csv(scores_df,
          file.path(table_dir, "pca_inmet_daily_scores.csv"),
          row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# PLOTS
# ══════════════════════════════════════════════════════════════════════════════
loadings_long <- as.data.frame(pca_fit$rotation) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(label = nice_names[variable], group = var_group[variable]) %>%
  pivot_longer(cols = starts_with("PC"), names_to = "pc", values_to = "loading") %>%
  mutate(pc = factor(pc, levels = paste0("PC", 1:ncol(pca_fit$rotation))))

# ── Plot 1: Scree ────────────────────────────────────────────────────────────
p_scree <- ggplot(var_df, aes(x = component)) +
  geom_col(aes(y = var_prop * 100), fill = "#4292C6", width = 0.6, alpha = 0.8) +
  geom_line(aes(y = var_cumul * 100, group = 1), colour = "#B2182B", linewidth = 0.9) +
  geom_point(aes(y = var_cumul * 100), colour = "#B2182B", size = 2.5) +
  geom_text(aes(y = var_prop * 100, label = sprintf("%.1f%%", var_prop * 100)),
            vjust = -0.5, size = 2.8) +
  geom_hline(yintercept = c(70, 80, 90), linetype = "dotted", colour = "grey60") +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10),
                     sec.axis = sec_axis(~ ., name = "Cumulative %")) +
  labs(title = "Scree Plot — Daily Joint PCA on INMET Climate",
       subtitle = sprintf("%d within-city z-scored daily variables, PoA + SP",
                          length(pca_vars)),
       x = NULL, y = "% Variance explained") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "pca_daily_scree.png"), p_scree,
       width = 9, height = 5, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_daily_scree.png"), "\n")

# ── Plot 2: Loading bars (first 4 PCs) ──────────────────────────────────────
load_4 <- loadings_long %>%
  filter(pc %in% paste0("PC", 1:4)) %>%
  mutate(pc_label = paste0(pc, " (",
                           sprintf("%.1f%%", var_df$var_prop[as.integer(sub("PC", "", pc))] * 100),
                           ")"))

p_bars <- ggplot(load_4,
                 aes(x = reorder(label, loading), y = loading, fill = group)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  coord_flip() +
  facet_wrap(~ pc_label, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = group_colours, name = "Feature group") +
  labs(title = "Variable Loadings by Component — Daily Joint PCA",
       subtitle = "Within-city z-scored daily features",
       x = NULL, y = "Loading") +
  theme_minimal(base_size = 9.5) +
  theme(strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        legend.position = "bottom")

ggsave(file.path(fig_dir, "pca_daily_loadings_bars.png"), p_bars,
       width = 12, height = 8, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_daily_loadings_bars.png"), "\n")

# ── Plot 3: Loadings heatmap (first 5 PCs) ──────────────────────────────────
load_5 <- loadings_long %>%
  filter(pc %in% paste0("PC", 1:5)) %>%
  mutate(label = factor(label, levels = rev(nice_names)))

p_heat <- ggplot(load_5, aes(x = pc, y = label, fill = loading)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", loading)), size = 2.7) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-0.6, 0.6), name = "Loading") +
  labs(title = "PCA Loadings Heatmap — Daily Joint PCA",
       subtitle = sprintf("%d within-city z-scored daily features", length(pca_vars)),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 9))

ggsave(file.path(fig_dir, "pca_daily_loadings_heatmap.png"), p_heat,
       width = 8, height = 7, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_daily_loadings_heatmap.png"), "\n")

# ── Plot 4: Biplot, faceted by city ─────────────────────────────────────────
scores_plot <- scores_df %>%
  mutate(
    mo     = month(date),
    season = case_when(
      mo %in% c(12, 1, 2) ~ "Summer",
      mo %in% 3:5         ~ "Autumn",
      mo %in% 6:8         ~ "Winter",
      mo %in% 9:11        ~ "Spring"
    ),
    season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring"))
  )

arrow_scale <- 4
arrows_df <- data.frame(
  label = nice_names[pca_vars],
  group = var_group[pca_vars],
  PC1   = pca_fit$rotation[pca_vars, 1] * arrow_scale,
  PC2   = pca_fit$rotation[pca_vars, 2] * arrow_scale
)

pc1_pct <- sprintf("%.1f%%", var_df$var_prop[1] * 100)
pc2_pct <- sprintf("%.1f%%", var_df$var_prop[2] * 100)

p_biplot <- ggplot() +
  geom_point(data = scores_plot, aes(x = PC1, y = PC2, colour = season),
             alpha = 0.25, size = 0.6) +
  geom_segment(data = arrows_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               colour = "grey25",
               arrow = arrow(length = unit(0.18, "cm")), linewidth = 0.5) +
  geom_label_repel(data = arrows_df,
                   aes(x = PC1, y = PC2, label = label, fill = group),
                   size = 2.3, fontface = "bold", colour = "white",
                   alpha = 0.9, box.padding = 0.3, max.overlaps = 25,
                   show.legend = FALSE) +
  facet_wrap(~ city) +
  scale_colour_manual(
    values = c(Summer = "#D6604D", Autumn = "#F4A582",
               Winter = "#4393C3", Spring = "#92C5DE"),
    name = "Season"
  ) +
  scale_fill_manual(values = group_colours) +
  labs(title = "Daily Joint PCA Biplot — INMET Climate Features",
       subtitle = "Days by season; arrows = variable loadings (shared across cities)",
       x = paste0("PC1 (", pc1_pct, ")"), y = paste0("PC2 (", pc2_pct, ")")) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 11)) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))

ggsave(file.path(fig_dir, "pca_daily_biplot.png"), p_biplot,
       width = 14, height = 8, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_daily_biplot.png"), "\n")

# ── Plot 5: Correlation circle ──────────────────────────────────────────────
cor_df <- data.frame(
  label = nice_names[pca_vars],
  group = var_group[pca_vars],
  PC1   = pca_fit$rotation[pca_vars, 1] * pca_fit$sdev[1],
  PC2   = pca_fit$rotation[pca_vars, 2] * pca_fit$sdev[2]
)
max_r <- max(sqrt(cor_df$PC1^2 + cor_df$PC2^2))
cor_df$PC1 <- cor_df$PC1 / max_r
cor_df$PC2 <- cor_df$PC2 / max_r

circle <- data.frame(
  x = cos(seq(0, 2 * pi, length.out = 100)),
  y = sin(seq(0, 2 * pi, length.out = 100))
)

p_circle <- ggplot() +
  geom_path(data = circle, aes(x, y), colour = "grey70", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_segment(data = cor_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = group),
               arrow = arrow(length = unit(0.18, "cm")), linewidth = 0.6) +
  geom_label_repel(data = cor_df,
                   aes(x = PC1, y = PC2, label = label, colour = group),
                   size = 2.8, fontface = "bold",
                   box.padding = 0.35, max.overlaps = 25,
                   show.legend = FALSE) +
  scale_colour_manual(values = group_colours, name = "Feature group") +
  coord_fixed(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15)) +
  labs(title = "Correlation Circle — PC1 vs PC2 (Daily Joint PCA)",
       subtitle = "Variables close together are correlated; opposite = inversely correlated",
       x = paste0("PC1 (", pc1_pct, ")"), y = paste0("PC2 (", pc2_pct, ")")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "pca_daily_correlation_circle.png"), p_circle,
       width = 9, height = 9, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_daily_correlation_circle.png"), "\n")

cat("\nDone!\n")
