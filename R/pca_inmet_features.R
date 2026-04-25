source("R/utils.R")

# ── pca_inmet_features.R ─────────────────────────────────────────────────────
# Feature engineering on INMET daily climate data, then a single joint PCA
# across Porto Alegre and São Paulo using within-city z-scored features.
#
# Rationale:
#   - Per-city seasonal P90/P95 thresholds and heatwave events make extreme-heat
#     features inherently relative (already comparable across cities).
#   - All other features are z-scored within each city before stacking, so the
#     PCA captures shared structure of climate *anomalies* — comparable PC
#     scores across cohorts for downstream mental-health modelling.
#
# Feature groups:
#   - Heat intensity
#   - Heatwave indicators (hw_days sourced from 03_identify_heatwaves.R)
#   - Temperature variability
#   - Moisture / humidity
#   - Precipitation patterns
#   - Atmospheric conditions
#
# Input:
#   data/processed/daily_climate_poa.rds
#   data/processed/daily_climate_sp.rds
#   data/processed/heatwaves_poa.rds
#   data/processed/heatwaves_sp.rds
#
# Output:
#   output/tables/monthly_climate_features.csv      (raw monthly features, both cities)
#   output/tables/pca_inmet_features_loadings.csv   (joint PCA loadings)
#   output/tables/pca_inmet_features_variance.csv   (joint PCA variance explained)
#   output/tables/pca_inmet_features_scores.csv     (PC scores with city + yr_mo)
#   output/figures/pca_features_scree.png
#   output/figures/pca_features_loadings_bars.png
#   output/figures/pca_features_loadings_heatmap.png
#   output/figures/pca_features_biplot.png          (faceted by city)
#   output/figures/pca_features_correlation_circle.png
# ──────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(ggrepel, lib.loc = Sys.getenv("R_LIBS_USER"))

cat("=== Feature engineering + joint PCA on INMET climate ===\n")

fig_dir   <- "output/figures"
table_dir <- "output/tables"
dir.create(fig_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# Heatwave definition pulled from 03_identify_heatwaves.R: tmax > P90, ≥3 days
HW_DEF <- "tmax_p90_d3"

# ── Shared PCA variable definitions ─────────────────────────────────────────
pca_vars <- c(
  # Heat intensity
  "mean_tmax", "max_tmax", "mean_tmean", "mean_apparent",
  # Extreme heat
  "days_above_p90", "days_above_p95", "hw_days",
  "degree_days_p90", "degree_days_p95",
  # Variability
  "sd_tmax", "sd_tmean", "mean_dtr", "sd_dtr", "mean_tsd",
  # Moisture
  "mean_humidity", "min_humidity", "max_humidity",
  "mean_dewpoint", "mean_dewpoint_range",
  # Precipitation
  "total_precip", "dry_days", "max_daily_precip",
  # Atmospheric
  "mean_pressure", "mean_pressure_range",
  "mean_radiation", "mean_radiation_peak",
  "mean_wind", "mean_wind_peak", "mean_gust_peak"
)

nice_names <- c(
  mean_tmax            = "Mean Tmax",
  max_tmax             = "Max Tmax",
  mean_tmean           = "Mean Tmean",
  mean_apparent        = "Mean apparent temp",
  days_above_p90       = "Days > P90",
  days_above_p95       = "Days > P95",
  hw_days              = "Heatwave days",
  degree_days_p90      = "Degree-days (P90)",
  degree_days_p95      = "Degree-days (P95)",
  sd_tmax              = "SD Tmax",
  sd_tmean             = "SD Tmean",
  mean_dtr             = "Diurnal temp range",
  sd_dtr               = "SD of DTR",
  mean_tsd             = "Within-day temp SD",
  mean_humidity        = "Mean humidity",
  min_humidity         = "Min humidity",
  max_humidity         = "Max humidity",
  mean_dewpoint        = "Mean dewpoint",
  mean_dewpoint_range  = "Dewpoint range",
  total_precip         = "Total precip",
  dry_days             = "Dry days",
  max_daily_precip     = "Max daily precip",
  mean_pressure        = "Mean pressure",
  mean_pressure_range  = "Pressure range",
  mean_radiation       = "Mean radiation",
  mean_radiation_peak  = "Peak radiation",
  mean_wind            = "Mean wind speed",
  mean_wind_peak       = "Peak wind speed",
  mean_gust_peak       = "Peak wind gust"
)

var_group <- c(
  mean_tmax = "Heat intensity", max_tmax = "Heat intensity",
  mean_tmean = "Heat intensity", mean_apparent = "Heat intensity",
  days_above_p90 = "Extreme heat", days_above_p95 = "Extreme heat",
  hw_days = "Extreme heat", degree_days_p90 = "Extreme heat",
  degree_days_p95 = "Extreme heat",
  sd_tmax = "Variability", sd_tmean = "Variability",
  mean_dtr = "Variability", sd_dtr = "Variability",
  mean_tsd = "Variability",
  mean_humidity = "Moisture", min_humidity = "Moisture",
  max_humidity = "Moisture",
  mean_dewpoint = "Moisture", mean_dewpoint_range = "Moisture",
  total_precip = "Precipitation", dry_days = "Precipitation",
  max_daily_precip = "Precipitation",
  mean_pressure = "Atmospheric", mean_pressure_range = "Atmospheric",
  mean_radiation = "Atmospheric", mean_radiation_peak = "Atmospheric",
  mean_wind = "Atmospheric", mean_wind_peak = "Atmospheric",
  mean_gust_peak = "Atmospheric"
)

group_colours <- c(
  "Heat intensity" = "#D6604D",
  "Extreme heat"   = "#B2182B",
  "Variability"    = "#F4A582",
  "Moisture"       = "#4393C3",
  "Precipitation"  = "#2166AC",
  "Atmospheric"    = "#878787"
)

city_colours <- c("Porto Alegre" = "#1F78B4", "São Paulo" = "#E31A1C")

# ── Helper: expand heatwave events to a daily TRUE/FALSE flag ───────────────
heatwave_days_from_events <- function(events, dates) {
  if (nrow(events) == 0) return(rep(FALSE, length(dates)))
  hw_dates <- do.call(c, Map(function(s, e) seq(s, e, by = "day"),
                             events$start, events$end))
  dates %in% hw_dates
}

# ── Build monthly features for one city ──────────────────────────────────────
# Heatwave thresholds (P90/P95) computed per city — these are inherently
# relative to each city's seasonal climate, so days_above_pXX and hw_days are
# already comparable across cities without further standardization.
build_monthly_features <- function(daily_path, hw_path, city_label) {
  cat(sprintf("\n--- Building monthly features for %s ---\n", city_label))

  daily <- readRDS(daily_path) %>%
    mutate(
      doy   = yday(date),
      yr    = year(date),
      mo    = month(date),
      yr_mo = paste0(yr, "-", sprintf("%02d", mo)),
      dtr   = tmax_daily - tmin_daily,
      dewpoint_range_daily = dewpoint_max_daily - dewpoint_min_daily,
      pressure_range_daily = pressure_max - pressure_min
    ) %>%
    arrange(date)

  cat("  Daily rows:", nrow(daily), " (", as.character(min(daily$date)),
      "to", as.character(max(daily$date)), ")\n")

  thr_p90 <- compute_seasonal_threshold(daily, "tmax_daily", 0.90)
  thr_p95 <- compute_seasonal_threshold(daily, "tmax_daily", 0.95)

  daily <- daily %>%
    left_join(thr_p90 %>% rename(thr_p90 = threshold), by = "doy") %>%
    left_join(thr_p95 %>% rename(thr_p95 = threshold), by = "doy") %>%
    mutate(
      above_p90  = tmax_daily > thr_p90 & !is.na(tmax_daily),
      above_p95  = tmax_daily > thr_p95 & !is.na(tmax_daily),
      excess_p90 = pmax(tmax_daily - thr_p90, 0, na.rm = TRUE),
      excess_p95 = pmax(tmax_daily - thr_p95, 0, na.rm = TRUE)
    )

  hw_events    <- readRDS(hw_path) %>% filter(hw_def == HW_DEF)
  daily$hw_day <- heatwave_days_from_events(hw_events, daily$date)
  cat(sprintf("  %d heatwave events / %d heatwave days (%s)\n",
              nrow(hw_events), sum(daily$hw_day), HW_DEF))

  monthly <- daily %>%
    group_by(yr, mo, yr_mo) %>%
    summarise(
      n_days     = n(),
      n_long_gap = sum(long_gap, na.rm = TRUE),
      n_obs_mean = mean(n_obs, na.rm = TRUE),

      # --- Heat intensity ---
      mean_tmax       = mean(tmax_daily, na.rm = TRUE),
      max_tmax        = max(tmax_daily, na.rm = TRUE),
      mean_tmean      = mean(tmean_daily, na.rm = TRUE),
      mean_apparent   = mean(apparent_temp, na.rm = TRUE),

      # --- Extreme heat / heatwaves ---
      days_above_p90  = sum(above_p90, na.rm = TRUE),
      days_above_p95  = sum(above_p95, na.rm = TRUE),
      hw_days         = sum(hw_day, na.rm = TRUE),
      mean_excess_p90 = mean(excess_p90[above_p90], na.rm = TRUE),
      degree_days_p90 = sum(excess_p90, na.rm = TRUE),
      mean_excess_p95 = mean(excess_p95[above_p95], na.rm = TRUE),
      degree_days_p95 = sum(excess_p95, na.rm = TRUE),

      # --- Temperature variability ---
      sd_tmax  = sd(tmax_daily, na.rm = TRUE),
      sd_tmean = sd(tmean_daily, na.rm = TRUE),
      mean_dtr = mean(dtr, na.rm = TRUE),
      sd_dtr   = sd(dtr, na.rm = TRUE),
      mean_tsd = mean(tsd_daily, na.rm = TRUE),

      # --- Moisture / humidity ---
      mean_humidity        = mean(humidity_mean, na.rm = TRUE),
      min_humidity         = min(humidity_min, na.rm = TRUE),
      max_humidity         = max(humidity_max, na.rm = TRUE),
      mean_dewpoint        = mean(dewpoint_daily, na.rm = TRUE),
      mean_dewpoint_range  = mean(dewpoint_range_daily, na.rm = TRUE),

      # --- Precipitation ---
      total_precip     = sum(precip_total, na.rm = TRUE),
      dry_days         = sum(precip_total < 1, na.rm = TRUE),
      max_daily_precip = max(precip_total, na.rm = TRUE),

      # --- Atmospheric ---
      mean_pressure        = mean(pressure_mean, na.rm = TRUE),
      mean_pressure_range  = mean(pressure_range_daily, na.rm = TRUE),
      mean_radiation       = mean(radiation_total, na.rm = TRUE),
      mean_radiation_peak  = mean(radiation_max, na.rm = TRUE),
      mean_wind            = mean(wind_mean, na.rm = TRUE),
      mean_wind_peak       = mean(wind_max, na.rm = TRUE),
      mean_gust_peak       = mean(wind_gust_max, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    mutate(
      mean_excess_p90 = ifelse(is.nan(mean_excess_p90), 0, mean_excess_p90),
      mean_excess_p95 = ifelse(is.nan(mean_excess_p95), 0, mean_excess_p95),
      city = city_label
    )

  cat("  Monthly rows:", nrow(monthly), "\n")
  monthly
}

# ── Build features for both cities and stack ─────────────────────────────────
monthly_poa <- build_monthly_features(
  "data/processed/daily_climate_poa.rds",
  "data/processed/heatwaves_poa.rds",
  "Porto Alegre"
)
monthly_sp <- build_monthly_features(
  "data/processed/daily_climate_sp.rds",
  "data/processed/heatwaves_sp.rds",
  "São Paulo"
)

monthly_all <- bind_rows(monthly_poa, monthly_sp) %>%
  select(city, yr, mo, yr_mo, n_days, n_long_gap, n_obs_mean, everything())

write.csv(monthly_all,
          file.path(table_dir, "monthly_climate_features.csv"),
          row.names = FALSE)
cat("Saved:", file.path(table_dir, "monthly_climate_features.csv"), "\n")

# ── Within-city z-score, then drop incomplete rows ──────────────────────────
cat("\nZ-scoring features within each city...\n")
monthly_z <- monthly_all %>%
  group_by(city) %>%
  mutate(across(all_of(pca_vars), ~ as.numeric(scale(.)))) %>%
  ungroup()

df_pca <- monthly_z %>%
  select(city, yr_mo, all_of(pca_vars)) %>%
  drop_na()
cat("Complete months for joint PCA:", nrow(df_pca),
    "(", sum(df_pca$city == "Porto Alegre"), "PoA /",
    sum(df_pca$city == "São Paulo"), "SP)\n")

# ── Joint PCA ────────────────────────────────────────────────────────────────
# Already z-scored within city, so the pooled data has unit variance per
# variable; using center=TRUE, scale.=TRUE is safe and handles any drift.
pca_fit <- prcomp(df_pca[, pca_vars], center = TRUE, scale. = TRUE)

var_imp <- summary(pca_fit)$importance
cat("\nVariance explained (first 6 PCs):\n")
print(round(var_imp[, 1:6], 3))

# ── Save tables ──────────────────────────────────────────────────────────────
loadings_wide <- as.data.frame(pca_fit$rotation) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(label = nice_names[variable], group = var_group[variable]) %>%
  select(variable, label, group, everything())
write.csv(loadings_wide,
          file.path(table_dir, "pca_inmet_features_loadings.csv"),
          row.names = FALSE)

var_df <- data.frame(
  component  = paste0("PC", seq_along(pca_fit$sdev)),
  eigenvalue = pca_fit$sdev^2,
  var_prop   = var_imp["Proportion of Variance", ],
  var_cumul  = var_imp["Cumulative Proportion", ]
) %>% mutate(component = factor(component, levels = component))
write.csv(var_df,
          file.path(table_dir, "pca_inmet_features_variance.csv"),
          row.names = FALSE)

scores_df <- as.data.frame(pca_fit$x) %>%
  mutate(city = df_pca$city, yr_mo = df_pca$yr_mo) %>%
  select(city, yr_mo, everything())
write.csv(scores_df,
          file.path(table_dir, "pca_inmet_features_scores.csv"),
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
  labs(title = "Scree Plot — Joint PCA on INMET Climate Features",
       subtitle = sprintf("%d within-city z-scored variables, Porto Alegre + São Paulo",
                          length(pca_vars)),
       x = NULL, y = "% Variance explained") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "pca_features_scree.png"), p_scree,
       width = 9, height = 5, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_scree.png"), "\n")

# ── Plot 2: Grouped loading bars (first 4 PCs) ──────────────────────────────
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
  labs(title = "Variable Loadings by Component — Joint PCA",
       subtitle = "Within-city z-scored; colour = feature group; bars sorted by loading",
       x = NULL, y = "Loading") +
  theme_minimal(base_size = 9.5) +
  theme(strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        legend.position = "bottom")

ggsave(file.path(fig_dir, "pca_features_loadings_bars.png"), p_bars,
       width = 12, height = 9, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_loadings_bars.png"), "\n")

# ── Plot 3: Heatmap of loadings (first 5 PCs) ───────────────────────────────
load_5 <- loadings_long %>%
  filter(pc %in% paste0("PC", 1:5)) %>%
  mutate(label = factor(label, levels = rev(nice_names)))

p_heat <- ggplot(load_5, aes(x = pc, y = label, fill = loading)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", loading)), size = 2.7) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-0.5, 0.5), name = "Loading") +
  labs(title = "PCA Loadings Heatmap — Joint PCA",
       subtitle = sprintf("%d within-city z-scored monthly features", length(pca_vars)),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 9))

ggsave(file.path(fig_dir, "pca_features_loadings_heatmap.png"), p_heat,
       width = 8, height = 8, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_loadings_heatmap.png"), "\n")

# ── Plot 4: Biplot (PC1 × PC2), faceted by city ─────────────────────────────
scores_plot <- scores_df %>%
  mutate(
    mo = as.integer(sub(".*-", "", yr_mo)),
    season = case_when(
      mo %in% c(12, 1, 2) ~ "Summer",
      mo %in% 3:5         ~ "Autumn",
      mo %in% 6:8         ~ "Winter",
      mo %in% 9:11        ~ "Spring"
    ),
    season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring"))
  )

arrow_scale <- 5
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
             alpha = 0.55, size = 1.8) +
  geom_segment(data = arrows_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               colour = "grey25",
               arrow = arrow(length = unit(0.18, "cm")), linewidth = 0.5) +
  geom_label_repel(data = arrows_df,
                   aes(x = PC1, y = PC2, label = label, fill = group),
                   size = 2.3, fontface = "bold", colour = "white",
                   alpha = 0.9, box.padding = 0.35, max.overlaps = 25,
                   show.legend = FALSE) +
  facet_wrap(~ city) +
  scale_colour_manual(
    values = c(Summer = "#D6604D", Autumn = "#F4A582",
               Winter = "#4393C3", Spring = "#92C5DE"),
    name = "Season"
  ) +
  scale_fill_manual(values = group_colours) +
  labs(title = "Joint PCA Biplot — INMET Climate Features",
       subtitle = "Months by season; arrows = variable loadings (shared across cities)",
       x = paste0("PC1 (", pc1_pct, ")"), y = paste0("PC2 (", pc2_pct, ")")) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 11))

ggsave(file.path(fig_dir, "pca_features_biplot.png"), p_biplot,
       width = 14, height = 8, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_biplot.png"), "\n")

# ── Plot 5: Correlation circle (PC1 × PC2) ──────────────────────────────────
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
  labs(title = "Correlation Circle — PC1 vs PC2 (Joint PCA)",
       subtitle = "Variables close together are correlated; opposite = inversely correlated",
       x = paste0("PC1 (", pc1_pct, ")"), y = paste0("PC2 (", pc2_pct, ")")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "pca_features_correlation_circle.png"), p_circle,
       width = 9, height = 9, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_correlation_circle.png"), "\n")

cat("\nDone!\n")
