source("R/utils.R")

# ── pca_inmet_features.R ─────────────────────────────────────────────────────
# Feature engineering on INMET daily climate data, then PCA.
# Creates interpretable climate dimensions from raw daily variables:
#   - Heat intensity
#   - Heatwave indicators
#   - Temperature variability
#   - Moisture / humidity
#   - Precipitation patterns
#   - Atmospheric conditions
# Aggregated into monthly summaries to reduce noise.
# ──────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(ggrepel, lib.loc = Sys.getenv("R_LIBS_USER"))

cat("=== Feature engineering + PCA on INMET climate ===\n")

fig_dir <- "output/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────
daily <- readRDS("data/processed/daily_climate_poa.rds") %>%
  mutate(
    doy      = yday(date),
    yr       = year(date),
    mo       = month(date),
    yr_mo    = paste0(yr, "-", sprintf("%02d", mo)),
    dtr      = tmax_daily - tmin_daily  # diurnal temperature range
  )

cat("Daily rows:", nrow(daily), "\n")
cat("Date range:", as.character(range(daily$date)), "\n")

# ── Seasonal thresholds (for heatwave / extreme day flags) ────────────────────
cat("Computing seasonal thresholds...\n")
thr_p90  <- compute_seasonal_threshold(daily, "tmax_daily", 0.90)
thr_p95  <- compute_seasonal_threshold(daily, "tmax_daily", 0.95)

daily <- daily %>%
  left_join(thr_p90 %>% rename(thr_p90 = threshold), by = "doy") %>%
  left_join(thr_p95 %>% rename(thr_p95 = threshold), by = "doy") %>%
  mutate(
    above_p90 = tmax_daily > thr_p90 & !is.na(tmax_daily),
    above_p95 = tmax_daily > thr_p95 & !is.na(tmax_daily),
    excess_p90 = pmax(tmax_daily - thr_p90, 0, na.rm = TRUE)
  )

# ── Heatwave detection (≥3 consecutive days above p90) per month ──────────────
# Tag each day as part of a heatwave run
cat("Identifying heatwave days...\n")
daily <- daily %>% arrange(date)
rle_hw <- rle(daily$above_p90)
hw_flag <- rep(FALSE, nrow(daily))
ends   <- cumsum(rle_hw$lengths)
starts <- ends - rle_hw$lengths + 1
for (i in seq_along(rle_hw$values)) {
  if (rle_hw$values[i] && rle_hw$lengths[i] >= 3) {
    hw_flag[starts[i]:ends[i]] <- TRUE
  }
}
daily$hw_day <- hw_flag

# ── Monthly aggregation ───────────────────────────────────────────────────────
cat("Aggregating to monthly features...\n")

monthly <- daily %>%
  group_by(yr, mo, yr_mo) %>%
  summarise(
    n_days = n(),

    # --- Heat intensity ---
    mean_tmax       = mean(tmax_daily, na.rm = TRUE),
    max_tmax        = max(tmax_daily, na.rm = TRUE),
    mean_tmean      = mean(tmean_daily, na.rm = TRUE),
    mean_apparent   = mean(apparent_temp, na.rm = TRUE),

    # --- Extreme heat / heatwaves ---
    days_above_p90  = sum(above_p90, na.rm = TRUE),
    days_above_p95  = sum(above_p95, na.rm = TRUE),
    hw_days         = sum(hw_day, na.rm = TRUE),
    mean_excess_p90 = mean(excess_p90[above_p90], na.rm = TRUE),  # mean excess on hot days
    degree_days_p90 = sum(excess_p90, na.rm = TRUE),

    # --- Temperature variability ---
    sd_tmax         = sd(tmax_daily, na.rm = TRUE),
    sd_tmean        = sd(tmean_daily, na.rm = TRUE),
    mean_dtr        = mean(dtr, na.rm = TRUE),
    sd_dtr          = sd(dtr, na.rm = TRUE),

    # --- Moisture / humidity ---
    mean_humidity   = mean(humidity_mean, na.rm = TRUE),
    min_humidity    = min(humidity_min, na.rm = TRUE),
    mean_dewpoint   = mean(dewpoint_daily, na.rm = TRUE),

    # --- Precipitation ---
    total_precip    = sum(precip_total, na.rm = TRUE),
    dry_days        = sum(precip_total < 1, na.rm = TRUE),
    max_daily_precip = max(precip_total, na.rm = TRUE),

    # --- Atmospheric ---
    mean_pressure   = mean(pressure_mean, na.rm = TRUE),
    mean_radiation  = mean(radiation_total, na.rm = TRUE),
    mean_wind       = mean(wind_mean, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  # Replace NaN (from mean of empty set) with 0
  mutate(mean_excess_p90 = ifelse(is.nan(mean_excess_p90), 0, mean_excess_p90))

cat("Monthly rows:", nrow(monthly), "\n")

# ── Select PCA variables ─────────────────────────────────────────────────────
pca_vars <- c(
  # Heat intensity
  "mean_tmax", "max_tmax", "mean_tmean", "mean_apparent",
  # Extreme heat
  "days_above_p90", "days_above_p95", "hw_days", "degree_days_p90",
  # Variability
  "sd_tmax", "sd_tmean", "mean_dtr", "sd_dtr",
  # Moisture
  "mean_humidity", "min_humidity", "mean_dewpoint",
  # Precipitation
  "total_precip", "dry_days", "max_daily_precip",
  # Atmospheric
  "mean_pressure", "mean_radiation", "mean_wind"
)

nice_names <- c(
  mean_tmax       = "Mean Tmax",
  max_tmax        = "Max Tmax",
  mean_tmean      = "Mean Tmean",
  mean_apparent   = "Mean apparent temp",
  days_above_p90  = "Days > P90",
  days_above_p95  = "Days > P95",
  hw_days         = "Heatwave days",
  degree_days_p90 = "Degree-days (P90)",
  sd_tmax         = "SD Tmax",
  sd_tmean        = "SD Tmean",
  mean_dtr        = "Diurnal temp range",
  sd_dtr          = "SD of DTR",
  mean_humidity   = "Mean humidity",
  min_humidity    = "Min humidity",
  mean_dewpoint   = "Mean dewpoint",
  total_precip    = "Total precip",
  dry_days        = "Dry days",
  max_daily_precip = "Max daily precip",
  mean_pressure   = "Mean pressure",
  mean_radiation  = "Mean radiation",
  mean_wind       = "Mean wind speed"
)

# Variable grouping for colour-coding
var_group <- c(
  mean_tmax = "Heat intensity", max_tmax = "Heat intensity",
  mean_tmean = "Heat intensity", mean_apparent = "Heat intensity",
  days_above_p90 = "Extreme heat", days_above_p95 = "Extreme heat",
  hw_days = "Extreme heat", degree_days_p90 = "Extreme heat",
  sd_tmax = "Variability", sd_tmean = "Variability",
  mean_dtr = "Variability", sd_dtr = "Variability",
  mean_humidity = "Moisture", min_humidity = "Moisture",
  mean_dewpoint = "Moisture",
  total_precip = "Precipitation", dry_days = "Precipitation",
  max_daily_precip = "Precipitation",
  mean_pressure = "Atmospheric", mean_radiation = "Atmospheric",
  mean_wind = "Atmospheric"
)

# Drop rows with NAs in PCA vars
df_pca <- monthly %>% select(yr_mo, all_of(pca_vars)) %>% drop_na()
cat("Complete months for PCA:", nrow(df_pca), "\n")

# ── Run PCA ──────────────────────────────────────────────────────────────────
pca_fit <- prcomp(df_pca[, pca_vars], center = TRUE, scale. = TRUE)

var_imp <- summary(pca_fit)$importance
cat("\nVariance explained (first 6 PCs):\n")
print(round(var_imp[, 1:6], 3))

# ── Save outputs ──────────────────────────────────────────────────────────────
loadings_wide <- as.data.frame(pca_fit$rotation) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(
    label = nice_names[variable],
    group = var_group[variable]
  ) %>%
  select(variable, label, group, everything())

write.csv(loadings_wide, "output/tables/pca_inmet_features_loadings.csv", row.names = FALSE)

var_df <- data.frame(
  component  = paste0("PC", seq_along(pca_fit$sdev)),
  eigenvalue = pca_fit$sdev^2,
  var_prop   = var_imp["Proportion of Variance", ],
  var_cumul  = var_imp["Cumulative Proportion", ]
) %>% mutate(component = factor(component, levels = component))

write.csv(var_df, "output/tables/pca_inmet_features_variance.csv", row.names = FALSE)
write.csv(df_pca, "output/tables/monthly_climate_features.csv", row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# PLOTS
# ══════════════════════════════════════════════════════════════════════════════

group_colours <- c(
  "Heat intensity" = "#D6604D",
  "Extreme heat"   = "#B2182B",
  "Variability"    = "#F4A582",
  "Moisture"       = "#4393C3",
  "Precipitation"  = "#2166AC",
  "Atmospheric"    = "#878787"
)

# ── Loadings for first 5 PCs ─────────────────────────────────────────────────
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
  labs(title = "Scree Plot \u2014 Engineered Monthly Climate Features",
       subtitle = "21 variables aggregated from daily INMET data (Porto Alegre)",
       x = NULL, y = "% Variance explained") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "pca_features_scree.png"), p_scree,
       width = 9, height = 5, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_scree.png"), "\n")

# ── Plot 2: Grouped loading bars (first 4 PCs) ──────────────────────────────
load_4 <- loadings_long %>% filter(pc %in% paste0("PC", 1:4))
load_4 <- load_4 %>%
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
  labs(title = "Variable Loadings by Component \u2014 Engineered Climate Features",
       subtitle = "Colour = feature group; bars sorted by loading within each PC",
       x = NULL, y = "Loading") +
  theme_minimal(base_size = 9.5) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "pca_features_loadings_bars.png"), p_bars,
       width = 12, height = 9, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_loadings_bars.png"), "\n")

# ── Plot 3: Heatmap of loadings (first 5 PCs) ────────────────────────────────
load_5 <- loadings_long %>%
  filter(pc %in% paste0("PC", 1:5)) %>%
  mutate(label = factor(label, levels = rev(nice_names)))

p_heat <- ggplot(load_5, aes(x = pc, y = label, fill = loading)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", loading)), size = 2.7) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-0.5, 0.5), name = "Loading") +
  labs(title = "PCA Loadings Heatmap \u2014 Engineered Climate Features",
       subtitle = "21 monthly features from INMET daily data",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 9))

ggsave(file.path(fig_dir, "pca_features_loadings_heatmap.png"), p_heat,
       width = 8, height = 8, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_loadings_heatmap.png"), "\n")

# ── Plot 4: Biplot (PC1 × PC2) ──────────────────────────────────────────────
scores <- as.data.frame(pca_fit$x[, 1:2])
scores$yr_mo <- df_pca$yr_mo
scores$mo <- as.integer(sub(".*-", "", scores$yr_mo))
scores$season <- case_when(
  scores$mo %in% c(12, 1, 2) ~ "Summer",
  scores$mo %in% 3:5         ~ "Autumn",
  scores$mo %in% 6:8         ~ "Winter",
  scores$mo %in% 9:11        ~ "Spring"
)
scores$season <- factor(scores$season, levels = c("Summer", "Autumn", "Winter", "Spring"))

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
  geom_point(data = scores, aes(x = PC1, y = PC2, colour = season),
             alpha = 0.5, size = 2) +
  geom_segment(data = arrows_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = group),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.7,
               show.legend = FALSE) +
  geom_label_repel(data = arrows_df,
                   aes(x = PC1, y = PC2, label = label, fill = group),
                   size = 2.5, fontface = "bold", colour = "white",
                   alpha = 0.9, box.padding = 0.4, max.overlaps = 25,
                   show.legend = FALSE) +
  scale_colour_manual(
    values = c(Summer = "#D6604D", Autumn = "#F4A582",
               Winter = "#4393C3", Spring = "#92C5DE",
               group_colours),
    name = "Season"
  ) +
  scale_fill_manual(values = group_colours) +
  labs(title = "PCA Biplot \u2014 Engineered Monthly Climate Features",
       subtitle = "Points = months by season; arrows = variable loadings",
       x = paste0("PC1 (", pc1_pct, ")"), y = paste0("PC2 (", pc2_pct, ")")) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

ggsave(file.path(fig_dir, "pca_features_biplot.png"), p_biplot,
       width = 10, height = 9, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_biplot.png"), "\n")

# ── Plot 5: Correlation circle (PC1 × PC2) ──────────────────────────────────
# Unit circle with variable vectors — shows correlations between vars and PCs
cor_df <- data.frame(
  label = nice_names[pca_vars],
  group = var_group[pca_vars],
  PC1   = pca_fit$rotation[pca_vars, 1] * pca_fit$sdev[1],
  PC2   = pca_fit$rotation[pca_vars, 2] * pca_fit$sdev[2]
)
# Normalise to unit circle
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
  labs(title = "Correlation Circle \u2014 PC1 vs PC2",
       subtitle = "Variables close together are correlated; opposite = inversely correlated",
       x = paste0("PC1 (", pc1_pct, ")"), y = paste0("PC2 (", pc2_pct, ")")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "pca_features_correlation_circle.png"), p_circle,
       width = 9, height = 9, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_features_correlation_circle.png"), "\n")

cat("\nDone!\n")
