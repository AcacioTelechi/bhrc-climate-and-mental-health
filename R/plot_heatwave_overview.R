source("R/utils.R")

# ── plot_heatwave_overview.R ────────────────────────────────────────────────
# Four figures comparing heatwave exposure across Porto Alegre and São Paulo:
#   1. Calendar heatmap: year × day-of-year tiles, coloured by °C excess
#      over the city's seasonal P90, faceted by city.
#   2. Rolling 30-day exposure time series: count of P90-exceeding days in
#      the past 30 days, one line per city.
#   3. Monthly PC3 stripes: warming-stripes-style strip per city showing
#      monthly mean of the joint-PCA "anomalous heatwave intensity" axis.
#   4. Excess-P90 densities per 5-year period: side-by-side per city,
#      quantifying the worsening of heat extremes.
#
# Input:
#   output/tables/pca_inmet_daily_features.csv
#
# Output:
#   output/figures/heatwave_calendar.png
#   output/figures/heatwave_rolling30d.png
#   output/figures/pc3_monthly_stripes.png
#   output/figures/excess_p90_by_period.png
# ──────────────────────────────────────────────────────────────────────────────

library(ggplot2)

fig_dir <- "output/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

features <- read.csv("output/tables/pca_inmet_daily_features.csv",
                     stringsAsFactors = FALSE)
features$date      <- as.Date(features$date)
features$above_p90 <- as.logical(features$above_p90)
features$above_p95 <- as.logical(features$above_p95)
features$hw_day    <- as.logical(features$hw_day)

city_colours <- c("Porto Alegre" = "#1F78B4", "São Paulo" = "#E31A1C")

# ── Plot 1: Calendar heatmap ────────────────────────────────────────────────
hw_calendar <- features %>%
  mutate(
    year = year(date),
    doy  = yday(date),
    # Show excess only on P90+ days; non-exceedance days stay light grey.
    intensity = ifelse(above_p90 & !is.na(above_p90), excess_p90, NA_real_)
  )

# Month label positions on a non-leap-year reference
month_breaks <- yday(as.Date(paste0("2023-", sprintf("%02d", 1:12), "-15")))
month_labels <- month.abb

p_cal <- ggplot(hw_calendar, aes(x = doy, y = year, fill = intensity)) +
  geom_tile(colour = NA) +
  # Mark heatwave-run days (≥3 consecutive P90 days) with a thin black border
  geom_tile(data = filter(hw_calendar, hw_day),
            colour = "black", fill = NA, linewidth = 0.15) +
  facet_wrap(~ city, ncol = 1) +
  scale_fill_gradientn(
    colours  = c("#FEE5D9", "#FCBBA1", "#FB6A4A", "#CB181D", "#67000D"),
    na.value = "grey94",
    name     = "Excess °C\nover P90",
    limits   = c(0, NA),
    guide    = guide_colourbar(barheight = unit(4, "cm"))
  ) +
  scale_x_continuous(breaks = month_breaks, labels = month_labels,
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(hw_calendar$year),
                     trans = "reverse", expand = c(0, 0)) +
  labs(
    title    = "Heatwave Calendar — Porto Alegre vs São Paulo, 2010–2025",
    subtitle = "Each tile = one day. Coloured tiles = Tmax above the city's fixed P90; black outline = day inside a ≥3-day heatwave run.",
    caption  = "Per-city fixed P90 thresholds (PoA ≈ 32.9 °C, SP ≈ 32.0 °C) from R/03_identify_heatwaves.R",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid          = element_blank(),
    strip.text          = element_text(face = "bold", size = 12),
    plot.title          = element_text(face = "bold"),
    axis.text.x         = element_text(size = 9),
    axis.text.y         = element_text(size = 8),
    legend.position     = "right",
    panel.spacing.y     = unit(0.6, "lines")
  )

ggsave(file.path(fig_dir, "heatwave_calendar.png"), p_cal,
       width = 14, height = 9, dpi = 150)
cat("Saved:", file.path(fig_dir, "heatwave_calendar.png"), "\n")

# ── Plot 2: Rolling 30-day exposure time series ─────────────────────────────
rolling_df <- features %>%
  select(city, date, n_above_p90_30d, n_above_p95_30d,
         n_hw_days_30d, sum_excess_p90_30d) %>%
  filter(!is.na(n_above_p90_30d))

p_roll <- ggplot(rolling_df, aes(x = date, y = n_above_p90_30d, colour = city)) +
  geom_line(linewidth = 0.4, alpha = 0.85) +
  geom_ribbon(aes(ymin = 0, ymax = n_above_p95_30d, fill = city),
              alpha = 0.18, colour = NA) +
  scale_colour_manual(values = city_colours, name = NULL) +
  scale_fill_manual(values = city_colours, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, NA),
                     expand = c(0, 0)) +
  geom_hline(yintercept = c(3, 7, 14), linetype = "dotted",
             colour = "grey55", linewidth = 0.3) +
  annotate("text", x = as.Date("2010-06-01"), y = c(3, 7, 14),
           label = c("3 d", "7 d", "14 d"),
           hjust = 0, vjust = -0.4, colour = "grey45", size = 3) +
  labs(
    title    = "Rolling 30-day Heat Exposure — Porto Alegre vs São Paulo",
    subtitle = "Solid line: days above P90 in the previous 30. Shaded ribbon: of which were also above P95.",
    caption  = "Per-city fixed P90/P95 thresholds (absolute heat). Higher values = more sustained heat exposure.",
    x = NULL, y = "Days above P90 (last 30 days)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92"),
    plot.title         = element_text(face = "bold")
  )

ggsave(file.path(fig_dir, "heatwave_rolling30d.png"), p_roll,
       width = 13, height = 6, dpi = 150)
cat("Saved:", file.path(fig_dir, "heatwave_rolling30d.png"), "\n")

# ── Plot 3: Monthly PC3 stripes (anomalous-heatwave axis over time) ─────────
# PC3 from the daily joint PCA bundles all heatwave variables (above_p90,
# above_p95, hw_day, excess_p90) with negative loadings on wind — so high
# PC3 = anomalously hot, calm, stagnant. Aggregating to monthly mean smooths
# daily noise into a "warming stripes"-style trend signature.

monthly_pc3 <- features %>%
  mutate(yr_mo = floor_date(date, "month")) %>%
  group_by(city, yr_mo) %>%
  summarise(mean_PC3 = mean(PC3, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(mean_PC3))

pc3_lim <- max(abs(monthly_pc3$mean_PC3), na.rm = TRUE)

p_stripes <- ggplot(monthly_pc3,
                    aes(x = yr_mo, y = city, fill = mean_PC3)) +
  geom_tile(colour = NA) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-pc3_lim, pc3_lim),
    name = "Monthly mean\nPC3 score"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Anomalous Heatwave Intensity (PC3) — Monthly Mean, 2010–2025",
    subtitle = "PC3 = joint-PCA axis: P90/P95/heatwave-day flags + cumulative excess vs calm winds.",
    caption  = "Red = anomalously hot stagnant month; blue = anomalously cool / windy.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid       = element_blank(),
    axis.text.y      = element_text(face = "bold", size = 11),
    axis.text.x      = element_text(size = 9),
    plot.title       = element_text(face = "bold")
  )

ggsave(file.path(fig_dir, "pc3_monthly_stripes.png"), p_stripes,
       width = 14, height = 4, dpi = 150)
cat("Saved:", file.path(fig_dir, "pc3_monthly_stripes.png"), "\n")

# ── Plot 4: Excess-P90 distribution per 5-year period ───────────────────────
# Compares the distribution of heat extremity (°C over P90 on exceedance days)
# across periods to quantify the worsening trend.

excess_df <- features %>%
  filter(above_p90 & !is.na(above_p90)) %>%
  mutate(period = case_when(
    year(date) <= 2014 ~ "2010–2014",
    year(date) <= 2019 ~ "2015–2019",
    TRUE               ~ "2020–2025"
  ))

period_summary <- excess_df %>%
  group_by(city, period) %>%
  summarise(
    n_p90_days  = n(),
    mean_excess = mean(excess_p90),
    p90_excess  = quantile(excess_p90, 0.90),
    max_excess  = max(excess_p90),
    .groups = "drop"
  )

cat("\nExcess °C above P90 — distribution stats per city × period:\n")
print(period_summary)

period_colours <- c("2010–2014" = "#92C5DE",
                    "2015–2019" = "#F4A582",
                    "2020–2025" = "#B2182B")

p_dens <- ggplot(excess_df, aes(x = excess_p90,
                                fill = period, colour = period)) +
  geom_density(alpha = 0.35, linewidth = 0.7) +
  facet_wrap(~ city, ncol = 2) +
  scale_fill_manual(values = period_colours, name = "Period") +
  scale_colour_manual(values = period_colours, name = "Period") +
  scale_x_continuous(breaks = seq(0, 10, 2), expand = c(0.01, 0)) +
  labs(
    title    = "Distribution of Heat Excess Above P90, by 5-year Period",
    subtitle = "Density of °C excess on P90+ days. Rightward shift = hotter extremes when they happen.",
    caption  = "Per-city fixed P90 thresholds (PoA ≈ 32.9 °C, SP ≈ 32.0 °C); non-exceedance days excluded.",
    x = "Excess °C over the day's seasonal P90 threshold",
    y = "Density"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    strip.text         = element_text(face = "bold", size = 12),
    plot.title         = element_text(face = "bold"),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(fig_dir, "excess_p90_by_period.png"), p_dens,
       width = 12, height = 6, dpi = 150)
cat("Saved:", file.path(fig_dir, "excess_p90_by_period.png"), "\n")

cat("\nDone!\n")
