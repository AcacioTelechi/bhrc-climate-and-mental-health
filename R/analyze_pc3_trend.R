source("R/utils.R")
library(ggplot2)

# ── analyze_pc3_trend.R ─────────────────────────────────────────────────────
# Quantify whether the recent (2010–2025) heat intensification in PoA and SP
# shows up specifically in the stagnation-heatwave dimension (PC3) or just
# in absolute warmth (PC1).
#
# PC3 in the daily joint PCA loads negatively on heatwave variables and
# positively on wind, so we work with `stagnation = -PC3` (positive = more
# stagnation: severe heat + calm air).
#
# Output:
#   output/figures/pc3_stagnation_trend.png
#   console: per-city linear trend stats for stagnation and PC1
# ──────────────────────────────────────────────────────────────────────────────

scores <- read.csv("output/tables/pca_inmet_daily_scores.csv",
                   stringsAsFactors = FALSE)
scores$date <- as.Date(scores$date)
scores$stagnation <- -scores$PC3   # sign-flip: positive = stagnation heatwave
scores$abs_warm   <- -scores$PC1   # sign-flip: positive = hot sunny day
scores$year       <- year(scores$date)

# Drop the partial last year if it lacks summer coverage. 2025 is full here,
# so include all years.
cat("Year range:", range(scores$year), "\n")
cat("Days per year per city (last 5 years):\n")
print(scores %>%
        filter(year >= 2021) %>%
        count(city, year))

# ── Annual summaries per city ───────────────────────────────────────────────
annual <- scores %>%
  group_by(city, year) %>%
  summarise(
    n_days            = n(),
    mean_stagnation   = mean(stagnation, na.rm = TRUE),
    p90_stagnation    = quantile(stagnation, 0.90, na.rm = TRUE),
    max_stagnation    = max(stagnation,    na.rm = TRUE),
    n_extreme_days    = sum(stagnation > 2,  na.rm = TRUE),  # > 2 SD: severe stagnation
    mean_abs_warm     = mean(abs_warm, na.rm = TRUE),
    n_hot_days        = sum(abs_warm > 2, na.rm = TRUE),
    .groups = "drop"
  )

# ── Per-city linear trends (slope per decade) ───────────────────────────────
fit_trend <- function(df, var) {
  m <- lm(reformulate("year", var), data = df)
  ci <- confint(m, "year")
  data.frame(
    slope_per_decade = unname(coef(m)["year"]) * 10,
    ci_lo = ci[1, 1] * 10,
    ci_hi = ci[1, 2] * 10,
    p_value = summary(m)$coefficients["year", 4]
  )
}

trend_stats <- scores %>%
  group_by(city) %>%
  group_modify(~ {
    rbind(
      cbind(metric = "stagnation (-PC3)", fit_trend(.x, "stagnation")),
      cbind(metric = "abs_warm   (-PC1)", fit_trend(.x, "abs_warm"))
    )
  }) %>%
  ungroup()

cat("\n=== Linear trend per city (slope = SD-units per decade, daily data) ===\n")
trend_stats_print <- trend_stats %>%
  mutate(across(c(slope_per_decade, ci_lo, ci_hi), ~ round(., 3)),
         p_value = signif(p_value, 2))
print(trend_stats_print, row.names = FALSE)

cat("\n=== Annual extreme-stagnation day counts (stagnation > 2 SD) ===\n")
extreme_summary <- annual %>%
  select(city, year, n_extreme_days, n_hot_days) %>%
  arrange(city, year)
# Show first/last 4 years per city for trend at a glance
for (c in unique(extreme_summary$city)) {
  cat(sprintf("\n%s — extreme-stagnation days per year:\n", c))
  d <- extreme_summary[extreme_summary$city == c, ]
  print(rbind(head(d, 4), tail(d, 4)), row.names = FALSE)
}

# ── Plot: annual trends in stagnation and absolute warmth ───────────────────
city_colours <- c("Porto Alegre" = "#1F78B4", "São Paulo" = "#E31A1C")

# Long format for facet
plot_df <- annual %>%
  select(city, year, mean_stagnation, mean_abs_warm,
         n_extreme_days, n_hot_days) %>%
  tidyr::pivot_longer(
    cols = c(mean_stagnation, mean_abs_warm,
             n_extreme_days, n_hot_days),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(
    panel = case_when(
      metric == "mean_stagnation" ~ "Mean daily PC3 stagnation\n(-PC3, SD units)",
      metric == "mean_abs_warm"   ~ "Mean daily PC1 warmth\n(-PC1, SD units)",
      metric == "n_extreme_days"  ~ "Days with stagnation > 2 SD\n(severe stagnation events)",
      metric == "n_hot_days"      ~ "Days with warmth > 2 SD\n(absolute hot days)"
    ),
    panel = factor(panel, levels = c(
      "Mean daily PC1 warmth\n(-PC1, SD units)",
      "Mean daily PC3 stagnation\n(-PC3, SD units)",
      "Days with warmth > 2 SD\n(absolute hot days)",
      "Days with stagnation > 2 SD\n(severe stagnation events)"
    ))
  )

p <- ggplot(plot_df, aes(x = year, y = value, colour = city)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.6,
              alpha = 0.15, formula = y ~ x) +
  facet_wrap(~ panel, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = city_colours, name = NULL) +
  scale_fill_manual(values = city_colours, guide = "none") +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  labs(
    title    = "Heat Intensification 2010–2025: Absolute Warmth vs Stagnation Heatwaves",
    subtitle = "Top: daily-mean PC scores per year. Bottom: annual count of extreme days (> 2 SD).",
    caption  = "PC1 = absolute hot day; PC3 stagnation = severe heat + calm wind. Bands = 95% linear-trend CI.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    strip.text      = element_text(face = "bold", size = 10),
    plot.title      = element_text(face = "bold"),
    panel.spacing   = unit(1, "lines")
  )

fig_path <- "output/figures/pc3_stagnation_trend.png"
ggsave(fig_path, p, width = 12, height = 8, dpi = 150)
cat("\nSaved:", fig_path, "\n")
