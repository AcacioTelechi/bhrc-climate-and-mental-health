source("R/utils.R")

# ── pca_inmet.R ───────────────────────────────────────────────────────────────
# PCA on INMET daily climate variables for Porto Alegre.
# Goal: understand the dimensionality of the climate data and identify
# interpretable components before building composite exposure scores.
# ──────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel, lib.loc = Sys.getenv("R_LIBS_USER"))

cat("=== PCA on INMET climate data ===\n")

fig_dir <- "output/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ── Load & prepare ────────────────────────────────────────────────────────────
climate <- readRDS("data/processed/daily_climate_poa.rds")

# Select continuous climate variables for PCA
pca_vars <- c("tmax_daily", "tmin_daily", "tmean_daily", "dewpoint_daily",
              "humidity_mean", "humidity_min", "pressure_mean",
              "radiation_total", "wind_mean", "precip_total", "apparent_temp")

nice_names <- c(
  tmax_daily      = "Tmax",
  tmin_daily      = "Tmin",
  tmean_daily     = "Tmean",
  dewpoint_daily  = "Dewpoint",
  humidity_mean   = "Humidity (mean)",
  humidity_min    = "Humidity (min)",
  pressure_mean   = "Pressure",
  radiation_total = "Radiation",
  wind_mean       = "Wind speed",
  precip_total    = "Precipitation",
  apparent_temp   = "Apparent temp"
)

# Drop rows with any NA in the PCA variables
df_pca <- climate %>%
  select(date, all_of(pca_vars)) %>%
  drop_na()

cat("Complete cases:", nrow(df_pca), "of", nrow(climate), "days\n")

# ── Run PCA (scaled) ─────────────────────────────────────────────────────────
pca_fit <- prcomp(df_pca[, pca_vars], center = TRUE, scale. = TRUE)

# Summaries
var_explained <- summary(pca_fit)$importance
cat("\nVariance explained:\n")
print(round(var_explained[, 1:6], 3))

# ── Extract loadings ──────────────────────────────────────────────────────────
loadings_df <- as.data.frame(pca_fit$rotation) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(variable_label = nice_names[variable]) %>%
  pivot_longer(cols = starts_with("PC"), names_to = "component",
               values_to = "loading") %>%
  mutate(component = factor(component, levels = paste0("PC", 1:length(pca_vars))))

# Save loadings table
loadings_wide <- as.data.frame(pca_fit$rotation) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(variable_label = nice_names[variable]) %>%
  select(variable, variable_label, everything())

write.csv(loadings_wide, "output/tables/pca_inmet_loadings.csv", row.names = FALSE)
cat("\nSaved: output/tables/pca_inmet_loadings.csv\n")

# Variance explained data frame
var_df <- data.frame(
  component     = paste0("PC", seq_along(pca_fit$sdev)),
  eigenvalue    = pca_fit$sdev^2,
  var_prop      = var_explained["Proportion of Variance", ],
  var_cumul     = var_explained["Cumulative Proportion", ]
) %>%
  mutate(component = factor(component, levels = component))

write.csv(var_df, "output/tables/pca_inmet_variance.csv", row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# PLOTS
# ══════════════════════════════════════════════════════════════════════════════

# ── Plot 1: Scree plot ────────────────────────────────────────────────────────
p_scree <- ggplot(var_df, aes(x = component)) +
  geom_col(aes(y = var_prop * 100), fill = "#4292C6", width = 0.6, alpha = 0.8) +
  geom_line(aes(y = var_cumul * 100, group = 1),
            colour = "#B2182B", linewidth = 0.9) +
  geom_point(aes(y = var_cumul * 100), colour = "#B2182B", size = 2.5) +
  geom_text(aes(y = var_prop * 100, label = sprintf("%.1f%%", var_prop * 100)),
            vjust = -0.5, size = 3) +
  geom_hline(yintercept = c(70, 80, 90), linetype = "dotted",
             colour = "grey60", linewidth = 0.3) +
  scale_y_continuous(
    name = "% Variance explained",
    sec.axis = sec_axis(~ ., name = "Cumulative %"),
    limits = c(0, 105), breaks = seq(0, 100, 10)
  ) +
  labs(
    title    = "Scree Plot \u2014 PCA on INMET Daily Climate Variables",
    subtitle = "Bars = individual; Red line = cumulative variance explained",
    x = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "pca_inmet_scree.png"), p_scree,
       width = 8, height = 5, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_inmet_scree.png"), "\n")


# ── Plot 2: Loading heatmap (first 4 PCs) ────────────────────────────────────
load_4pc <- loadings_df %>%
  filter(component %in% paste0("PC", 1:4))

p_heat <- ggplot(load_4pc,
                 aes(x = component, y = reorder(variable_label, -abs(loading)),
                     fill = loading)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", loading)), size = 3.2) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-0.6, 0.6),
    name = "Loading"
  ) +
  labs(
    title    = "PCA Loadings \u2014 INMET Climate Variables",
    subtitle = "First 4 principal components",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_text(size = 10)
  )

ggsave(file.path(fig_dir, "pca_inmet_loadings_heatmap.png"), p_heat,
       width = 7, height = 6, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_inmet_loadings_heatmap.png"), "\n")


# ── Plot 3: Biplot (PC1 × PC2) ───────────────────────────────────────────────
# Arrow coordinates from loadings, scaled for visibility
scores <- as.data.frame(pca_fit$x[, 1:2])
scores$date <- df_pca$date
scores$month <- as.integer(format(df_pca$date, "%m"))
scores$season <- case_when(
  scores$month %in% c(12, 1, 2) ~ "Summer",
  scores$month %in% 3:5         ~ "Autumn",
  scores$month %in% 6:8         ~ "Winter",
  scores$month %in% 9:11        ~ "Spring"
)
scores$season <- factor(scores$season, levels = c("Summer", "Autumn", "Winter", "Spring"))

# Arrows
arrow_scale <- 4
arrows_df <- data.frame(
  variable_label = nice_names[pca_vars],
  PC1 = pca_fit$rotation[, 1] * arrow_scale,
  PC2 = pca_fit$rotation[, 2] * arrow_scale
)

pc1_pct <- sprintf("%.1f%%", var_df$var_prop[1] * 100)
pc2_pct <- sprintf("%.1f%%", var_df$var_prop[2] * 100)

p_biplot <- ggplot() +
  # Day-level scores, coloured by season
  geom_point(data = scores, aes(x = PC1, y = PC2, colour = season),
             alpha = 0.15, size = 0.6) +
  # Variable arrows
  geom_segment(data = arrows_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey20", linewidth = 0.6) +
  geom_label_repel(data = arrows_df,
                   aes(x = PC1, y = PC2, label = variable_label),
                   size = 3, fontface = "bold",
                   fill = "white", alpha = 0.85,
                   box.padding = 0.4, max.overlaps = 20) +
  scale_colour_manual(
    values = c(Summer = "#D6604D", Autumn = "#F4A582",
               Winter = "#4393C3", Spring = "#92C5DE"),
    name = "Season"
  ) +
  labs(
    title    = "PCA Biplot \u2014 INMET Daily Climate (Porto Alegre)",
    subtitle = "Points = days coloured by season; arrows = variable loadings",
    x = paste0("PC1 (", pc1_pct, ")"),
    y = paste0("PC2 (", pc2_pct, ")")
  ) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

ggsave(file.path(fig_dir, "pca_inmet_biplot.png"), p_biplot,
       width = 9, height = 8, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_inmet_biplot.png"), "\n")


# ── Plot 4: Loading bar plots per PC (top 4) ─────────────────────────────────
load_4pc <- load_4pc %>%
  mutate(
    direction = ifelse(loading >= 0, "positive", "negative"),
    pc_label  = paste0(component, " (",
                       sprintf("%.1f%%", var_df$var_prop[as.integer(sub("PC", "", component))] * 100),
                       ")")
  )

p_bars <- ggplot(load_4pc,
                 aes(x = reorder(variable_label, loading),
                     y = loading, fill = direction)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", loading),
                hjust = ifelse(loading >= 0, -0.15, 1.15)),
            size = 2.8) +
  coord_flip() +
  facet_wrap(~ pc_label, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = c(positive = "#B2182B", negative = "#2166AC")) +
  labs(
    title    = "Variable Loadings on Each Component",
    subtitle = "Red = positive, Blue = negative contribution",
    x = NULL, y = "Loading"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )

ggsave(file.path(fig_dir, "pca_inmet_loadings_bars.png"), p_bars,
       width = 10, height = 7, dpi = 150)
cat("Saved:", file.path(fig_dir, "pca_inmet_loadings_bars.png"), "\n")

cat("\nDone!\n")
