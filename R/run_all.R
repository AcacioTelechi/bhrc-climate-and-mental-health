# R/run_all.R
# Master script: run the full CSEI pipeline from extraction to analytical dataset.

cat("=== CSEI Pipeline ===\n\n")

steps <- c(
  "R/01_extract_inmet.R",
  "R/02_clean_daily_climate.R",
  "R/03_identify_heatwaves.R",
  "R/04_derive_birth_dates.R",
  "R/05_compute_csei_components.R",
  "R/06_build_csei.R",
  "R/07_merge_analytical.R"
)

for (step in steps) {
  cat("\n========================================\n")
  cat("Running:", step, "\n")
  cat("========================================\n\n")
  source(step, local = new.env())
  cat("\n", step, "completed.\n")
}

cat("\n=== Pipeline complete ===\n")
cat("Final dataset: data/analytical/bhrc_csei_analytical.rds\n")
