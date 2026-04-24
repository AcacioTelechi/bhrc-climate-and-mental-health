# R/01_extract_inmet.R
# Extract INMET station A801 (Porto Alegre) and A701 (São Paulo) from nested zip archive
# Output: data/raw/porto_alegre/INMET_A801_YYYY.CSV and data/raw/sao_paulo/INMET_A701_YYYY.CSV

source("R/utils.R")
library(zip)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
inmet_dir <- "data/INMET"
out_pa  <- "data/raw/porto_alegre"
out_sp  <- "data/raw/sao_paulo"

years <- 2010:2025

# ---------------------------------------------------------------------------
# Helper: extract a single station file from a yearly zip into a temp dir,
# then copy to the output destination with a standardised name.
# ---------------------------------------------------------------------------
extract_station_year <- function(year_zip, station_code, out_dir, year) {
  # List files in the zip and find the target station
  contents <- zip::zip_list(year_zip)$filename

  # For 2025 there are two files (partial + full year); prefer the one ending 31-12-YYYY
  matches <- grep(station_code, contents, value = TRUE)
  if (length(matches) == 0) {
    stop(sprintf("No file matching station %s found in %s", station_code, year_zip))
  }

  # Prefer complete-year file (ends with _A_31-12-YYYY.CSV)
  full_year_pat <- sprintf("_A_31-12-%d\\.CSV$", year)
  full_year_match <- grep(full_year_pat, matches, value = TRUE, ignore.case = TRUE)
  chosen <- if (length(full_year_match) > 0) full_year_match[1] else matches[1]

  message(sprintf("  Extracting: %s", chosen))

  # Extract to a temp directory
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  zip::unzip(year_zip, files = chosen, exdir = tmp)

  # The extracted file may be inside a subdirectory (e.g. 2010/INMET_...)
  extracted_path <- file.path(tmp, chosen)

  dest_name <- if (station_code == "A801") {
    sprintf("INMET_A801_%d.CSV", year)
  } else {
    sprintf("INMET_A701_%d.CSV", year)
  }
  dest_path <- file.path(out_dir, dest_name)

  file.copy(extracted_path, dest_path, overwrite = TRUE)
  message(sprintf("  -> Saved: %s", dest_path))
}

# ---------------------------------------------------------------------------
# Extract CSVs from yearly zips (2010-2025)
# ---------------------------------------------------------------------------
message("=== Extracting CSV files from yearly zips ===")

for (yr in years) {
  year_zip <- file.path(inmet_dir, sprintf("%d.zip", yr))
  message(sprintf("\nYear %d:", yr))

  extract_station_year(year_zip, "A801", out_pa, yr)
  extract_station_year(year_zip, "A701", out_sp, yr)
}


# ---------------------------------------------------------------------------
# Verify outputs
# ---------------------------------------------------------------------------
message("\n=== Verification ===")

pa_files <- list.files(out_pa, pattern = "\\.CSV$", full.names = FALSE)
sp_files <- list.files(out_sp, pattern = "\\.CSV$", full.names = FALSE)

message(sprintf("Porto Alegre files (%d):", length(pa_files)))
for (f in sort(pa_files)) message(sprintf("  %s", f))

message(sprintf("\nSao Paulo files (%d):", length(sp_files)))
for (f in sort(sp_files)) message(sprintf("  %s", f))

expected_csv <- length(years)  # one CSV per year
if (length(pa_files) >= expected_csv && length(sp_files) >= expected_csv) {
  message("\nAll expected files extracted successfully.")
} else {
  warning(sprintf(
    "Expected >= %d files per city. Got PA=%d, SP=%d",
    expected_csv, length(pa_files), length(sp_files)
  ))
}
