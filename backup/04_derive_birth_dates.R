# R/04_derive_birth_dates.R
# Derive participant birth dates from assessment age + assessment date.
# Produces a one-row-per-participant dataset.
# Output: data/processed/participant_birthdates.rds

source("R/utils.R")
library(haven)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
input_path  <- "data/Santoro_climate_BHRC_2025_12_19.rds"
output_path <- "data/processed/participant_birthdates.rds"

# ---------------------------------------------------------------------------
# 1. Read cohort data
# ---------------------------------------------------------------------------
message("Reading cohort data...")
dat <- readRDS(input_path)
message(sprintf("  Input: %d rows x %d columns", nrow(dat), ncol(dat)))

# ---------------------------------------------------------------------------
# 2. Select relevant columns
# ---------------------------------------------------------------------------
dat_sel <- dat %>%
  select(ext_genid, redcap_event_name, site, gender, d_date, age) %>%
  mutate(
    site   = as.numeric(site),
    gender = as.numeric(gender)
  )

# ---------------------------------------------------------------------------
# 3. Filter to rows where both d_date and age are non-NA
# ---------------------------------------------------------------------------
dat_valid <- dat_sel %>%
  filter(!is.na(d_date), !is.na(age))

message(sprintf("  Rows with valid d_date & age: %d", nrow(dat_valid)))

# ---------------------------------------------------------------------------
# 4. Derive estimated birth date
#    birth_date_est = d_date - round(age * 365.25) days
# ---------------------------------------------------------------------------
dat_valid <- dat_valid %>%
  mutate(birth_date_est = d_date - round(age * 365.25))

# ---------------------------------------------------------------------------
# 5. One birth date per participant
#    - Sort so wave0_arm_1 comes first (alphabetical sort achieves this)
#    - Use first (wave0 if available) estimate as the birth date
#    - Compute SD of estimates across waves (in days) for QC
# ---------------------------------------------------------------------------
participant_births <- dat_valid %>%
  arrange(ext_genid, redcap_event_name) %>%
  group_by(ext_genid) %>%
  summarise(
    birth_date       = first(birth_date_est),
    n_estimates      = n(),
    birth_date_sd_days = if (n() > 1) {
      sd(as.numeric(birth_date_est))
    } else {
      NA_real_
    },
    site             = first(site),
    gender           = first(gender),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# 6. Flag participants with inconsistent birth dates (SD > 30 days)
# ---------------------------------------------------------------------------
n_inconsistent <- sum(participant_births$birth_date_sd_days > 30, na.rm = TRUE)
message(sprintf("  Participants with birth date SD > 30 days: %d", n_inconsistent))

# ---------------------------------------------------------------------------
# 7. Add derived columns
#    birth_year: year of estimated birth date
#    city: site==1 -> "porto_alegre", site==2 -> "sao_paulo"
# ---------------------------------------------------------------------------
participant_births <- participant_births %>%
  mutate(
    birth_year = year(birth_date),
    city       = case_when(
      site == 1 ~ "porto_alegre",
      site == 2 ~ "sao_paulo",
      TRUE      ~ NA_character_
    )
  ) %>%
  select(ext_genid, birth_date, n_estimates, birth_date_sd_days,
         site, gender, birth_year, city)

# ---------------------------------------------------------------------------
# 8. Save output
# ---------------------------------------------------------------------------
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(participant_births, output_path)
message(sprintf("Saved: %s", output_path))

# ---------------------------------------------------------------------------
# 9. Verification summary
# ---------------------------------------------------------------------------
message("\n=== Verification ===")
message(sprintf("Participant count:      %d", nrow(participant_births)))
message(sprintf("Birth year range:       %d - %d",
  min(participant_births$birth_year, na.rm = TRUE),
  max(participant_births$birth_year, na.rm = TRUE)))
message("Site distribution:")
print(table(participant_births$city, useNA = "ifany"))
message(sprintf("Inconsistent BD (SD>30): %d", n_inconsistent))
message("\nFirst few rows:")
print(head(participant_births))
