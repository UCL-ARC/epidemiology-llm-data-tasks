library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define filenames from metadata
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

# Load files
load_tab <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(.default = 'd'), trim_ws = TRUE)
}

# Since NSID is a string, we need to ensure it's read as such
load_tab_fixed <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(NSID = col_character(), .default = 'd'), trim_ws = TRUE)
}

data1 <- load_tab_fixed('wave_one_lsype_family_background_2020.tab')
data2 <- load_tab_fixed('wave_two_lsype_family_background_2020.tab')
data3 <- load_tab_fixed('wave_three_lsype_family_background_2020.tab')
data4 <- load_tab_fixed('wave_four_lsype_family_background_2020.tab')

# Merge datasets
full_df <- data1 %>%
  full_join(data2, by = 'NSID') %>%
  full_join(data3, by = 'NSID') %>%
  full_join(data4, by = 'NSID')

# Define Banding Logic for Ages 14-15
# 1: Up to 49, 2: 50-99, 3: 100-199, 4: 200-299, 5: 300-399, 6: 400-499, 7: 500-599, 8: 600-699, 9: 700-799, 10: 800-899, 11: 900-999, 12: 1000+
band_income <- function(x) {
  case_when(
    x < 0 ~ x, # Preserve missing codes
    x < 50 ~ 1,
    x < 100 ~ 2,
    x < 200 ~ 3,
    x < 300 ~ 4,
    x < 400 ~ 5,
    x < 500 ~ 6,
    x < 600 ~ 7,
    x < 700 ~ 8,
    x < 800 ~ 9,
    x < 900 ~ 10,
    x < 1000 ~ 11,
    TRUE ~ 12
  )
}

# Missing Value Mapping Function
map_missing <- function(val, wave) {
  # Specifics for 14-15
  if (wave %in% c(14, 15)) {
    case_when(
      val == -3 ~ -1, # Not yet paid -> Not applicable
      val == -1 ~ -8, # Don't know -> Don't know
      val == -992 ~ -9, # Refused -> Refusal
      val == -92 ~ -9,  # Refused -> Refusal
      val == -91 ~ -1,  # Not applicable -> Not applicable
      val == -99 ~ -3,  # Not interviewed -> Not asked
      val == -94 ~ -8,  # Insufficient info -> Don't know
      val == -999 ~ -2, # Missing in error -> Information lost
      TRUE ~ val
    )
  } else if (wave == 16) {
    case_when(
      val == -92 ~ -9,  # Refused
      val == -1 ~ -8,   # Don't know
      val == -99 ~ -3,  # Not interviewed
      TRUE ~ val
    )
  } else if (wave == 17) {
    case_when(
      val == -996 ~ -3, # No parent in HH -> Not asked
      val == -92 ~ -9,  # Refused
      val == -1 ~ -8,   # Don't know
      val == -99 ~ -3,  # Not interviewed
      TRUE ~ val
    )
  }
}

# Process Variables
# Age 14
raw14 <- full_df$W1GrsswkHH
proc14_missing <- map_missing(raw14, 14)
proc14_missing[is.na(proc14_missing)] <- -3
inc14_cont <- proc14_missing
inc14_band <- band_income(proc14_missing)

# Age 15
raw15 <- full_df$W2GrsswkHH
proc15_missing <- map_missing(raw15, 15)
proc15_missing[is.na(proc15_missing)] <- -3
inc15_cont <- proc15_missing
inc15_band <- band_income(proc15_missing)

# Age 16
raw16 <- full_df$W3incestw
proc16_missing <- map_missing(raw16, 16)
proc16_missing[is.na(proc16_missing)] <- -3
inc16_band <- proc16_missing

# Age 17
raw17 <- full_df$w4IncEstW
proc17_missing <- map_missing(raw17, 17)
proc17_missing[is.na(proc17_missing)] <- -3
inc17_band <- proc17_missing

# Create Final Dataframe
final_df <- data.frame(
  NSID = full_df$NSID,
  inc14 = inc14_cont,
  inc15 = inc15_cont,
  inc14_band = inc14_band,
  inc15_band = inc15_band,
  inc16_band = inc16_band,
  inc17_band = inc17_band
)

# Factor labels for banded variables
band_labels <- c(
  "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say",
  "-3" = "Not asked", "-2" = "Information lost", "-1" = "Not applicable",
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more"
)

# Apply factor labels
apply_labels <- function(x) {
  factor(x, levels = as.numeric(names(band_labels)), labels = band_labels)
}

final_df$inc14_band <- apply_labels(final_df$inc14_band)
final_df$inc15_band <- apply_labels(final_df$inc15_band)
final_df$inc16_band <- apply_labels(final_df$inc16_band)
final_df$inc17_band <- apply_labels(final_df$inc17_band)

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')
