library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refused
    var %in% c(-8, -8.0, -1, -1.0) ~ -8,  # Don't know / insufficient information
    var %in% c(-997, -997.0, -995, -995.0, -999, -999.0, -998, -998.0, -94, -94.0, -100, -100.0, -97, -97.0) ~ -2,  # Schedule not applicable / script error / information lost
    var %in% c(-91, -91.0) ~ -1,  # Item not applicable
    TRUE ~ var
  )
}

# Function to harmonize housing tenure categories
harmonize_tenure <- function(var, wave) {
  var <- map_missing(var, wave)
  
  case_when(
    var %in% c(1, 1.0) ~ 1,  # Owned outright
    var %in% c(2, 2.0) ~ 2,  # Own, buying with mortgage/loan
    var %in% c(3, 3.0) ~ 3,  # Part rent, part mortgage (shared equity)
    var %in% c(4, 4.0, 5, 5.0, 6, 6.0) ~ 4,  # Rent it (collapsed category)
    var %in% c(7, 7.0) ~ 5,  # Live rent-free
    var %in% c(8, 8.0, 6, 6.0) ~ 6,  # Squatting / Other arrangement
    TRUE ~ var
  )
}

# Create detailed time-varying housing tenure variables for ages 14-20
merged_data <- merged_data %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH %in% c(1, 1.0) ~ 1,
      W1hous12HH %in% c(2, 2.0) ~ 2,
      W1hous12HH %in% c(3, 3.0) ~ 3,
      W1hous12HH %in% c(4, 4.0) ~ 4,
      W1hous12HH %in% c(5, 5.0) ~ 5,
      W1hous12HH %in% c(6, 6.0) ~ 6,
      W1hous12HH %in% c(7, 7.0) ~ 7,
      W1hous12HH %in% c(8, 8.0) ~ 8,
      TRUE ~ map_missing(W1hous12HH, "wave1")
    ),
    hownteen15 = case_when(
      W2Hous12HH %in% c(1, 1.0) ~ 1,
      W2Hous12HH %in% c(2, 2.0) ~ 2,
      W2Hous12HH %in% c(3, 3.0) ~ 3,
      W2Hous12HH %in% c(4, 4.0) ~ 4,
      W2Hous12HH %in% c(5, 5.0) ~ 5,
      W2Hous12HH %in% c(6, 6.0) ~ 6,
      W2Hous12HH %in% c(7, 7.0) ~ 7,
      W2Hous12HH %in% c(8, 8.0) ~ 8,
      TRUE ~ map_missing(W2Hous12HH, "wave2")
    ),
    hownteen16 = case_when(
      W3hous12HH %in% c(1, 1.0) ~ 1,
      W3hous12HH %in% c(2, 2.0) ~ 2,
      W3hous12HH %in% c(3, 3.0) ~ 3,
      W3hous12HH %in% c(4, 4.0) ~ 4,
      W3hous12HH %in% c(5, 5.0) ~ 5,
      W3hous12HH %in% c(6, 6.0) ~ 6,
      W3hous12HH %in% c(7, 7.0) ~ 7,
      W3hous12HH %in% c(8, 8.0) ~ 8,
      TRUE ~ map_missing(W3hous12HH, "wave3")
    ),
    hownteen17 = case_when(
      W4Hous12HH %in% c(1, 1.0) ~ 1,
      W4Hous12HH %in% c(2, 2.0) ~ 2,
      W4Hous12HH %in% c(3, 3.0) ~ 3,
      W4Hous12HH %in% c(4, 4.0) ~ 4,
      W4Hous12HH %in% c(5, 5.0) ~ 5,
      W4Hous12HH %in% c(6, 6.0) ~ 6,
      W4Hous12HH %in% c(7, 7.0) ~ 7,
      W4Hous12HH %in% c(8, 8.0) ~ 8,
      TRUE ~ map_missing(W4Hous12HH, "wave4")
    ),
    hownteen19 = case_when(
      W6Hous12YP %in% c(1, 1.0) ~ 1,
      W6Hous12YP %in% c(2, 2.0) ~ 2,
      W6Hous12YP %in% c(3, 3.0) ~ 3,
      TRUE ~ map_missing(W6Hous12YP, "wave6")
    ),
    hownteen20 = case_when(
      W7Hous12YP %in% c(1, 1.0) ~ 1,
      W7Hous12YP %in% c(2, 2.0) ~ 2,
      W7Hous12YP %in% c(3, 3.0) ~ 3,
      TRUE ~ map_missing(W7Hous12YP, "wave7")
    )
  )

# Create collapsed time-varying housing tenure variables for ages 14-32
merged_data <- merged_data %>%
  mutate(
    hown14 = harmonize_tenure(W1hous12HH, "wave1"),
    hown15 = harmonize_tenure(W2Hous12HH, "wave2"),
    hown16 = harmonize_tenure(W3hous12HH, "wave3"),
    hown17 = harmonize_tenure(W4Hous12HH, "wave4"),
    hown19 = harmonize_tenure(W6Hous12YP, "wave6"),
    hown20 = harmonize_tenure(W7Hous12YP, "wave7"),
    hown25 = harmonize_tenure(W8TENURE, "wave8"),
    hown32 = harmonize_tenure(W9DTENURE, "wave9")
  )

# Select only the ID variable and final derived variables
output_data <- merged_data %>%
  select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen19, hownteen20,
          hown14, hown15, hown16, hown17, hown19, hown20, hown25, hown32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")
