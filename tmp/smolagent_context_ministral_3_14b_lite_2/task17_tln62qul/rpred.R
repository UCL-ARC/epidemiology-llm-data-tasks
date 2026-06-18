
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths and names
file_paths <- list(
  wave_one_lsype_young_person_2020 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two_lsype_family_background_2020 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three_lsype_family_background_2020 = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four_lsype_young_person_2020 = "data/input/wave_four_lsype_young_person_2020.tab",
  ns9_2022_derived_variables = "data/input/ns9_2022_derived_variables.tab"
)

# Load all datasets
wave_one_data <- readr::read_delim(file_paths$wave_one_lsype_young_person_2020, delim = "\t")
wave_two_data <- readr::read_delim(file_paths$wave_two_lsype_family_background_2020, delim = "\t")
wave_three_data <- readr::read_delim(file_paths$wave_three_lsype_family_background_2020, delim = "\t")
wave_four_data <- readr::read_delim(file_paths$wave_four_lsype_young_person_2020, delim = "\t")
ns9_data <- readr::read_delim(file_paths$ns9_2022_derived_variables, delim = "\t")

# Merge datasets by NSID to ensure full cohort frame is preserved
merged_data <- full_join(wave_one_data, wave_two_data, by = "NSID") %>%
  full_join(wave_three_data, by = "NSID") %>%
  full_join(wave_four_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

# Extract IMDRSCORE from wave_two_data (age 15) and apply missing value harmonization
wave_two_imd <- wave_two_data %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd15_raw = IMDRSCORE)

# Extract IMDRSCORE from wave_three_data (age 16) and apply missing value harmonization
wave_three_imd <- wave_three_data %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd16_raw = IMDRSCORE)

# Merge the extracted IMDRSCORE variables into the main dataset
merged_data <- merged_data %>%
  left_join(wave_two_imd, by = "NSID") %>%
  left_join(wave_three_imd, by = "NSID")

# Process imd15 (age 15) from wave_two_imd
merged_data <- merged_data %>%
  mutate(
    imd15 = case_when(
      imd15_raw %in% c(-999.0, -998.0, -997.0, -995.0) ~ -2,
      imd15_raw == -94.0 ~ -8,
      imd15_raw == -92.0 ~ -9,
      imd15_raw == -91.0 ~ -1,
      imd15_raw == -99.0 ~ -3,
      !is.na(imd15_raw) ~ imd15_raw,
      TRUE ~ -3
    )
  )

# Process imd16 (age 16) from wave_three_imd
merged_data <- merged_data %>%
  mutate(
    imd16 = case_when(
      imd16_raw %in% c(-999.0, -998.0, -997.0, -995.0) ~ -2,
      imd16_raw == -94.0 ~ -8,
      imd16_raw == -92.0 ~ -9,
      imd16_raw == -91.0 ~ -1,
      imd16_raw == -99.0 ~ -3,
      !is.na(imd16_raw) ~ imd16_raw,
      TRUE ~ -3
    )
  )

# Process imd32 (age 32) from W9DIMDD
merged_data <- merged_data %>%
  mutate(
    imd32 = case_when(
      W9DIMDD == -8.0 ~ -8,
      !is.na(W9DIMDD) ~ W9DIMDD,
      TRUE ~ -3
    )
  )

# Remove only the intermediate raw variables
merged_data <- merged_data %>%
  select(-imd15_raw, -imd16_raw)

# Write the cleaned data to CSV
readr::write_csv(merged_data, "data/output/cleaned_data.csv")
