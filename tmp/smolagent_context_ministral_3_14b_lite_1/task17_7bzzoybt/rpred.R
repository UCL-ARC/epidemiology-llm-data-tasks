
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets based on metadata
wave_one_young_person <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two_family_background <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three_family_background <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four_young_person <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9_derived_variables <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Rename IMDRSCORE to wave-specific names before merging
wave_two_family_background <- wave_two_family_background %>%
  rename(imdscore_w2 = IMDRSCORE)

wave_three_family_background <- wave_three_family_background %>%
  rename(imdscore_w3 = IMDRSCORE)

# Merge datasets using full_join by NSID
cleaned_data <- full_join(wave_one_young_person, wave_two_family_background, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave_three_family_background, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave_four_young_person, by = "NSID")
cleaned_data <- full_join(cleaned_data, ns9_derived_variables, by = "NSID")

# Define missing value harmonization for IMDRSCORE (waves 2 and 3)
cleaned_data <- cleaned_data %>%
  mutate(
    # Wave 2 (Age 15) IMDRSCORE
    imd15 = case_when(
      imdscore_w2 == -94.0 ~ -8,  # Insufficient Information
      imdscore_w2 >= -999 & imdscore_w2 <= -1 ~ -3,  # Not asked or schedule not applicable
      !is.na(imdscore_w2) ~ imdscore_w2,
      TRUE ~ NA_real_
    ),
    # Wave 3 (Age 16) IMDRSCORE
    imd16 = case_when(
      imdscore_w3 == -94.0 ~ -8,  # Insufficient Information
      imdscore_w3 >= -999 & imdscore_w3 <= -1 ~ -3,  # Not asked or schedule not applicable
      !is.na(imdscore_w3) ~ imdscore_w3,
      TRUE ~ NA_real_
    )
  )

# Define missing value harmonization for W9DIMDD (Age 32)
cleaned_data <- cleaned_data %>%
  mutate(
    imd32 = case_when(
      W9DIMDD == -8.0 ~ -8,  # Insufficient information
      !is.na(W9DIMDD) ~ W9DIMDD,
      TRUE ~ NA_real_
    )
  )

# Remove raw source variables and keep only final derived variables
cleaned_data <- cleaned_data %>%
  select(NSID, imd15, imd16, imd32)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
