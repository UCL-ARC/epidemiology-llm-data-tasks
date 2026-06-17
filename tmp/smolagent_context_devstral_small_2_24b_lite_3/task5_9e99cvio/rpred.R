library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to standardize missing value codes
standardize_missing <- function(x) {
  case_when(
    x %in% c(-92, -9) ~ -9,  # Refusal
    x %in% c(-94, -8) ~ -8,  # Don't know / insufficient information
    x %in% c(-97, -7) ~ -7,  # Prefer not to say
    x %in% c(-999, -997, -995, -2) ~ -2,  # Schedule not applicable / script error / information lost
    x %in% c(-99, -91, -3) ~ -3,  # Not asked at the fieldwork stage / not interviewed
    x %in% c(-1) ~ -1,  # Item not applicable
    TRUE ~ x
  )
}

# Derive partnr19 (Age 19)
merged_data <- merged_data %>%
  mutate(partnr19 = case_when(
    W6MarStatYP == 1 ~ 1,  # Single
    W6MarStatYP == 2 ~ 2,  # Married
    W6MarStatYP == 3 ~ 3,  # Separated
    W6MarStatYP == 4 ~ 4,  # Divorced
    W6MarStatYP == 5 ~ 5,  # Widowed
    TRUE ~ standardize_missing(W6MarStatYP)
  ))

# Derive partnr25 (Age 25)
merged_data <- merged_data %>%
  mutate(partnr25 = case_when(
    W8DMARSTAT == 1 ~ 1,  # Single
    W8DMARSTAT == 2 ~ 2,  # Married
    W8DMARSTAT == 3 ~ 3,  # Separated
    W8DMARSTAT == 4 ~ 4,  # Divorced
    W8DMARSTAT == 5 ~ 5,  # Widowed
    W8DMARSTAT == 6 ~ 6,  # Civil Partner
    W8DMARSTAT == 7 ~ 7,  # Separated Civil Partner
    W8DMARSTAT == 8 ~ 8,  # Former Civil Partner
    W8DMARSTAT == 9 ~ 9,  # Surviving Civil Partner
    TRUE ~ standardize_missing(W8DMARSTAT)
  ))

# Derive partnr32 (Age 32)
merged_data <- merged_data %>%
  mutate(partnr32 = case_when(
    W9DMARSTAT == 1 ~ 1,  # Single
    W9DMARSTAT == 2 ~ 2,  # Married
    W9DMARSTAT == 3 ~ 3,  # Divorced
    W9DMARSTAT == 4 ~ 4,  # Legally separated
    W9DMARSTAT == 5 ~ 5,  # Widowed
    W9DMARSTAT == 6 ~ 6,  # Civil Partner
    W9DMARSTAT == 7 ~ 7,  # Former Civil Partner
    W9DMARSTAT == 8 ~ 8,  # Surviving Civil Partner
    TRUE ~ standardize_missing(W9DMARSTAT)
  ))

# Derive partnradu25 (Adult version for Age 25)
merged_data <- merged_data %>%
  mutate(partnradu25 = case_when(
    W8DMARSTAT == 1 ~ 1,  # Single
    W8DMARSTAT == 2 ~ 2,  # Married
    W8DMARSTAT == 3 ~ 3,  # Separated
    W8DMARSTAT == 4 ~ 4,  # Divorced
    W8DMARSTAT == 5 ~ 5,  # Widowed
    W8DMARSTAT == 6 ~ 6,  # Civil Partner
    W8DMARSTAT == 7 ~ 7,  # Separated Civil Partner
    W8DMARSTAT == 8 ~ 8,  # Former Civil Partner
    W8DMARSTAT == 9 ~ 9,  # Surviving Civil Partner
    TRUE ~ standardize_missing(W8DMARSTAT)
  ))

# Derive partnradu32 (Adult version for Age 32)
merged_data <- merged_data %>%
  mutate(partnradu32 = case_when(
    W9DMARSTAT == 1 ~ 1,  # Single
    W9DMARSTAT == 2 ~ 2,  # Married
    W9DMARSTAT == 3 ~ 3,  # Divorced
    W9DMARSTAT == 4 ~ 4,  # Legally separated
    W9DMARSTAT == 5 ~ 5,  # Widowed
    W9DMARSTAT == 6 ~ 6,  # Civil Partner
    W9DMARSTAT == 7 ~ 7,  # Former Civil Partner
    W9DMARSTAT == 8 ~ 8,  # Surviving Civil Partner
    TRUE ~ standardize_missing(W9DMARSTAT)
  ))

# Select only NSID and derived variables
final_data <- merged_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
