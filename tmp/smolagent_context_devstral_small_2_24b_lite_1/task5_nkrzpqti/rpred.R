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

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define standard missing-value codes
standard_missing <- c(
  `-9` = "Refusal",
  `-8` = "Don't know / insufficient information",
  `-7` = "Prefer not to say",
  `-3` = "Not asked at the fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable"
)

# Harmonize W6MarStatYP (Age 19)
merged_data <- merged_data %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP == 1 ~ 1,  # Single
      W6MarStatYP == 2 ~ 2,  # Married
      W6MarStatYP == 3 ~ 3,  # Separated
      W6MarStatYP == 4 ~ 4,  # Divorced
      W6MarStatYP == 5 ~ 5,  # Widowed
      W6MarStatYP == -92 ~ -9,  # Refused
      W6MarStatYP == -91 ~ -1,  # Not applicable
      W6MarStatYP == -1 ~ -8,  # Don't know
      W6MarStatYP == -97 ~ -7,  # Prefer not to say
      W6MarStatYP == -997 ~ -2,  # Script error
      is.na(W6MarStatYP) ~ -3,  # Not interviewed
      TRUE ~ as.numeric(W6MarStatYP)  # Fallback
    )
  )

# Harmonize W8DMARSTAT (Age 25)
merged_data <- merged_data %>%
  mutate(
    partnr25 = case_when(
      W8DMARSTAT == 1 ~ 1,  # Single
      W8DMARSTAT == 2 ~ 2,  # Married
      W8DMARSTAT == 3 ~ 3,  # Separated
      W8DMARSTAT == 4 ~ 4,  # Divorced
      W8DMARSTAT == 5 ~ 5,  # Widowed
      W8DMARSTAT == 6 ~ 6,  # Civil Partner
      W8DMARSTAT == 7 ~ 7,  # Separated Civil Partner
      W8DMARSTAT == 8 ~ 8,  # Former Civil Partner
      W8DMARSTAT == 9 ~ 9,  # Surviving Civil Partner
      W8DMARSTAT == -9 ~ -9,  # Refused
      W8DMARSTAT == -8 ~ -8,  # Insufficient information
      W8DMARSTAT == -1 ~ -1,  # Not applicable
      is.na(W8DMARSTAT) ~ -3,  # Not interviewed
      TRUE ~ as.numeric(W8DMARSTAT)  # Fallback
    )
  )

# Harmonize W9DMARSTAT (Age 32)
merged_data <- merged_data %>%
  mutate(
    partnr32 = case_when(
      W9DMARSTAT == 1 ~ 1,  # Single
      W9DMARSTAT == 2 ~ 2,  # Married
      W9DMARSTAT == 3 ~ 3,  # Divorced
      W9DMARSTAT == 4 ~ 4,  # Legally separated
      W9DMARSTAT == 5 ~ 5,  # Widowed
      W9DMARSTAT == 6 ~ 6,  # Civil Partner
      W9DMARSTAT == 7 ~ 7,  # Former Civil Partner
      W9DMARSTAT == 8 ~ 8,  # Surviving Civil Partner
      W9DMARSTAT == -9 ~ -9,  # Refused
      W9DMARSTAT == -8 ~ -8,  # Insufficient information
      is.na(W9DMARSTAT) ~ -3,  # Not interviewed
      TRUE ~ as.numeric(W9DMARSTAT)  # Fallback
    )
  )

# Create detailed adult partnership status variables for ages 25 and 32
# For partnradu25 and partnradu32, we will use the same harmonized variables as above
merged_data <- merged_data %>%
  mutate(
    partnradu25 = partnr25,
    partnradu32 = partnr32
  )

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"