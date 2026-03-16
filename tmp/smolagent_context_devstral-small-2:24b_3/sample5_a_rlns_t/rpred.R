library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022_derived_variables <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8_2015_derived, by = "NSID") %>%
  full_join(ns9_2022_derived_variables, by = "NSID")

# Standard missing value codes
standard_missing_codes <- c(-9, -8, -1, -3, -2, -7)

# Harmonize missing value codes for W6MarStatYP
merged_data <- merged_data %>%
  mutate(W6MarStatYP = case_when(
    W6MarStatYP == -997.0 ~ -2,
    W6MarStatYP == -97.0 ~ -9,
    W6MarStatYP == -92.0 ~ -9,
    W6MarStatYP == -91.0 ~ -1,
    W6MarStatYP == -1.0 ~ -8,
    TRUE ~ W6MarStatYP
  ))

# Harmonize missing value codes for W8DMARSTAT
merged_data <- merged_data %>%
  mutate(W8DMARSTAT = case_when(
    W8DMARSTAT == -9.0 ~ -9,
    W8DMARSTAT == -8.0 ~ -8,
    W8DMARSTAT == -1.0 ~ -1,
    TRUE ~ W8DMARSTAT
  ))

# Harmonize missing value codes for W9DMARSTAT
merged_data <- merged_data %>%
  mutate(W9DMARSTAT = case_when(
    W9DMARSTAT == -9.0 ~ -9,
    W9DMARSTAT == -8.0 ~ -8,
    TRUE ~ W9DMARSTAT
  ))

# Create harmonized marital status variables
merged_data <- merged_data %>%
  mutate(
    marstat14 = NA_real_,
    marstat17 = NA_real_,
    marstat19 = W6MarStatYP,
    marstat25 = W8DMARSTAT,
    marstat32 = W9DMARSTAT
  )

# Set factor levels and labels for marital status variables
marstat_labels <- c("-9" = "Refusal", "-8" = "Don't know/insufficient information", "-1" = "Item not applicable", "-3" = "Not asked at the fieldwork stage/participated/interviewed", "-2" = "Schedule not applicable/Script error/information lost", "-7" = "Prefer not to say", "1" = "Single, that is never married", "2" = "Married", "3" = "Separated", "4" = "Divorced", "5" = "Widowed")

merged_data <- merged_data %>%
  mutate(
    marstat19 = factor(marstat19, levels = names(marstat_labels), labels = unlist(marstat_labels)),
    marstat25 = factor(marstat25, levels = names(marstat_labels), labels = unlist(marstat_labels)),
    marstat32 = factor(marstat32, levels = names(marstat_labels), labels = unlist(marstat_labels))
  )

# Save the cleaned data to a single CSV file
write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)