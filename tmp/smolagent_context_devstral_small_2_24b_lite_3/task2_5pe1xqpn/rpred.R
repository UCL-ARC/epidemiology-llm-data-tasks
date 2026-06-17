library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define a function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable / script error
    var %in% c(-94, -100) ~ -8,                 # Insufficient information
    var == -92 ~ -9,                            # Refused
    var == -91 ~ -1,                            # Not applicable
    var == -99 ~ -3,                            # Not interviewed
    var == -1 ~ -8,                             # Don't know
    TRUE ~ var
  )
}

# Harmonize ethnicity variables
# Wave 1
merged_data <- merged_data %>%
  mutate(W1ethnic2YP_clean = map_missing(W1ethnic2YP, "wave1"))

# Wave 2
merged_data <- merged_data %>%
  mutate(W2ethnicYP_clean = map_missing(W2ethnicYP, "wave2"))

# Wave 4
merged_data <- merged_data %>%
  mutate(w4ethnic2YP_clean = map_missing(w4ethnic2YP, "wave4"))

# Wave 8
merged_data <- merged_data %>%
  mutate(W8DETHN15_clean = map_missing(W8DETHN15, "wave8"))

# Wave 9
merged_data <- merged_data %>%
  mutate(W9DETHN15_clean = map_missing(W9DETHN15, "wave9"))

# Create consolidated ethnicity variable using earliest-valid-first approach
merged_data <- merged_data %>%
  mutate(eth = coalesce(W1ethnic2YP_clean, W2ethnicYP_clean, w4ethnic2YP_clean, W8DETHN15_clean, W9DETHN15_clean))

# Convert missing values to -3 if still NA
merged_data <- merged_data %>%
  mutate(eth = ifelse(is.na(eth), -3, eth))

# Select only NSID and the final derived variable
final_data <- merged_data %>%
  select(NSID, eth)

# Write the output file
write_csv(final_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"