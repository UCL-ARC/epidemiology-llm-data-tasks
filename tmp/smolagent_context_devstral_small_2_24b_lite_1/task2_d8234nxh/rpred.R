library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable / script error / information lost
    var == -94 ~ -8,  # Insufficient information
    var == -92 ~ -9,  # Refused
    var == -91 ~ -1,  # Not applicable
    var == -1 ~ -8,   # Don't know
    var == -99 ~ -3,  # Not interviewed
    var == -100 ~ -7, # Prefer not to say
    var == -97 ~ -7,  # Prefer not to say
    TRUE ~ var
  )
}

# Process ethnicity variables
# Wave 1 (Age 14)
merged_data <- merged_data %>%
  mutate(W1ethnic2YP_clean = map_missing(W1ethnic2YP, "wave1"))

# Wave 2 (Age 15)
merged_data <- merged_data %>%
  mutate(W2ethnicYP_clean = map_missing(W2ethnicYP, "wave2"))

# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(w4ethnic2YP_clean = map_missing(w4ethnic2YP, "wave4"))

# Wave 8 (Age 25)
merged_data <- merged_data %>%
  mutate(W8DETHN15_clean = map_missing(W8DETHN15, "wave8"))

# Wave 9 (Age 32)
merged_data <- merged_data %>%
  mutate(W9DETHN15_clean = map_missing(W9DETHN15, "wave9"))

# Consolidate ethnicity variable using earliest-valid-first approach
merged_data <- merged_data %>%
  mutate(eth = coalesce(W1ethnic2YP_clean, W2ethnicYP_clean, w4ethnic2YP_clean, W8DETHN15_clean, W9DETHN15_clean))

# Convert missing values to standard codes
merged_data <- merged_data %>%
  mutate(eth = ifelse(is.na(eth), -3, eth))

# Select only NSID and the consolidated ethnicity variable
final_data <- merged_data %>%
  select(NSID, eth)

# Write the output file
write_csv(final_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"