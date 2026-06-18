library(readr)
library(dplyr)
library(haven)
library(labelled)

# Load the datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_2015, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Function to standardize missing values
standardize_missing <- function(var) {
  case_when(
    var %in% c(-9, -92) ~ -9,  # Refusal
    var %in% c(-8, -94) ~ -8,  # Don't know / insufficient information
    var %in% c(-7, -97) ~ -7,  # Prefer not to say
    var %in% c(-3, -99) ~ -3,  # Not asked at the fieldwork stage / not interviewed
    var %in% c(-2, -999, -998, -997, -995) ~ -2,  # Schedule not applicable / script error / information lost
    var %in% c(-1, -91) ~ -1,  # Item not applicable
    TRUE ~ var
  )
}

# Derive bmi25 from W8DBMI
merged_data <- merged_data %>%
  mutate(bmi25 = standardize_missing(W8DBMI))

# Derive bmi32 from W9DBMI
merged_data <- merged_data %>%
  mutate(bmi32 = standardize_missing(W9DBMI))

# Select only the ID and derived variables
output_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print a summary to verify
cat("Output data summary:\n")
print(summary(output_data))