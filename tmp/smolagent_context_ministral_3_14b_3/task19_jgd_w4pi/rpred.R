
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load metadata files
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Check if W8DBMI exists in wave_eight
if (!"W8DBMI" %in% colnames(wave_eight)) {
  stop("W8DBMI variable not found in wave_eight dataset.")
}

# Check if W9DBMI exists in wave_nine
if (!"W9DBMI" %in% colnames(wave_nine)) {
  stop("W9DBMI variable not found in wave_nine dataset.")
}

# Merge datasets using full_join by NSID
merged_data <- full_join(wave_one, wave_four, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3
    x[x == -9] <- -9
    x[x == -8] <- -8
    x[x == -1] <- -1
    return(x)
  } else {
    return(x)
  }
}

# Process W8DBMI (Age 25) and W9DBMI (Age 32)
merged_data <- merged_data %>%
  mutate(
    bmi25 = map_missing_values(W8DBMI),
    bmi32 = map_missing_values(W9DBMI)
  )

# Select only NSID and derived BMI variables
final_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write output to CSV
readr::write_csv(final_data, "data/output/cleaned_data.csv")

# Print confirmation message
message("Output file 'data/output/cleaned_data.csv' has been successfully created.")
