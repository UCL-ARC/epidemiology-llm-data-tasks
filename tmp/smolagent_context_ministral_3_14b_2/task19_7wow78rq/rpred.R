# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave_one_lsype_young_person_2020 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four_lsype_young_person_2020 = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_2015_derived = "data/input/ns8_2015_derived.tab",
  ns9_2022_derived_variables = "data/input/ns9_2022_derived_variables.tab"
)

# Load each file explicitly
wave_one_data <- readr::read_delim(files$wave_one_lsype_young_person_2020, delim = "\t")
wave_four_data <- readr::read_delim(files$wave_four_lsype_young_person_2020, delim = "\t")
ns8_data <- readr::read_delim(files$ns8_2015_derived, delim = "\t")
ns9_data <- readr::read_delim(files$ns9_2022_derived_variables, delim = "\t")

# Merge all datasets by NSID
merged_data <- 
  full_join(wave_one_data, wave_four_data, by = "NSID") %>%
  full_join(ns8_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

# Function to map missing values to standard codes
harmonise_missing_values <- function(x, source_missing_map) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  
  for (source_code in names(source_missing_map)) {
    target_code <- source_missing_map[[source_code]]
    x[x == as.numeric(substr(source_code, 1, nchar(source_code) - 2))] <- target_code
  }
  
  return(x)
}

# Define mapping for W8DBMI missing values
w8_missing_map <- c("-9.0" = -9, "-8.0" = -8, "-1.0" = -1)

# Define mapping for W9DBMI missing values
w9_missing_map <- c("-9.0" = -9, "-8.0" = -8, "-1.0" = -1)

# Process W8DBMI (Age 25)
merged_data <- merged_data %>%
  mutate(W8DBMI = harmonise_missing_values(W8DBMI, w8_missing_map))

# Process W9DBMI (Age 32)
merged_data <- merged_data %>%
  mutate(W9DBMI = harmonise_missing_values(W9DBMI, w9_missing_map))

# Rename variables to match requirements
merged_data <- merged_data %>%
  rename(
    bmi25 = W8DBMI,
    bmi32 = W9DBMI
  )

# Select only NSID and derived BMI variables
final_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write the final output to CSV
readr::write_csv(final_data, "data/output/cleaned_data.csv")