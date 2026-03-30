
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_derived.tab",
  wave_nine = "data/input/ns9_2022_derived_variables.tab"
)

# Load datasets
load_dataset <- function(file_path) {
  read_delim(file_path, delim = "\t")
}

datasets <- map(file_paths, load_dataset)

# Name datasets for clarity
wave_one <- datasets[[1]]
wave_four <- datasets[[2]]
wave_eight <- datasets[[3]]
wave_nine <- datasets[[4]]

# Merge datasets by NSID
merged_data <- full_join(wave_one, wave_four, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(x) {
  x <- as.numeric(x)

  # Map wave-specific missing codes to standard codes
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1

  # Replace NA and other negative values with -3 (not asked/interviewed)
  x[is.na(x) | x < -9] <- -3

  return(x)
}

# Apply harmonization to BMI variables
merged_data <- merged_data %>%
  mutate(
    bmi25 = harmonize_missing(W8DBMI),
    bmi32 = harmonize_missing(W9DBMI)
  )

# Select only required variables
cleaned_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write output to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Print missing value codes and their meanings
cat("Missing value codes and their meanings:\n")
cat("-9: Refusal\n")
cat("-8: Don't know/insufficient information\n")
cat("-1: Item not applicable\n")
cat("-3: Not asked/interviewed\n")
cat("-2: Script error/lost\n")

# Print confirmation
message("Cleaned dataset has been written to data/output/cleaned_data.csv")
