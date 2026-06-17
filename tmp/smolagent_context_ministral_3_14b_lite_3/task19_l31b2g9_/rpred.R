
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load metadata and define file paths
metadata <- list(
  wave_one_lsype_young_person_2020 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four_lsype_young_person_2020 = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_2015_derived = 'data/input/ns8_2015_derived.tab',
  ns9_2022_derived_variables = 'data/input/ns9_2022_derived_variables.tab'
)

# Load each file into a separate object
wave_one_data <- readr::read_delim(metadata$wave_one_lsype_young_person_2020, delim = '\t')
wave_four_data <- readr::read_delim(metadata$wave_four_lsype_young_person_2020, delim = '\t')
ns8_derived_data <- readr::read_delim(metadata$ns8_2015_derived, delim = '\t')
ns9_derived_data <- readr::read_delim(metadata$ns9_2022_derived_variables, delim = '\t')

# Merge datasets by NSID
merged_data <- full_join(wave_one_data, wave_four_data, by = 'NSID')
merged_data <- full_join(merged_data, ns8_derived_data, by = 'NSID')
merged_data <- full_join(merged_data, ns9_derived_data, by = 'NSID')

# Check if W8DBMI and W9DBMI exist in the datasets
if (!"W8DBMI" %in% names(merged_data)) {
  stop("W8DBMI variable not found in the dataset.")
}

if (!"W9DBMI" %in% names(merged_data)) {
  stop("W9DBMI variable not found in the dataset.")
}

# Ensure W8DBMI and W9DBMI are numeric
if (!is.numeric(merged_data$W8DBMI)) {
  stop("W8DBMI is not numeric.")
}

if (!is.numeric(merged_data$W9DBMI)) {
  stop("W9DBMI is not numeric.")
}

# Standardize missing-value codes for BMI variables
standardize_missing <- function(x) {
  x[is.na(x)] <- -3  # Default for NA to -3 (Not asked/Not interviewed)
  x[x == -9] <- -9   # Refusal
  x[x == -8] <- -8   # Insufficient information
  x[x == -1] <- -1   # Not applicable
  return(x)
}

# Apply standardization to BMI variables
merged_data <- merged_data %>%
  mutate(bmi25 = standardize_missing(W8DBMI),
         bmi32 = standardize_missing(W9DBMI))

# Select only the ID and final derived variables
final_output <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write output to CSV
output_path <- 'data/output/cleaned_data.csv'
readr::write_csv(final_output, output_path)

# Confirm output file creation
if (file.exists(output_path)) {
  message("Successfully created output file: ", output_path)
} else {
  stop("Failed to create output file.")
}
