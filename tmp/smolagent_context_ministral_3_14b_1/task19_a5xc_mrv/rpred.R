
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the input and output paths
input_path <- "data/input/"
output_path <- "data/output/cleaned_data.csv"

# Load metadata for reference
metadata <- list(
  wave_one_lsype_young_person_2020 = list(
    file = "wave_one_lsype_young_person_2020.tab",
    description = "Sweep 1 (Age 14) Young Person Data File"
  ),
  wave_four_lsype_young_person_2020 = list(
    file = "wave_four_lsype_young_person_2020.tab",
    description = "Sweep 4 (Age 17) Young Person Data File"
  ),
  ns8_2015_derived = list(
    file = "ns8_2015_derived.tab",
    description = "Wave Eight Derived Variables Data File",
    bmi_var = "W8DBMI",
    wave_age = 25
  ),
  ns9_2022_derived = list(
    file = "ns9_2022_derived_variables.tab",
    description = "Age 32 (Sweep 9) Derived Variables Data File",
    bmi_var = "W9DBMI",
    wave_age = 32
  )
)

# Load all files into a list
load_files <- function(file_list) {
  purrr::map(file_list, function(file_info) {
    file_path <- paste0(input_path, file_info$file)
    readr::read_delim(file_path, delim = "\t")
  })
}

# Load all files
all_files <- load_files(purrr::map(metadata, function(x) x))

# Extract the relevant files for BMI variables
ns8_data <- all_files[[3]]
ns9_data <- all_files[[4]]

# Merge datasets by NSID to ensure full cohort frame
merged_data <- full_join(ns8_data, ns9_data, by = "NSID")

# Standardize missing values for BMI variables
standardize_missing_values <- function(data, bmi_var, user_missing_values) {
  # Map user missing values to standard codes
  data <- data %>%
    mutate(
      !!bmi_var := case_when(
        .data[[bmi_var]] %in% c(-9, -8, -1) ~
          case_when(
            .data[[bmi_var]] == -9 ~ -9,
            .data[[bmi_var]] == -8 ~ -8,
            .data[[bmi_var]] == -1 ~ -1,
            TRUE ~ NA_real_
          ),
        is.na(.data[[bmi_var]]) ~ -3,
        TRUE ~ .data[[bmi_var]]
      )
    )
  data
}

# Standardize missing values for W8DBMI and W9DBMI
ns8_data <- standardize_missing_values(ns8_data, "W8DBMI", "-9.0 thru -8.0 and -1.0")
ns9_data <- standardize_missing_values(ns9_data, "W9DBMI", "-1.0 thru -8.0 and -9.0")

# Merge datasets again after standardizing missing values
merged_data <- full_join(ns8_data, ns9_data, by = "NSID")

# Rename BMI variables to match the required output names
merged_data <- merged_data %>%
  rename(
    bmi25 = W8DBMI,
    bmi32 = W9DBMI
  )

# Select only the ID and the derived BMI variables
final_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write the final cleaned data to CSV
write_csv(final_data, output_path)

# Print confirmation of successful execution
cat("Cleaned data has been written to:", output_path)
