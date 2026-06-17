# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
file_paths <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_derived_variables.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load each file into separate objects
loaded_files <- map(file_paths, ~ readr::read_delim(.x, delim = "\t"))

# Merge datasets using full_join by NSID
merged_data <- reduce(loaded_files, full_join, by = "NSID")

# Define function to map missing values
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    x <- as.numeric(x)
    x[is.na(x)] <- -3  # Replace NA with -3 (Not asked)
    x <- ifelse(x == -94, -8, x)      # Insufficient information
    x <- ifelse(x %in% c(-999, -998, -997, -995), -2, x)  # Schedule not applicable/script error
    x <- ifelse(x == -99, -3, x)     # Not asked
    x <- ifelse(x == -92, -9, x)     # Refused
    x <- ifelse(x == -91, -1, x)     # Not applicable
    return(x)
  }
  return(x)
}

# Define function to create labelled factors
create_labelled_factor <- function(x, labels) {
  x <- map_missing_values(x)
  return(as.factor(x))
}

# Process regub15 (Urban/Rural Indicator at age 15)
wave_two_data <- loaded_files[[2]]
if (!"urbind" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    left_join(wave_two_data, by = "NSID")
}
merged_data <- merged_data %>%
  mutate(regub15 = create_labelled_factor(urbind, 
    c("-8" = "Insufficient information",
      "1" = "Urban >= 10k - sparse",
      "2" = "Town & Fringe - sparse",
      "3" = "Village - sparse",
      "4" = "Hamlet and Isolated Dwelling - sparse",
      "5" = "Urban >= 10k - less sparse",
      "6" = "Town & Fringe - less sparse",
      "7" = "Village - less sparse",
      "8" = "Hamlet & Isolated Dwelling"
    )))

# Process regub16 (Urban/Rural Indicator at age 16)
wave_three_data <- loaded_files[[3]]
if (!"urbind" %in% names(merged_data) || all(is.na(merged_data$urbind))) {
  merged_data <- merged_data %>%
    left_join(wave_three_data, by = "NSID")
}
merged_data <- merged_data %>%
  mutate(regub16 = create_labelled_factor(urbind, 
    c("-8" = "Insufficient information",
      "1" = "Urban >= 10k - sparse",
      "2" = "Town & Fringe - sparse",
      "3" = "Village - sparse",
      "4" = "Hamlet and Isolated Dwelling - sparse",
      "5" = "Urban >= 10k - less sparse",
      "6" = "Town & Fringe - less sparse",
      "7" = "Village - less sparse",
      "8" = "Hamlet & Isolated Dwelling"
    )))

# Process regov15 (Government Office Region at age 15)
if (!"gor" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    left_join(wave_two_data, by = "NSID")
}
merged_data <- merged_data %>%
  mutate(regov15 = create_labelled_factor(gor, 
    c("-8" = "Insufficient information",
      "1" = "North East",
      "2" = "North West",
      "3" = "Yorkshire and The Humber",
      "4" = "East Midlands",
      "5" = "West Midlands",
      "6" = "East of England",
      "7" = "London",
      "8" = "South East",
      "9" = "South West"
    )))

# Process regov16 (Government Office Region at age 16)
if (!"gor" %in% names(merged_data) || all(is.na(merged_data$gor))) {
  merged_data <- merged_data %>%
    left_join(wave_three_data, by = "NSID")
}
merged_data <- merged_data %>%
  mutate(regov16 = create_labelled_factor(gor, 
    c("-8" = "Insufficient information",
      "1" = "North East",
      "2" = "North West",
      "3" = "Yorkshire and The Humber",
      "4" = "East Midlands",
      "5" = "West Midlands",
      "6" = "East of England",
      "7" = "London",
      "8" = "South East",
      "9" = "South West"
    )))

# Process regor25 (Government Office Region at age 25)
ns8_data <- loaded_files[[5]]
if (!"W8DGOR" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    left_join(ns8_data, by = "NSID")
}
merged_data <- merged_data %>%
  mutate(regor25 = create_labelled_factor(W8DGOR, 
    c("-9" = "Refused",
      "-8" = "Insufficient information",
      "-1" = "Not applicable",
      "1" = "North East",
      "2" = "North West",
      "3" = "Yorkshire and The Humber",
      "4" = "East Midlands",
      "5" = "West Midlands",
      "6" = "East of England",
      "7" = "London",
      "8" = "South East",
      "9" = "South West",
      "10" = "Wales",
      "11" = "Scotland",
      "12" = "Northern Ireland",
      "13" = "Unknown due to faulty/missing postcode"
    )))

# Process regint32 (International Region at age 32)
ns9_main_data <- loaded_files[[7]]
if (!"W9NATIONRES" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    left_join(ns9_main_data, by = "NSID")
}
merged_data <- merged_data %>%
  mutate(regint32 = create_labelled_factor(W9NATIONRES, 
    c("-9" = "Refused",
      "-8" = "Don't know",
      "-3" = "Not asked at fieldwork stage",
      "-1" = "Not applicable",
      "1" = "England",
      "2" = "Scotland",
      "3" = "Wales",
      "4" = "Northern Ireland",
      "5" = "Outside of UK or unknown"
    )))

# Select only the ID and final derived variables
final_output <- merged_data %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regint32)

# Write output to CSV
readr::write_csv(final_output, "data/output/cleaned_data.csv")

# Output success message
cat("Cleaned data has been written to data/output/cleaned_data.csv\n")