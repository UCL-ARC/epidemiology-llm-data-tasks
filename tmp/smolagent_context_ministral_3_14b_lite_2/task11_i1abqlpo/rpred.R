# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = 'data/input/wave_one_lsype_family_background_2020.tab',
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four = 'data/input/wave_four_lsype_family_background_2020.tab'
)

# Load datasets
load_datasets <- function(paths) {
  map(paths, ~ read_delim(.x, delim = '\t', col_types = cols(NSID = col_character()))) 
}

datasets <- load_datasets(file_paths)

# Assign loaded datasets to named objects
wave_one <- datasets[[1]]
wave_two <- datasets[[2]]
wave_three <- datasets[[3]]
wave_four <- datasets[[4]]

# Mapping for missing values
missing_map <- tribble(
  ~old_code, ~new_code,
  -999, -2,
  -99, -3,
  -98, -1,
  -94, -8,
  -92, -9,
  -996, -1
)

# Function to map missing values
map_missing_values <- function(x) {
  x <- as.numeric(x)
  for (i in 1:nrow(missing_map)) {
    x[x == missing_map$old_code[i]] <- missing_map$new_code[i]
  }
  x
}

# Define value labels for employment status
emp_labels <- c(
  '1' = 'Doing paid work for 30 or more hours a week',
  '2' = 'Doing paid work for fewer than 30 hours a week',
  '3' = 'Unemployed/ Looking for a job',
  '4' = 'On a training course or scheme',
  '5' = 'In full-time education/ at school',
  '6' = 'Looking after the family/ household',
  '7' = 'Retired from work altogether',
  '8' = 'Sick/ disabled',
  '9' = 'Other'
)

# Map missing values for all variables
wave_one$W1empsmum <- map_missing_values(wave_one$W1empsmum)
wave_one$W1empsdad <- map_missing_values(wave_one$W1empsdad)
wave_two$W2empsmum <- map_missing_values(wave_two$W2empsmum)
wave_two$W2empsdad <- map_missing_values(wave_two$W2empsdad)
wave_three$W3empsmum <- map_missing_values(wave_three$W3empsmum)
wave_three$W3empsdad <- map_missing_values(wave_three$W3empsdad)
wave_four$w4empsmum <- map_missing_values(wave_four$w4empsmum)
wave_four$w4empsdad <- map_missing_values(wave_four$w4empsdad)

# Create labelled factors for employment status
wave_one$W1empsmum <- factor(wave_one$W1empsmum, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))
wave_one$W1empsdad <- factor(wave_one$W1empsdad, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))

wave_two$W2empsmum <- factor(wave_two$W2empsmum, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))
wave_two$W2empsdad <- factor(wave_two$W2empsdad, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))

wave_three$W3empsmum <- factor(wave_three$W3empsmum, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))
wave_three$W3empsdad <- factor(wave_three$W3empsdad, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))

wave_four$w4empsmum <- factor(wave_four$w4empsmum, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))
wave_four$w4empsdad <- factor(wave_four$w4empsdad, levels = c(-9, -8, -7, -3, -2, -1, 1:9), labels = c(
  NA, NA, NA, 'Not interviewed', 'Schedule not applicable', 'Not present',
  emp_labels['1'], emp_labels['2'], emp_labels['3'], emp_labels['4'],
  emp_labels['5'], emp_labels['6'], emp_labels['7'], emp_labels['8'], emp_labels['9']
))

# Merge datasets by NSID
merged_data <- full_join(
  full_join(wave_one, wave_two, by = 'NSID'),
  full_join(wave_three, wave_four, by = 'NSID'),
  by = 'NSID'
)

# Rename variables to match output requirements
merged_data <- merged_data %>%
  rename(
    ecoactma14 = W1empsmum,
    ecoactpa14 = W1empsdad,
    ecoactma15 = W2empsmum,
    ecoactpa15 = W2empsdad,
    ecoactma16 = W3empsmum,
    ecoactpa16 = W3empsdad,
    ecoactma17 = w4empsmum,
    ecoactpa17 = w4empsdad
  )

# Select only NSID and derived variables
final_data <- merged_data %>%
  select(NSID, starts_with('ecoact'))

# Write output to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print success message
message('Data cleaning and preprocessing completed successfully.')