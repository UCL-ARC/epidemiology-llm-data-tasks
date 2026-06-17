
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
file_paths <- list(
  wave_one_lsype_young_person_2020 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two_lsype_family_background_2020 = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three_lsype_family_background_2020 = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four_lsype_young_person_2020 = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns9_2022_derived_variables = 'data/input/ns9_2022_derived_variables.tab'
)

# Load each file
wave_one_data <- readr::read_delim(file_paths$wave_one_lsype_young_person_2020, delim = '\t')
wave_two_data <- readr::read_delim(file_paths$wave_two_lsype_family_background_2020, delim = '\t')
wave_three_data <- readr::read_delim(file_paths$wave_three_lsype_family_background_2020, delim = '\t')
wave_four_data <- readr::read_delim(file_paths$wave_four_lsype_young_person_2020, delim = '\t')
wave_nine_data <- readr::read_delim(file_paths$ns9_2022_derived_variables, delim = '\t')

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3
    x[x == -999] <- -2
    x[x == -94] <- -8
    return(x)
  } else {
    return(x)
  }
}

# Extract and map IMDRSCORE from wave_two_data (Age 15)
wave_two_data <- wave_two_data %>%
  mutate(imd15 = map_missing_values(IMDRSCORE)) %>%
  select(NSID, imd15)

# Extract and map IMDRSCORE from wave_three_data (Age 16)
wave_three_data <- wave_three_data %>%
  mutate(imd16 = map_missing_values(IMDRSCORE)) %>%
  select(NSID, imd16)

# Extract and map W9DIMDD from wave_nine_data (Age 32)
wave_nine_data <- wave_nine_data %>%
  mutate(imd32 = map_missing_values(W9DIMDD)) %>%
  select(NSID, imd32)

# Merge datasets by NSID
merged_data <- full_join(wave_one_data %>% select(NSID), wave_two_data, by = 'NSID') %>%
  full_join(wave_three_data, by = 'NSID') %>%
  full_join(wave_four_data %>% select(NSID), by = 'NSID') %>%
  full_join(wave_nine_data, by = 'NSID')

# Write the final cleaned dataset to CSV
readr::write_csv(merged_data, 'data/output/cleaned_data.csv')
