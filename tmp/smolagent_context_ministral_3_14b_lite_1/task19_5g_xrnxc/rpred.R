# Load required libraries
library(readr)
library(dplyr)

# Define file paths
file_paths <- c(
  'data/input/wave_one_lsype_young_person_2020.tab',
  'data/input/wave_four_lsype_young_person_2020.tab',
  'data/input/ns8_2015_derived.tab',
  'data/input/ns9_2022_derived_variables.tab'
)

# Load datasets
wave1_data <- read_delim(file_paths[1], delim = '\t')
wave4_data <- read_delim(file_paths[2], delim = '\t')
wave8_data <- read_delim(file_paths[3], delim = '\t')
wave9_data <- read_delim(file_paths[4], delim = '\t')

# Merge datasets
cleaned_data <- full_join(
  full_join(wave1_data, wave4_data, by = 'NSID'),
  full_join(wave8_data, wave9_data, by = 'NSID'), 
  by = 'NSID'
)

# Extract and rename BMI variables
cleaned_data <- cleaned_data %>%
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

# Standardize missing values
cleaned_data <- cleaned_data %>%
  mutate(
    bmi25 = ifelse(bmi25 == -9 | bmi25 == -8 | bmi25 == -1, bmi25, ifelse(is.na(bmi25), -3, bmi25)),
    bmi32 = ifelse(bmi32 == -9 | bmi32 == -8 | bmi32 == -1, bmi32, ifelse(is.na(bmi32), -3, bmi32))
  )

# Select and save final output
final_output <- cleaned_data %>%
  select(NSID, bmi25, bmi32)

write_csv(final_output, 'data/output/cleaned_data.csv')