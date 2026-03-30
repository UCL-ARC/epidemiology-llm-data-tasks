
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing value codes
standard_missing_codes <- list(
  refusal = -9,
  dont_know = -8,
  not_applicable = -1,
  not_asked = -3
)

# Define file paths
file_paths <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_three = 'data/input/wave_three_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_five = 'data/input/wave_five_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  ns8 = 'data/input/ns8_2015_main_interview.tab',
  ns9 = 'data/input/ns9_2022_main_interview.tab'
)

# Load datasets
load_dataset <- function(file_path) {
  read_delim(file_path, delim = '\t', col_types = cols(NSID = col_character()))
}

datasets <- map(file_paths, load_dataset)

# Name datasets for clarity
wave_one <- datasets[[1]]
wave_two <- datasets[[2]]
wave_three <- datasets[[3]]
wave_four <- datasets[[4]]
wave_five <- datasets[[5]]
wave_six <- datasets[[6]]
wave_seven <- datasets[[7]]
ns8 <- datasets[[8]]
ns9 <- datasets[[9]]

# Function to harmonize missing values (only for numeric columns)
harmonize_missing_values <- function(x) {
  x %>%
    mutate(across(where(is.numeric), ~ case_when(
      . %in% c(-999, -998, -997, -995, -99) ~ standard_missing_codes$not_asked,
      . %in% c(-92) ~ standard_missing_codes$refusal,
      . %in% c(-91, -1) ~ standard_missing_codes$not_applicable,
      . %in% c(-8) ~ standard_missing_codes$dont_know,
      TRUE ~ .
    )))
}

# Harmonize missing values for each dataset
wave_one <- harmonize_missing_values(wave_one)
wave_two <- harmonize_missing_values(wave_two)
wave_three <- harmonize_missing_values(wave_three)
wave_four <- harmonize_missing_values(wave_four)
wave_five <- harmonize_missing_values(wave_five)
wave_six <- harmonize_missing_values(wave_six)
wave_seven <- harmonize_missing_values(wave_seven)
ns8 <- harmonize_missing_values(ns8)
ns9 <- harmonize_missing_values(ns9)

# Merge datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = 'NSID') %>%
  full_join(wave_three, by = 'NSID') %>%
  full_join(wave_four, by = 'NSID') %>%
  full_join(wave_five, by = 'NSID') %>%
  full_join(wave_six, by = 'NSID') %>%
  full_join(wave_seven, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Create consolidated sex variable
consolidated_sex <- merged_data %>%
  transmute(
    NSID,
    sex = coalesce(
      W9DSEX,  # Most recent valid response
      W8CMSEX,
      W7Sex,
      W6Sex,
      W5SexYP,
      W4SexYP,
      W3sexYP,
      W2SexYP,
      W1sexYP
    )
  )

# Define labels for sex
sex_labels <- c(
  '1' = 'Male',
  '2' = 'Female',
  '-9' = 'Refusal',
  '-8' = "Don't know",
  '-1' = 'Not applicable',
  '-3' = 'Not asked'
)

# Convert sex to factor with labels
consolidated_sex$sex <- factor(consolidated_sex$sex, labels = sex_labels, levels = c(-3, -9, -8, -1, 1, 2))

# Output the cleaned data
write_csv(consolidated_sex, 'data/output/cleaned_data.csv')

# Print confirmation message
message('Cleaned data has been saved to data/output/cleaned_data.csv')
