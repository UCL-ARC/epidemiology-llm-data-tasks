
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Define file paths and metadata
file_metadata <- list(
  wave_one_lsype_young_person_2020 = list(file = 'data/input/wave_one_lsype_young_person_2020.tab'),
  wave_four_lsype_young_person_2020 = list(file = 'data/input/wave_four_lsype_young_person_2020.tab'),
  ns8_2015_derived = list(file = 'data/input/ns8_2015_derived.tab'),
  ns9_2022_derived = list(file = 'data/input/ns9_2022_derived_variables.tab')
)

# Load each file individually
wave_one_lsype_young_person_2020 <- readr::read_delim(file_metadata$wave_one_lsype_young_person_2020$file, delim = '\t')
wave_four_lsype_young_person_2020 <- readr::read_delim(file_metadata$wave_four_lsype_young_person_2020$file, delim = '\t')
ns8_2015_derived <- readr::read_delim(file_metadata$ns8_2015_derived$file, delim = '\t')
ns9_2022_derived_variables <- readr::read_delim(file_metadata$ns9_2022_derived$file, delim = '\t')

# Merge datasets by NSID, keeping all observations
merged_data <- wave_one_lsype_young_person_2020 %>%
  full_join(wave_four_lsype_young_person_2020, by = 'NSID') %>%
  full_join(ns8_2015_derived, by = 'NSID') %>%
  full_join(ns9_2022_derived_variables, by = 'NSID')

# Define income labels
income_labels <- c(
  '-1' = 'Not applicable',
  '1' = 'less than 25',
  '2' = '25 to 50',
  '3' = '50 to 90',
  '4' = '90 to 140',
  '5' = '140 to 240',
  '6' = '240 to 300',
  '7' = '300 to 350',
  '8' = '350 to 400',
  '9' = '400 to 500',
  '10' = '500 to 600',
  '11' = '600 to 700',
  '12' = '700 to 800',
  '13' = '800 to 900',
  '14' = '900 to 1200',
  '15' = '1200 to 1400',
  '16' = 'more than 1400'
)

# Function to convert missing values and create factors
create_factor_with_labels <- function(df, var_name) {
  df %>%
    mutate(
      !!var_name := factor(
        case_when(
          .data[[var_name]] == -1 ~ -1,
          is.na(.data[[var_name]]) ~ NA,
          TRUE ~ as.integer(.data[[var_name]])
        ),
        levels = as.numeric(names(income_labels)),
        labels = income_labels
      )
    )
}

# Apply the conversion and factor creation to income variables
merged_data <- merged_data %>%
  create_factor_with_labels('W8DINCB') %>%
  create_factor_with_labels('W9DINCB')

# Convert missing values to standard codes in the final output
merged_data <- merged_data %>%
  mutate(
    W8DINCB = ifelse(is.na(W8DINCB), -3, W8DINCB),
    W9DINCB = ifelse(is.na(W9DINCB), -3, W9DINCB)
  )

# Rename variables to match additional requirements
merged_data <- merged_data %>%
  rename(
    inc25 = W8DINCB,
    inc32 = W9DINCB
  )

# Select only the ID and derived variables
final_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Write the final output to CSV
readr::write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Data cleaning and preprocessing completed. Output saved to data/output/cleaned_data.csv\n')
