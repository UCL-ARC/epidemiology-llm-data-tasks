
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Suppress messages for cleaner output
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(labelled)
  library(readr)
})

# Define paths and filenames
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_derived = 'data/input/ns8_2015_derived.tab',
  ns9_derived = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all datasets
load_data <- function(file) {
  read_delim(file, delim = '\t', show_col_types = FALSE)
}

# Load each dataset individually
wave1 <- load_data(files$wave_one)
wave2 <- load_data(files$wave_two)
wave4 <- load_data(files$wave_four)
wave8 <- load_data(files$ns8_derived)
wave9 <- load_data(files$ns9_derived)

# Merge all datasets by NSID
merged_data <- full_join(wave1, wave2, by = 'NSID')
merged_data <- full_join(merged_data, wave4, by = 'NSID')
merged_data <- full_join(merged_data, wave8, by = 'NSID')
merged_data <- full_join(merged_data, wave9, by = 'NSID')

# Define ethnicity mapping rules for missing values
missing_mapping <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3  # Convert NA to -3 (not interviewed)
    x[x == -999] <- -2  # Schedule not applicable/script error
    x[x == -998] <- -2
    x[x == -997] <- -2
    x[x == -995] <- -2
    x[x == -94] <- -8  # Insufficient information
    x[x == -92] <- -9  # Refused
    x[x == -91] <- -1  # Not applicable
    x[x == -99] <- -3  # Not interviewed
  }
  return(x)
}

# Apply missing value mapping to all ethnicity variables
ethnicity_vars <- c('W1ethnic2YP', 'W2ethnicYP', 'w4ethnic2YP', 'W8DETHN15', 'W9DETHN15')
for (var in ethnicity_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- missing_mapping(merged_data[[var]])
  }
}

# Create a consolidated ethnicity variable (eth) using earliest-valid-first rule
consolidated_ethnicity <- function(df) {
  ethnicity_vars <- c('W1ethnic2YP', 'W2ethnicYP', 'w4ethnic2YP', 'W8DETHN15', 'W9DETHN15')
  df$eth <- -3  # Default to not interviewed

  for (var in ethnicity_vars) {
    if (!(var %in% names(df))) next
    valid_values <- df[[var]] != -3 & df[[var]] != -2 & df[[var]] != -1 & df[[var]] != -8 & df[[var]] != -9
    df$eth[valid_values & df$eth == -3] <- df[[var]][valid_values & df$eth == -3]
  }
  return(df)
}

# Apply the consolidated ethnicity variable
merged_data <- consolidated_ethnicity(merged_data)

# Define value labels for the consolidated ethnicity variable
eth_labels <- c(
  `-9` = 'Refused',
  `-8` = 'Insufficient information',
  `-3` = 'Not interviewed',
  `-2` = 'Schedule not applicable / script error',
  `-1` = 'Not applicable',
  `1` = 'White - British',
  `2` = 'White - Irish',
  `3` = 'Any other White background',
  `4` = 'Mixed - White and Black Caribbean',
  `5` = 'Mixed - White and Black African',
  `6` = 'Mixed - White and Asian',
  `7` = 'Any other mixed background',
  `8` = 'Indian',
  `9` = 'Pakistani',
  `10` = 'Bangladeshi',
  `11` = 'Any other Asian background',
  `12` = 'Black Caribbean',
  `13` = 'Black African',
  `14` = 'Any other Black background',
  `15` = 'Chinese',
  `16` = 'Any other ethnic background'
)

# Convert eth to a labeled factor
merged_data$eth <- factor(merged_data$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

# Select only the required variables: NSID and eth
final_data <- merged_data %>% select(NSID, eth)

# Print first few rows of the final dataset to confirm
cat('First few rows of the final dataset:\n')
print(head(final_data))

# Write the final dataset to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')
cat('\nDataset successfully written to data/output/cleaned_data.csv\n')
