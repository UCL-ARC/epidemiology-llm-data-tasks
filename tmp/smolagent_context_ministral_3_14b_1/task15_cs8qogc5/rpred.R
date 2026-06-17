
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Load metadata and define file paths
metadata <- list(
  wave_one_lsype_young_person_2020 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four_lsype_young_person_2020 = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_2015_derived = 'data/input/ns8_2015_derived.tab',
  ns9_2022_derived_variables = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all files into separate data frames
load_files <- function(file_path) {
  read_delim(file_path, delim = '\t', show_col_types = FALSE)
}

# Use map to load files
loaded_dfs <- map(metadata, load_files)

# Merge all datasets by NSID to preserve the full cohort frame
merged_df <- reduce(loaded_dfs, full_join, by = 'NSID')

# Define income band labels
income_band_labels <- c(
  'Not applicable', 'Missing',
  'less than 25', '25 to 50', '50 to 90', '90 to 140', '140 to 240',
  '240 to 300', '300 to 350', '350 to 400', '400 to 500', '500 to 600',
  '600 to 700', '700 to 800', '800 to 900', '900 to 1200', '1200 to 1400',
  'more than 1400'
)

# Function to map missing values
map_missing_values <- function(x) {
  replace_na(x, -3)  # Replace NA with -3
}

# Derive inc25 variable from W8DINCB (age 25)
if ('W8DINCB' %in% names(merged_df)) {
  merged_df$inc25 <- map_missing_values(merged_df$W8DINCB)
  levels_inc25 <- c(-1, -3, 1:16)
  merged_df$inc25 <- factor(merged_df$inc25, levels = levels_inc25, labels = income_band_labels)
  merged_df$inc25 <- as.numeric(as.character(merged_df$inc25))
}

# Derive inc32 variable from W9DINCB (age 32)
if ('W9DINCB' %in% names(merged_df)) {
  merged_df$inc32 <- map_missing_values(merged_df$W9DINCB)
  levels_inc32 <- c(-1, -3, 1:16)
  merged_df$inc32 <- factor(merged_df$inc32, levels = levels_inc32, labels = income_band_labels)
  merged_df$inc32 <- as.numeric(as.character(merged_df$inc32))
}

# Select only the required variables: NSID, inc25, and inc32
final_df <- merged_df %>%
  select(NSID, inc25, inc32)

# Write the final cleaned dataset to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

# Print confirmation
message('Cleaned dataset written to data/output/cleaned_data.csv')
