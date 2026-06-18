
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(forcats)

# Define file paths and variable mappings
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_eight = 'data/input/ns8_2015_derived.tab',
  wave_nine = 'data/input/ns9_2022_derived_variables.tab'
)

# Load each dataset explicitly
wave_one_data <- read_delim(files$wave_one, delim = '\t', show_col_types = FALSE)
wave_four_data <- read_delim(files$wave_four, delim = '\t', show_col_types = FALSE)
wave_eight_data <- read_delim(files$wave_eight, delim = '\t', show_col_types = FALSE)
wave_nine_data <- read_delim(files$wave_nine, delim = '\t', show_col_types = FALSE)

# Merge all datasets using full_join by NSID
merged_data <- wave_one_data %>%
  full_join(wave_four_data, by = 'NSID') %>%
  full_join(wave_eight_data, by = 'NSID') %>%
  full_join(wave_nine_data, by = 'NSID')

# Define missing value harmonisation function
harmonise_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3  # Default for NA to -3
  x
}

# Extract and clean inc25 (W8DINCB) and inc32 (W9DINCB)
final_data <- merged_data %>%
  transmute(
    NSID,
    inc25 = harmonise_missing(W8DINCB),
    inc32 = harmonise_missing(W9DINCB)
  )

# Define value labels for inc25 and inc32
value_labels_inc <- c(
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
  '16' = 'more than 1400',
  '-3' = 'Missing',
  '-2' = 'Schedule not applicable',
  '-9' = 'Refusal',
  '-8' = 'Insufficient information'
)

# Convert to factor with labels
final_data$inc25 <- factor(
  final_data$inc25,
  levels = as.numeric(names(value_labels_inc)),
  labels = value_labels_inc
)

final_data$inc32 <- factor(
  final_data$inc32,
  levels = as.numeric(names(value_labels_inc)),
  labels = value_labels_inc
)

# Write the output to CSV
readr::write_csv(final_data, 'data/output/cleaned_data.csv')
