
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Suppress unnecessary messages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# Define paths and filenames
input_path <- 'data/input/'
output_path <- 'data/output/'
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load all datasets
load_datasets <- function(file) {
  read_delim(paste0(input_path, file), delim = '\t', show_col_types = FALSE)
}

# Load datasets into a list
datasets <- map(files, load_datasets)

# Assign names to datasets
wave1 <- datasets[[1]]
wave4 <- datasets[[2]]
wave8 <- datasets[[3]]
wave9 <- datasets[[4]]

# Ensure NSID is a character column
wave1$NSID <- as.character(wave1$NSID)
wave4$NSID <- as.character(wave4$NSID)
wave8$NSID <- as.character(wave8$NSID)
wave9$NSID <- as.character(wave9$NSID)

# Merge all datasets by NSID
merged_data <- full_join(
  full_join(wave1, wave4, by = 'NSID'),
  full_join(wave8, wave9, by = 'NSID'),
  by = 'NSID'
)

# Define missing value mapping
map_missing_values <- function(x) {
  x[is.na(x)] <- -3
  x
}

# Process income variables
merged_data$inc25 <- map_missing_values(merged_data$W8DINCB)
merged_data$inc32 <- map_missing_values(merged_data$W9DINCB)

# Define income labels
inc_labels <- c(
  '-3' = 'Missing',
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

# Convert to factors with labels
merged_data$inc25 <- factor(merged_data$inc25, levels = names(inc_labels), labels = inc_labels[names(inc_labels)])
merged_data$inc32 <- factor(merged_data$inc32, levels = names(inc_labels), labels = inc_labels[names(inc_labels)])

# Select required variables
final_data <- merged_data %>% select(NSID, inc25, inc32)

# Write output to CSV
write_csv(final_data, paste0(output_path, 'cleaned_data.csv'))

cat('Data cleaning and preprocessing complete. Output saved to: data/output/cleaned_data.csv\n')
