
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the input and output paths
input_path <- 'data/input/'
output_path <- 'data/output/'

# Load each file explicitly by name into a separate object
wave_one <- read_delim(file.path(input_path, 'wave_one_lsype_young_person_2020.tab'), delim = '\t')
wave_four <- read_delim(file.path(input_path, 'wave_four_lsype_young_person_2020.tab'), delim = '\t')
ns8_derived <- read_delim(file.path(input_path, 'ns8_2015_derived.tab'), delim = '\t')
ns9_derived <- read_delim(file.path(input_path, 'ns9_2022_derived_variables.tab'), delim = '\t')

# Merge datasets using full_join by NSID
merged_data <- full_join(wave_one, wave_four, by = 'NSID')
merged_data <- full_join(merged_data, ns8_derived, by = 'NSID')
merged_data <- full_join(merged_data, ns9_derived, by = 'NSID')

# Define value labels for W8DINCB and W9DINCB
value_labels <- c(
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

# Clean W8DINCB and W9DINCB
clean_W8DINCB <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -1] <- -1
  x <- factor(x, levels = c(-1, 1:16), labels = value_labels)
  x
}

clean_W9DINCB <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -1] <- -1
  x <- factor(x, levels = c(-1, 1:16), labels = value_labels)
  x
}

# Apply cleaning functions
merged_data <- merged_data %>%
  mutate(
    inc25 = clean_W8DINCB(W8DINCB),
    inc32 = clean_W9DINCB(W9DINCB)
  )

# Select only the ID variable and the derived variables
final_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Write the final dataset to CSV
write_csv(final_data, file.path(output_path, 'cleaned_data.csv'))

# Print confirmation
message('Data cleaning and preprocessing completed successfully. Output written to data/output/cleaned_data.csv')
