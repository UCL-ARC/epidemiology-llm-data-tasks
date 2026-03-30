library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Define file paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_family_background_2020.tab',
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four = 'data/input/wave_four_lsype_family_background_2020.tab'
)

# Load all datasets
wave1 <- read_delim(files$wave_one, delim = '\t', col_types = cols(.default = 'c'))
wave2 <- read_delim(files$wave_two, delim = '\t', col_types = cols(.default = 'c'))
wave3 <- read_delim(files$wave_three, delim = '\t', col_types = cols(.default = 'c'))
wave4 <- read_delim(files$wave_four, delim = '\t', col_types = cols(.default = 'c'))

# Code missing values function
code_missing_values <- function(x) {
  x <- as.numeric(x)
  
  # Handle empty string values - code as -3
  if (any(x == '')) {
    x[x == ''] <- -3
  }
  
  # Apply specific mappings based on meaning
  x[x == -999] <- -8  # Missing household information
  x[x == -99] <- -1   # Not interviewed
  x[x == -98] <- -1   # Not present
  x[x == -94] <- -8   # Insufficient information
  x[x == -996] <- -8  # No parent in household
  x[x == -92] <- -9   # Refusal
  
  return(x)
}

# Apply missing value recoding
wave1$W1empsmum <- code_missing_values(wave1$W1empsmum)
wave1$W1empsdad <- code_missing_values(wave1$W1empsdad)

wave2$W2empsmum <- code_missing_values(wave2$W2empsmum)
wave2$W2empsdad <- code_missing_values(wave2$W2empsdad)

wave3$W3empsmum <- code_missing_values(wave3$W3empsmum)
wave3$W3empsdad <- code_missing_values(wave3$W3empsdad)

wave4$w4empsmum <- code_missing_values(wave4$w4empsmum)
wave4$w4empsdad <- code_missing_values(wave4$w4empsdad)

# Rename variables to follow conventions (lowercase, age suffix)
wave1_renamed <- wave1 %>%
  rename(
    empsmum14 = W1empsmum,
    empsdad14 = W1empsdad
  )

wave2_renamed <- wave2 %>%
  rename(
    empsmum15 = W2empsmum,
    empsdad15 = W2empsdad
  )

wave3_renamed <- wave3 %>%
  rename(
    empsmum16 = W3empsmum,
    empsdad16 = W3empsdad
  )

wave4_renamed <- wave4 %>%
  rename(
    empsmum17 = w4empsmum,
    empsdad17 = w4empsdad
  )

# Merge all waves using full_join by NSID
merged_data <- full_join(wave1_renamed, wave2_renamed, by = 'NSID', suffix = c('', '.'))
merged_data <- full_join(merged_data, wave3_renamed, by = 'NSID', suffix = c('', '.'))
merged_data <- full_join(merged_data, wave4_renamed, by = 'NSID', suffix = c('', '.'))

# Create harmonized factor variables with explicit labels
harmonize_employment <- function(x) {
  # Create factor with explicit levels and labels
  lvl <- c(-9, -8, -1, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  lab <- c(
    '-9' = 'Refusal',
    '-8' = "Don't know/insufficient information",
    '-1' = 'Item not applicable',
    '-3' = 'Not asked at fieldwork stage',
    '1' = 'Paid work (30+ hours)',
    '2' = 'Paid work (<30 hours)',
    '3' = 'Unemployed',
    '4' = 'Training',
    '5' = 'Education',
    '6' = 'Looking after family',
    '7' = 'Retired',
    '8' = 'Sick/disabled',
    '9' = 'Other'
  )
  
  # Convert to factor first
  x <- factor(x, levels = lvl, labels = lab)
  
  return(x)
}

# Create harmonized variables for each wave
merged_data$emp_mom14_harmonized <- harmonize_employment(merged_data$empsmum14)
merged_data$emp_dad14_harmonized <- harmonize_employment(merged_data$empsdad14)
merged_data$emp_mom15_harmonized <- harmonize_employment(merged_data$empsmum15)
merged_data$emp_dad15_harmonized <- harmonize_employment(merged_data$empsdad15)
merged_data$emp_mom16_harmonized <- harmonize_employment(merged_data$empsmum16)
merged_data$emp_dad16_harmonized <- harmonize_employment(merged_data$empsdad16)
merged_data$emp_mom17_harmonized <- harmonize_employment(merged_data$empsmum17)
merged_data$emp_dad17_harmonized <- harmonize_employment(merged_data$empsdad17)

# Define output variables
output_vars <- c('NSID',
  'empsmum14', 'empsdad14', 'emp_mom14_harmonized',
  'empsmum15', 'empsdad15', 'emp_mom15_harmonized',
  'empsmum16', 'empsdad16', 'emp_mom16_harmonized',
  'empsmum17', 'empsdad17', 'emp_mom17_harmonized',
  'emp_dad17_harmonized'
)

# Filter to only include these variables
final_data <- merged_data %>% select(all_of(output_vars))

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
cat('Output saved to: data/output/cleaned_data.csv\n')
