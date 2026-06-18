
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Define paths
input_path <- 'data/input/'
output_path <- 'data/output/'

# Load files
wave1 <- readr::read_delim(paste0(input_path, 'wave_one_lsype_family_background_2020.tab'), delim = '\t')
wave2 <- readr::read_delim(paste0(input_path, 'wave_two_lsype_family_background_2020.tab'), delim = '\t')
wave3 <- readr::read_delim(paste0(input_path, 'wave_three_lsype_family_background_2020.tab'), delim = '\t')
wave4 <- readr::read_delim(paste0(input_path, 'wave_four_lsype_family_background_2020.tab'), delim = '\t')

# Merge datasets
merged_data <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Function to handle missing values
map_missing <- function(x) {
  x <- ifelse(x == -999 | x == -992, -2, x)
  x <- ifelse(x == -99 | x == -94, -3, x)
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(x == -91, -1, x)
  x <- ifelse(x == -3 | x == -1, -3, x)
  x <- ifelse(is.na(x), -3, x)
  return(x)
}

# Process age 14
merged_data$incband14 <- case_when(
  merged_data$W1GrsswkHH %in% c(-999, -992) ~ -2,
  merged_data$W1GrsswkHH %in% c(-99, -94) ~ -3,
  merged_data$W1GrsswkHH == -92 ~ -9,
  merged_data$W1GrsswkHH == -91 ~ -1,
  merged_data$W1GrsswkHH %in% 1:12 ~ merged_data$W1GrsswkHH,
  TRUE ~ -3
)
merged_data$inccont14 <- map_missing(merged_data$W1GrsswkHH)
merged_data$inccont14[merged_data$inccont14 >= 1] <-
  c(NA, NA, NA, NA, NA, NA, NA, NA, 24.5, 74.5, 149.5, 249.5, 349.5, 449.5,
    549.5, 649.5, 749.5, 849.5, 949.5, 1000)[merged_data$inccont14[merged_data$inccont14 >= 1] + 8]

# Process age 15
merged_data$incband15 <- case_when(
  merged_data$W2GrsswkHH %in% c(-999, -992) ~ -2,
  merged_data$W2GrsswkHH %in% c(-99, -94) ~ -3,
  merged_data$W2GrsswkHH == -92 ~ -9,
  merged_data$W2GrsswkHH == -91 ~ -1,
  merged_data$W2GrsswkHH %in% 1:12 ~ merged_data$W2GrsswkHH,
  TRUE ~ -3
)
merged_data$inccont15 <- map_missing(merged_data$W2GrsswkHH)
merged_data$inccont15[merged_data$inccont15 >= 1] <-
  c(NA, NA, NA, NA, NA, NA, NA, NA, 24.5, 74.5, 149.5, 249.5, 349.5, 449.5,
    549.5, 649.5, 749.5, 849.5, 949.5, 1000)[merged_data$inccont15[merged_data$inccont15 >= 1] + 8]

# Process age 16
merged_data$incband16 <- case_when(
  merged_data$W3incestw %in% c(-999, -992) ~ -2,
  merged_data$W3incestw %in% c(-99, -94) ~ -3,
  merged_data$W3incestw == -92 ~ -9,
  merged_data$W3incestw == -91 ~ -1,
  merged_data$W3incestw %in% 1:12 ~ merged_data$W3incestw,
  TRUE ~ -3
)

# Process age 17
merged_data$incband17 <- case_when(
  merged_data$w4IncEstW %in% c(-999, -992) ~ -2,
  merged_data$w4IncEstW %in% c(-99, -94) ~ -3,
  merged_data$w4IncEstW == -92 ~ -9,
  merged_data$w4IncEstW == -91 ~ -1,
  merged_data$w4IncEstW %in% 1:12 ~ merged_data$w4IncEstW,
  TRUE ~ -3
)

# Create labeled factors for banded variables
labels <- c(
  '-9' = 'Refused',
  '-8' = 'Don\'t know',
  '-3' = 'Not interviewed/insufficient info',
  '-2' = 'Schedule not applicable/script error',
  '-1' = 'Not applicable',
  '1' = 'Up to £49',
  '2' = '£50 up to £99',
  '3' = '£100 up to £199',
  '4' = '£200 up to £299',
  '5' = '£300 up to £399',
  '6' = '£400 up to £499',
  '7' = '£500 up to £599',
  '8' = '£600 up to £699',
  '9' = '£700 up to £799',
  '10' = '£800 up to £899',
  '11' = '£900 up to £999',
  '12' = '£1,000 or more'
)

merged_data$incband14 <- factor(merged_data$incband14, levels = names(labels), labels = labels)
merged_data$incband15 <- factor(merged_data$incband15, levels = names(labels), labels = labels)
merged_data$incband16 <- factor(merged_data$incband16, levels = names(labels), labels = labels)
merged_data$incband17 <- factor(merged_data$incband17, levels = names(labels), labels = labels)

# Select final variables
final_vars <- merged_data %>%
  select(NSID, incband14, incband15, incband16, incband17, inccont14, inccont15)

# Write output
readr::write_csv(final_vars, paste0(output_path, 'cleaned_data.csv'))

cat('Cleaned data saved to: ', paste0(output_path, 'cleaned_data.csv'), '\n')
