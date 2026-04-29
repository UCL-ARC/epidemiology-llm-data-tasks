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
input_dir <- 'data/input/'
output_file <- 'data/output/cleaned_data.csv'

# Define file names
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

# Load datasets
wave1 <- read_delim(paste0(input_dir, files[1]), delim = '\t')
wave2 <- read_delim(paste0(input_dir, files[2]), delim = '\t')
wave3 <- read_delim(paste0(input_dir, files[3]), delim = '\t')
wave4 <- read_delim(paste0(input_dir, files[4]), delim = '\t')

# Standard missing codes
std_missing <- c(-9, -8, -1, -3, -2)

# Function to harmonize missing codes for age 14
harmonize_age14 <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- case_when(
    x == -999.0 ~ -2,
    x == -992.0 ~ -8,
    x == -99.0 ~ -3,
    x == -94.0 ~ -8,
    x == -92.0 ~ -9,
    x == -91.0 ~ -1,
    x == -3.0 ~ -3,
    x == -1.0 ~ -1,
    TRUE ~ x
  )
  return(x)
}

# Function to harmonize missing codes for age 15
harmonize_age15 <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- case_when(
    x == -999.0 ~ -2,
    x == -992.0 ~ -8,
    x == -99.0 ~ -3,
    x == -94.0 ~ -8,
    x == -92.0 ~ -9,
    x == -91.0 ~ -1,
    x == -3.0 ~ -3,
    x == -1.0 ~ -1,
    TRUE ~ x
  )
  return(x)
}

# Function to harmonize missing codes for age 16
harmonize_age16 <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- case_when(
    x == -99.0 ~ -3,
    x == -92.0 ~ -9,
    x == -1.0 ~ -1,
    TRUE ~ x
  )
  return(x)
}

# Function to harmonize missing codes for age 17
harmonize_age17 <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- case_when(
    x == -996.0 ~ -1,
    x == -99.0 ~ -3,
    x == -92.0 ~ -9,
    x == -1.0 ~ -1,
    x == 1.0 ~ 1,
    x == 2.0 ~ 2,
    x == 3.0 ~ 3,
    x == 4.0 ~ 4,
    x == 5.0 ~ 5,
    x == 6.0 ~ 6,
    x == 7.0 ~ 7,
    x == 8.0 ~ 8,
    x == 9.0 ~ 9,
    x == 10.0 ~ 10,
    x == 11.0 ~ 11,
    x == 12.0 ~ 12,
    TRUE ~ x
  )
  return(x)
}

# Function to band income for ages 14-15
band_income_14_15 <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- case_when(
    x == -999.0 ~ -2,
    x == -992.0 ~ -8,
    x == -99.0 ~ -3,
    x == -94.0 ~ -8,
    x == -92.0 ~ -9,
    x == -91.0 ~ -1,
    x == -3.0 ~ -3,
    x == -1.0 ~ -1,
    x == 1.0 ~ 1,
    x == 2.0 ~ 2,
    x == 3.0 ~ 3,
    x == 4.0 ~ 4,
    x == 5.0 ~ 5,
    x == 6.0 ~ 6,
    x == 7.0 ~ 7,
    x == 8.0 ~ 8,
    x == 9.0 ~ 9,
    x == 10.0 ~ 10,
    x == 11.0 ~ 11,
    x == 12.0 ~ 12,
    TRUE ~ -3
  )
  return(x)
}

# Combine datasets first
combined <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Process age 14 continuous income
inc14_cont <- combined$W1GrsswkHH
inc14_cont <- harmonize_age14(inc14_cont)

# Process age 15 continuous income
inc15_cont <- combined$W2GrsswkHH
inc15_cont <- harmonize_age15(inc15_cont)

# Band age 14 and 15 income
inc14_banded <- band_income_14_15(inc14_cont)
inc15_banded <- band_income_14_15(inc15_cont)

# Process age 16 continuous income
inc16_cont <- combined$W3incestw
inc16_cont <- harmonize_age16(inc16_cont)
inc16_banded <- inc16_cont  # Use as factor for age 16

# Process age 17 continuous income
inc17_cont <- combined$w4IncEstW
inc17_cont <- harmonize_age17(inc17_cont)
inc17_banded <- inc17_cont  # Use as factor for age 17

# Create final dataset with required variables
final_data <- combined %>%
  mutate(
    NSID = NSID,
    incwhh14 = as.factor(inc14_banded),
    incwhh15 = as.factor(inc15_banded),
    incwhhcnt14 = inc14_cont,
    incwhhcnt15 = inc15_cont,
    incwhh16 = as.factor(inc16_banded),
    incwhh17 = as.factor(inc17_banded)
  )

# Write to CSV
write_csv(final_data, output_file)

# Print summary
print(head(final_data))
print(paste('Total rows:', nrow(final_data)))
print(paste('Total columns:', ncol(final_data)))