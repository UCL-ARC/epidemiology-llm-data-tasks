library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Set paths
input_dir <- 'data/input'
output_dir <- 'data/output'

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

print('All files loaded successfully')

# Combine all waves
cleaned <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

print('All waves merged successfully')
print(paste('Total cases:', nrow(cleaned)))

# Create detailed teens variables (ages 14-20) with prefix hownteen
# Age 14 (W1hous12HH)
cleaned$hownteen14 <- cleaned$W1hous12HH

# Age 15 (W2Hous12HH)
cleaned$hownteen15 <- cleaned$W2Hous12HH

# Age 16 (W3hous12HH)
cleaned$hownteen16 <- cleaned$W3hous12HH

# Age 17 (W4Hous12HH)
cleaned$hownteen17 <- cleaned$W4Hous12HH

# Age 18 (W5Hous12HH)
cleaned$hownteen18 <- cleaned$W5Hous12HH

# Age 19 (W6Hous12YP)
cleaned$hownteen19 <- cleaned$W6Hous12YP

# Age 20 (W7Hous12YP)
cleaned$hownteen20 <- cleaned$W7Hous12YP

print('Detailed teens variables created')

# Create collapsed variables (ages 14-32) with prefix hown
# Rental categories need to be merged into single 'Rent it' category

# For wave 1 (age 14): rental categories are 4, 5, 6, 7 - map to 4
wave1_hown <- cleaned$W1hous12HH
wave1_hown[cleaned$W1hous12HH %in% c(4, 5, 6, 7)] <- 4
cleaned$hown14 <- wave1_hown

# For wave 2 (age 15): rental categories are 4, 5, 6, 7 - map to 4
wave2_hown <- cleaned$W2Hous12HH
wave2_hown[cleaned$W2Hous12HH %in% c(4, 5, 6, 7)] <- 4
cleaned$hown15 <- wave2_hown

# For wave 3 (age 16): rental categories are 4, 5, 6, 7 - map to 4
wave3_hown <- cleaned$W3hous12HH
wave3_hown[cleaned$W3hous12HH %in% c(4, 5, 6, 7)] <- 4
cleaned$hown16 <- wave3_hown

# For wave 4 (age 17): rental categories are 4, 5, 6, 7 - map to 4
wave4_hown <- cleaned$W4Hous12HH
wave4_hown[cleaned$W4Hous12HH %in% c(4, 5, 6, 7)] <- 4
cleaned$hown17 <- wave4_hown

# For wave 5 (age 18): rental is 2
wave5_hown <- cleaned$W5Hous12HH
wave5_hown[cleaned$W5Hous12HH == 2] <- 4  # map to Rent it
cleaned$hown18 <- wave5_hown

# For wave 6 (age 19): rental is 2
wave6_hown <- cleaned$W6Hous12YP
wave6_hown[cleaned$W6Hous12YP == 2] <- 4
cleaned$hown19 <- wave6_hown

# For wave 7 (age 20): rental is 2
wave7_hown <- cleaned$W7Hous12YP
wave7_hown[cleaned$W7Hous12YP == 2] <- 4
cleaned$hown20 <- wave7_hown

# For wave 8 (age 25): rental is 4 (already Rent it)
cleaned$hown25 <- cleaned$W8TENURE

# For wave 9 (age 32): rental is 4 (already Rent it)
cleaned$hown32 <- cleaned$W9DTENURE

print('Collapsed variables created')

# Write output
write_csv(cleaned, 'data/output/cleaned_data.csv')
print('Script completed successfully')