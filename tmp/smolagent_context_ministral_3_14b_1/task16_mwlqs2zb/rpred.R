
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files with explicit column types
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab",
                    delim = "\t",
                    col_types = cols(NSID = col_character(), W1GrsswkHH = col_double()))

wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab",
                    delim = "\t",
                    col_types = cols(NSID = col_character(), W2GrsswkHH = col_double()))

wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab",
                    delim = "\t",
                    col_types = cols(NSID = col_character(), W3incestw = col_double()))

wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab",
                    delim = "\t",
                    col_types = cols(NSID = col_character(), w4IncEstW = col_double()))

# Define missing value mappings for each wave
missing_mapping_wave1 <- function(x) {
  case_when(
    x == -999 | x == -99 | x == -94 | x == -91 | x == -992 ~ -3,
    x == -3 ~ -1,
    x == -1 ~ -8,
    x == -92 ~ -9,
    TRUE ~ x
  )
}

missing_mapping_wave2 <- function(x) {
  case_when(
    x == -999 | x == -99 | x == -94 | x == -91 | x == -992 ~ -3,
    x == -3 ~ -1,
    x == -1 ~ -8,
    x == -92 ~ -9,
    TRUE ~ x
  )
}

missing_mapping_wave3 <- function(x) {
  case_when(
    x == -999 | x == -99 | x == -94 | x == -91 ~ -3,
    x == -92 ~ -9,
    TRUE ~ x
  )
}

missing_mapping_wave4 <- function(x) {
  case_when(
    x == -996 ~ -3,
    x == -99 | x == -94 | x == -91 ~ -3,
    x == -92 ~ -9,
    TRUE ~ x
  )
}

# Map missing values for continuous variables (waves 1 and 2)
wave1$W1GrsswkHH <- missing_mapping_wave1(wave1$W1GrsswkHH)
wave2$W2GrsswkHH <- missing_mapping_wave2(wave2$W2GrsswkHH)

# Map missing values for banded variables (waves 3 and 4)
wave3$W3incestw <- missing_mapping_wave3(wave3$W3incestw)
wave4$w4IncEstW <- missing_mapping_wave4(wave4$w4IncEstW)

# Define income banding function for continuous values (waves 1 and 2)
band_income <- function(x) {
  case_when(
    x >= 0 & x <= 49 ~ 1,
    x > 49 & x <= 99 ~ 2,
    x > 99 & x <= 199 ~ 3,
    x > 199 & x <= 299 ~ 4,
    x > 299 & x <= 399 ~ 5,
    x > 399 & x <= 499 ~ 6,
    x > 499 & x <= 599 ~ 7,
    x > 599 & x <= 699 ~ 8,
    x > 699 & x <= 799 ~ 9,
    x > 799 & x <= 899 ~ 10,
    x > 899 & x <= 999 ~ 11,
    x >= 1000 ~ 12,
    TRUE ~ NA_integer_
  )
}

# Create banded income variables for waves 1 and 2
wave1$incwhh14_banded <- band_income(wave1$W1GrsswkHH)
wave2$incwhh15_banded <- band_income(wave2$W2GrsswkHH)

# Create labelled factors for banded income variables
inc_bands_labels <- c(
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

# Map banded variables to labelled factors
wave1$incwhh14 <- factor(wave1$incwhh14_banded, levels = 1:12, labels = inc_bands_labels)
wave2$incwhh15 <- factor(wave2$incwhh15_banded, levels = 1:12, labels = inc_bands_labels)

# Map wave 3 and 4 banded variables to labelled factors
wave3$incwhh16 <- factor(wave3$W3incestw, levels = 1:12, labels = inc_bands_labels)
wave4$incwhh17 <- factor(wave4$w4IncEstW, levels = 1:12, labels = inc_bands_labels)

# Select only the required variables and NSID
wave1_clean <- wave1 %>% select(NSID, W1GrsswkHH, incwhh14)
wave2_clean <- wave2 %>% select(NSID, W2GrsswkHH, incwhh15)
wave3_clean <- wave3 %>% select(NSID, W3incestw, incwhh16)
wave4_clean <- wave4 %>% select(NSID, w4IncEstW, incwhh17)

# Rename continuous variables for consistency
wave1_clean <- wave1_clean %>% rename(incwhhcnt14 = W1GrsswkHH)
wave2_clean <- wave2_clean %>% rename(incwhhcnt15 = W2GrsswkHH)

# Merge all datasets
merged_data <- full_join(wave1_clean, wave2_clean, by = "NSID") %>%
  full_join(wave3_clean, by = "NSID") %>%
  full_join(wave4_clean, by = "NSID")

# Convert NA to -3 for continuous variables
merged_data$incwhhcnt14 <- ifelse(is.na(merged_data$incwhhcnt14), -3, merged_data$incwhhcnt14)
merged_data$incwhhcnt15 <- ifelse(is.na(merged_data$incwhhcnt15), -3, merged_data$incwhhcnt15)

# Write the cleaned data to CSV
write_csv(merged_data, "data/output/cleaned_data.csv")
