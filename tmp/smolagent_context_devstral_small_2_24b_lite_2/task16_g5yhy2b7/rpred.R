library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(var, wave) {
  # Define mapping based on metadata
  if (wave == "wave1" || wave == "wave2") {
    var <- case_when(
      var == -999 ~ -2,  # Missing in error
      var == -992 ~ -2,  # No information - work status questions refused
      var == -99 ~ -3,   # HH not interviewed
      var == -94 ~ -8,   # Insufficient information
      var == -92 ~ -9,   # Refused
      var == -91 ~ -1,   # Not applicable
      var == -3 ~ -2,   # Not yet paid
      var == -1 ~ -8,   # Don't know
      TRUE ~ var
    )
  } else if (wave == "wave3" || wave == "wave4") {
    var <- case_when(
      var == -999 ~ -2,  # Missing in error
      var == -996 ~ -2,  # No parent in household
      var == -99 ~ -3,   # MP not interviewed
      var == -92 ~ -9,   # Refused
      var == -1 ~ -8,   # Don't know
      TRUE ~ var
    )
  }
  return(var)
}

# Harmonize missing values for each wave
merged_data$W1GrsswkHH <- harmonize_missing(merged_data$W1GrsswkHH, "wave1")
merged_data$W2GrsswkHH <- harmonize_missing(merged_data$W2GrsswkHH, "wave2")
merged_data$W3incestw <- harmonize_missing(merged_data$W3incestw, "wave3")
merged_data$w4IncEstW <- harmonize_missing(merged_data$w4IncEstW, "wave4")

# Create banded income variables for ages 14, 15, 16, and 17
merged_data$income_banded_14 <- merged_data$W1GrsswkHH
merged_data$income_banded_15 <- merged_data$W2GrsswkHH
merged_data$income_banded_16 <- merged_data$W3incestw
merged_data$income_banded_17 <- merged_data$w4IncEstW

# Create continuous income variables for ages 14 and 15
# For continuous variables, we need to convert banded values to midpoints
# Define a function to convert banded income to continuous values
convert_to_continuous <- function(banded_var) {
  continuous_var <- case_when(
    banded_var == 1 ~ 24.5,    # Up to £49
    banded_var == 2 ~ 74.5,    # £50 up to £99
    banded_var == 3 ~ 149.5,   # £100 up to £199
    banded_var == 4 ~ 249.5,   # £200 up to £299
    banded_var == 5 ~ 349.5,   # £300 up to £399
    banded_var == 6 ~ 449.5,   # £400 up to £499
    banded_var == 7 ~ 549.5,   # £500 up to £599
    banded_var == 8 ~ 649.5,   # £600 up to £699
    banded_var == 9 ~ 749.5,   # £700 up to £799
    banded_var == 10 ~ 849.5,  # £800 up to £899
    banded_var == 11 ~ 949.5,  # £900 up to £999
    banded_var == 12 ~ 1000,   # £1,000 or more
    TRUE ~ banded_var
  )
  return(continuous_var)
}

merged_data$income_continuous_14 <- convert_to_continuous(merged_data$income_banded_14)
merged_data$income_continuous_15 <- convert_to_continuous(merged_data$income_banded_15)

# Select only the required variables for output
output_data <- merged_data %>%
  select(NSID, income_banded_14, income_banded_15, income_banded_16, income_banded_17, income_continuous_14, income_continuous_15)

# Write the output CSV file
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"