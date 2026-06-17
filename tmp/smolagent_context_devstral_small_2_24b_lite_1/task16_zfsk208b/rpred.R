library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refusal
    var %in% c(-1, -1.0) ~ -8,    # Don't know / insufficient information
    var %in% c(-94, -94.0) ~ -8,  # Insufficient information
    var %in% c(-99, -99.0) ~ -3,  # Not interviewed
    var %in% c(-999, -999.0) ~ -2, # Missing in error / script error
    var %in% c(-996, -996.0) ~ -2, # No parent in household
    var %in% c(-992, -992.0) ~ -2, # No information - work status questions refused
    var %in% c(-91, -91.0) ~ -1,  # Not applicable
    var %in% c(-3, -3.0) ~ -1,   # Not yet paid
    TRUE ~ var
  )
}

# Process banded income variables for ages 14, 15, 16, and 17
merged_data <- merged_data %>%
  mutate(
    # Age 14 (wave1)
    income14 = map_missing(W1GrsswkHH, "wave1"),
    # Age 15 (wave2)
    income15 = map_missing(W2GrsswkHH, "wave2"),
    # Age 16 (wave3)
    income16 = map_missing(W3incestw, "wave3"),
    # Age 17 (wave4)
    income17 = map_missing(w4IncEstW, "wave4")
  )

# Process continuous income variables for ages 14 and 15
# For continuous variables, we need to map the banded values to their midpoints
map_to_continuous <- function(var) {
  case_when(
    var == 1 ~ 24.5,    # Up to £49
    var == 2 ~ 74.5,    # £50 up to £99
    var == 3 ~ 149.5,   # £100 up to £199
    var == 4 ~ 249.5,   # £200 up to £299
    var == 5 ~ 349.5,   # £300 up to £399
    var == 6 ~ 449.5,   # £400 up to £499
    var == 7 ~ 549.5,   # £500 up to £599
    var == 8 ~ 649.5,   # £600 up to £699
    var == 9 ~ 749.5,   # £700 up to £799
    var == 10 ~ 849.5,  # £800 up to £899
    var == 11 ~ 949.5,  # £900 up to £999
    var == 12 ~ 1000,   # £1,000 or more
    TRUE ~ var
  )
}

merged_data <- merged_data %>%
  mutate(
    # Continuous income for age 14
    income_cont14 = map_to_continuous(income14),
    # Continuous income for age 15
    income_cont15 = map_to_continuous(income15)
  )

# Select only the final derived variables and NSID
final_data <- merged_data %>%
  select(NSID, income14, income15, income16, income17, income_cont14, income_cont15)

# Write the output
write_csv(final_data, "data/output/cleaned_data.csv")

# Return the path to the output file
cat("Output written to data/output/cleaned_data.csv")