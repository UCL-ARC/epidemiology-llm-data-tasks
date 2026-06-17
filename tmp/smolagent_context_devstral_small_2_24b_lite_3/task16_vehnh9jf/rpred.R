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

# Merge datasets using full_join by NSID
merged_data <- wave1 %>% 
  full_join(wave2, by = "NSID") %>% 
  full_join(wave3, by = "NSID") %>% 
  full_join(wave4, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refused
    var %in% c(-1, -1.0) ~ -8,    # Don't know
    var %in% c(-94, -94.0) ~ -8,  # Insufficient information
    var %in% c(-99, -99.0) ~ -3,  # Not interviewed
    var %in% c(-999, -999.0) ~ -2, # Missing in error / Not applicable
    var %in% c(-996, -996.0) ~ -2, # No parent in household
    var %in% c(-992, -992.0) ~ -9, # No information - work status questions refused
    var %in% c(-91, -91.0) ~ -1,  # Not applicable
    var %in% c(-3, -3.0) ~ -1,    # Not yet paid
    TRUE ~ var
  )
}

# Process each wave's income variable
# Wave 1 (Age 14)
merged_data <- merged_data %>% 
  mutate(inc_banded_14 = map_missing(W1GrsswkHH, "wave1"))

# Wave 2 (Age 15)
merged_data <- merged_data %>% 
  mutate(inc_banded_15 = map_missing(W2GrsswkHH, "wave2"))

# Wave 3 (Age 16)
merged_data <- merged_data %>% 
  mutate(inc_banded_16 = map_missing(W3incestw, "wave3"))

# Wave 4 (Age 17)
merged_data <- merged_data %>% 
  mutate(inc_banded_17 = map_missing(w4IncEstW, "wave4"))

# Continuous variables for ages 14 and 15
# For continuous variables, retain numeric values and map missing codes
merged_data <- merged_data %>% 
  mutate(inc_continuous_14 = case_when(
    W1GrsswkHH %in% c(-92, -92.0, -992, -992.0) ~ NA_real_,
    W1GrsswkHH %in% c(-1, -1.0, -94, -94.0) ~ NA_real_,
    W1GrsswkHH %in% c(-99, -99.0) ~ NA_real_,
    W1GrsswkHH %in% c(-999, -999.0, -91, -91.0, -3, -3.0) ~ NA_real_,
    TRUE ~ W1GrsswkHH
  ))

merged_data <- merged_data %>% 
  mutate(inc_continuous_15 = case_when(
    W2GrsswkHH %in% c(-92, -92.0, -992, -992.0) ~ NA_real_,
    W2GrsswkHH %in% c(-1, -1.0, -94, -94.0) ~ NA_real_,
    W2GrsswkHH %in% c(-99, -99.0) ~ NA_real_,
    W2GrsswkHH %in% c(-999, -999.0, -91, -91.0, -3, -3.0) ~ NA_real_,
    TRUE ~ W2GrsswkHH
  ))

# Select only the ID and derived variables
output_data <- merged_data %>% 
  select(NSID, inc_banded_14, inc_banded_15, inc_banded_16, inc_banded_17, inc_continuous_14, inc_continuous_15)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"