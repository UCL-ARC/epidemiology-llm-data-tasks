# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files from metadata
# Wave 6 (Age 19)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", 
                     delim = "\t", 
                     col_types = cols(.default = col_character()))

# Wave 8 (Age 25) - Derived variables
wave8 <- read_delim("data/input/ns8_2015_derived.tab", 
                    delim = "\t", 
                    col_types = cols(.default = col_character()))

# Wave 9 (Age 32) - Derived variables
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                    delim = "\t", 
                    col_types = cols(.default = col_character()))

# Merge all datasets by NSID
clean_data <- full_join(wave6, wave8, by = "NSID")
clean_data <- full_join(clean_data, wave9, by = "NSID")

# Extract the marital status variables as numeric
w6_marstat <- as.numeric(clean_data$W6MarStatYP)
w8_marstat <- as.numeric(clean_data$W8DMARSTAT)
w9_marstat <- as.numeric(clean_data$W9DMARSTAT)

# W6 (Age 19) - Create partnr19 (collapsed harmonised)
clean_data$partnr19 <- case_when(
  w6_marstat == -997 ~ -2,   # Script error
  w6_marstat == -97 ~ -2,    # Respondent declined self completion
  w6_marstat == -92 ~ -9,    # Refused
  w6_marstat == -91 ~ -1,    # Not applicable
  w6_marstat == -1 ~ -8,     # Don't know
  w6_marstat == 1 ~ 1,       # Single, that is never married
  w6_marstat == 2 ~ 2,       # Married
  w6_marstat == 3 ~ 3,       # Separated
  w6_marstat == 4 ~ 4,       # Divorced
  w6_marstat == 5 ~ 5,       # Widowed
  TRUE ~ NA_real_
)

# W8 (Age 25) - Create detailed adult variable partnradu25
clean_data$partnradu25 <- case_when(
  w8_marstat == -9 ~ -9,     # Refused
  w8_marstat == -8 ~ -8,     # Insufficient information
  w8_marstat == -1 ~ -1,     # Not applicable
  w8_marstat == 1 ~ 1,       # Single and never married or in a CP
  w8_marstat == 2 ~ 2,       # Married
  w8_marstat == 3 ~ 3,       # Separated but still legally married
  w8_marstat == 4 ~ 4,       # Divorced
  w8_marstat == 5 ~ 5,       # Widowed
  w8_marstat == 6 ~ 6,       # A Civil Partner
  w8_marstat == 7 ~ 7,       # Separated but still legally in a CP
  w8_marstat == 8 ~ 8,       # A former Civil Partner
  w8_marstat == 9 ~ 9,       # A surviving Civil Partner
  TRUE ~ NA_real_
)

# W8 (Age 25) - Create collapsed partnr25
clean_data$partnr25 <- case_when(
  w8_marstat == -9 ~ -9,     # Refused
  w8_marstat == -8 ~ -8,     # Insufficient information
  w8_marstat == -1 ~ -1,     # Not applicable
  w8_marstat == 1 ~ 1,       # Single and never married or in a CP
  w8_marstat == 2 ~ 2,       # Married
  w8_marstat %in% c(3, 7) ~ 3,   # Separated (both married and CP)
  w8_marstat %in% c(4, 8) ~ 4,   # Divorced (both married and CP)
  w8_marstat %in% c(5, 9) ~ 5,   # Widowed (both married and CP)
  w8_marstat == 6 ~ 2,           # Civil Partner (mapped to Married for harmonisation)
  TRUE ~ NA_real_
)

# W9 (Age 32) - Create detailed adult variable partnradu32
clean_data$partnradu32 <- case_when(
  w9_marstat == -9 ~ -9,     # Refused
  w9_marstat == -8 ~ -8,     # Insufficient information
  w9_marstat == 1 ~ 1,       # Single that is never married or never in a Civil Partnership
  w9_marstat == 2 ~ 2,       # Married
  w9_marstat == 3 ~ 3,       # Divorced
  w9_marstat == 4 ~ 4,       # Legally separated
  w9_marstat == 5 ~ 5,       # Widowed
  w9_marstat == 6 ~ 6,       # A Civil Partner in a legally recognised Civil Partnership
  w9_marstat == 7 ~ 7,       # A former Civil Partner
  w9_marstat == 8 ~ 8,       # A surviving Civil Partner
  TRUE ~ NA_real_
)

# W9 (Age 32) - Create collapsed partnr32
clean_data$partnr32 <- case_when(
  w9_marstat == -9 ~ -9,     # Refused
  w9_marstat == -8 ~ -8,     # Insufficient information
  w9_marstat == 1 ~ 1,       # Single
  w9_marstat == 2 ~ 2,       # Married
  w9_marstat == 3 ~ 4,       # Divorced (mapped to 4 for harmonisation)
  w9_marstat == 4 ~ 3,       # Legally separated (mapped to 3 for harmonisation)
  w9_marstat == 5 ~ 5,       # Widowed
  w9_marstat == 6 ~ 2,       # Civil Partner (mapped to Married)
  w9_marstat == 7 ~ 4,       # Former Civil Partner (mapped to Divorced)
  w9_marstat == 8 ~ 5,       # Surviving Civil Partner (mapped to Widowed)
  TRUE ~ NA_real_
)

# Keep only final variables: NSID and partnership variables
final_data <- clean_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Print summary
cat("Cleaned data created successfully.\n")
cat("Number of records:", nrow(final_data), "\n")
cat("Variables:", names(final_data), "\n")
