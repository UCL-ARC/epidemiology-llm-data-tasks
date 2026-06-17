library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load files
# Wave 1 (Age 14)
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = "c"))
# Wave 4 (Age 17)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = "c"))
# Wave 8 (Age 25) Derived
wave8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(NSID = "c", W8DBMI = "d", .default = "c"))
# Wave 9 (Age 32) Derived
wave9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(NSID = "c", W9DBMI = "d", .default = "c"))

# Merge datasets using full_join on NSID
# Start with wave1 as the base frame
merged_data <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8_derived, by = 'NSID') %>%
  full_join(wave9_derived, by = 'NSID')

# Process BMI variables
# Standard Missing-Value Codes:
# -9 = Refusal
# -8 = Don't know / insufficient information
# -1 = Item not applicable
# -3 = Not asked / NA

# BMI at age 25 (W8DBMI)
bmi25 <- merged_data$W8DBMI
# The metadata says: -9.0 Refused, -8.0 Insufficient information, -1.0 Not applicable
# These already align with standard codes. We just need to handle NAs.
bmi25[is.na(bmi25)] <- -3

# BMI at age 32 (W9DBMI)
bmi32 <- merged_data$W9DBMI
# The metadata says: -9.0 Refused, -8.0 Insufficient information, -1.0 Not applicable
bmi32[is.na(bmi32)] <- -3

# Create final dataframe
final_df <- data.frame(NSID = merged_data$NSID, bmi25 = bmi25, bmi32 = bmi32)

# Write to CSV
readr::write_csv(final_df, 'data/output/cleaned_data.csv')