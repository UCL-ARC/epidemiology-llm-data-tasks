library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
# The metadata lists 4 files. Even if some only contain NSID, they must be loaded to preserve the cohort frame.
# Corrected col_types usage: removing 'guess' shortcut which caused the error
file1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
file4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
file8 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
file9 <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge datasets using full_join by NSID
cohort <- file1 %>%
  full_join(file4, by = 'NSID') %>%
  full_join(file8, by = 'NSID') %>%
  full_join(file9, by = 'NSID')

# Mapping: wave8 -> 25, wave9 -> 32
# Variable W8DBMI corresponds to bmi25
# Variable W9DBMI corresponds to bmi32

# Cleaning function for BMI
clean_bmi <- function(var) {
  # Create a copy of the vector
  res <- var
  
  # Handle NAs: map to -3 as per requirements
  res[is.na(res)] <- -3
  
  # Metadata for W8DBMI and W9DBMI both map:
  # -9.0 -> Refused (-9)
  # -8.0 -> Insufficient information (-8)
  # -1.0 -> Not applicable (-1)
  # These already align with the standard codes requested in the general guidance
  
  return(res)
}

# Derive bmi25
bmi25 <- clean_bmi(cohort$W8DBMI)

# Derive bmi32
bmi32 <- clean_bmi(cohort$W9DBMI)

# Prepare final data frame
final_data <- cohort %>%
  select(NSID) %>%
  mutate(bmi25 = bmi25, bmi32 = bmi32)

# Write to CSV
readr::write_csv(final_data, 'data/output/cleaned_data.csv')
