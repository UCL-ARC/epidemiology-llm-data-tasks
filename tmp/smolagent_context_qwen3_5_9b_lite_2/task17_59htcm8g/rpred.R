library(haven)
library(dplyr)
library(readr)

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Wave 2 (Age 15): Convert IMDRSCORE to imd15
# User missing: -999 to -1.0 maps to -2, -94 maps to -8
wave2_imd15 <- wave2 %>%
  mutate(imd15 = case_when(
    IMDRSCORE == -94.0 ~ -8,
    IMDRSCORE >= -999 & IMDRSCORE <= -1 ~ -2,
    is.na(IMDRSCORE) ~ -3,
    TRUE ~ as.integer(IMDRSCORE)
  ))

# Wave 3 (Age 16): Convert IMDRSCORE to imd16
# Same missing value mapping as wave 2
wave3_imd16 <- wave3 %>%
  mutate(imd16 = case_when(
    IMDRSCORE == -94.0 ~ -8,
    IMDRSCORE >= -999 & IMDRSCORE <= -1 ~ -2,
    is.na(IMDRSCORE) ~ -3,
    TRUE ~ as.integer(IMDRSCORE)
  ))

# Wave 9 (Age 32): W9DIMDD is already a decile (1-10)
# Missing values: -8 (insufficient info), -8 to None
ns9_imd32 <- ns9 %>%
  mutate(imd32 = case_when(
    W9DIMDD == -8 ~ -8,
    W9DIMDD >= 1 & W9DIMDD <= 10 ~ as.numeric(W9DIMDD),
    TRUE ~ -3
  ))

# Merge all datasets by NSID
# Start with wave1 (largest cohort)
full_data <- wave1
full_data <- full_data %>%
  full_join(wave2_imd15, by = 'NSID') %>%
  full_join(wave3_imd16, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns9_imd32, by = 'NSID')

# Write output
cleaned_data <- full_data
write_csv(cleaned_data, 'data/output/cleaned_data.csv')

# Verify output
cat('Output written successfully with', nrow(cleaned_data), 'rows\n')
