library(haven)
library(dplyr)
library(readr)
library(purrr)
library(labelled)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', '\t')

# Create merged dataset
merged <- w1 %>%
  full_join(w4, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Create income variable for age 25 (Wave 8)
# W8DINCB has user_missing_values -1.0, maps to -1 (Not applicable)
# R NA maps to -3 (Not asked)
merged <- merged %>%
  mutate(
    inc25 = case_when(
      is.na(W8DINCB) ~ -3,
      W8DINCB == -1 ~ -1,
      W8DINCB == 1 ~ 1,
      W8DINCB == 2 ~ 2,
      W8DINCB == 3 ~ 3,
      W8DINCB == 4 ~ 4,
      W8DINCB == 5 ~ 5,
      W8DINCB == 6 ~ 6,
      W8DINCB == 7 ~ 7,
      W8DINCB == 8 ~ 8,
      W8DINCB == 9 ~ 9,
      W8DINCB == 10 ~ 10,
      W8DINCB == 11 ~ 11,
      W8DINCB == 12 ~ 12,
      W8DINCB == 13 ~ 13,
      W8DINCB == 14 ~ 14,
      W8DINCB == 15 ~ 15,
      W8DINCB == 16 ~ 16,
      TRUE ~ -3
    )
  )

# Create income variable for age 32 (Wave 9)
# Same mapping structure
merged <- merged %>%
  mutate(
    inc32 = case_when(
      is.na(W9DINCB) ~ -3,
      W9DINCB == -1 ~ -1,
      W9DINCB == 1 ~ 1,
      W9DINCB == 2 ~ 2,
      W9DINCB == 3 ~ 3,
      W9DINCB == 4 ~ 4,
      W9DINCB == 5 ~ 5,
      W9DINCB == 6 ~ 6,
      W9DINCB == 7 ~ 7,
      W9DINCB == 8 ~ 8,
      W9DINCB == 9 ~ 9,
      W9DINCB == 10 ~ 10,
      W9DINCB == 11 ~ 11,
      W9DINCB == 12 ~ 12,
      W9DINCB == 13 ~ 13,
      W9DINCB == 14 ~ 14,
      W9DINCB == 15 ~ 15,
      W9DINCB == 16 ~ 16,
      TRUE ~ -3
    )
  )

# Select only ID and final derived variables
final <- merged %>%
  select(NSID, inc25, inc32)

# Write output
write_csv(final, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
