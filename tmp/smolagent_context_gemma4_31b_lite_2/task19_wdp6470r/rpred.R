library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load files
# All listed files must be loaded to preserve the full cohort frame
wave1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave8_derived <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave9_derived <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Ensure NSID is character for merging
wave1$NSID <- as.character(wave1$NSID)
wave4$NSID <- as.character(wave4$NSID)
wave8_derived$NSID <- as.character(wave8_derived$NSID)
wave9_derived$NSID <- as.character(wave9_derived$NSID)

# Merge datasets using full_join by NSID
cohort_frame <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8_derived, by = 'NSID') %>%
  full_join(wave9_derived, by = 'NSID')

# Process BMI for age 25 (Wave 8)
# Source variable: W8DBMI
# Missing values: -9 (Refused), -8 (Insufficient information), -1 (Not applicable)
# General guidance: R NA to -3 (Not asked)
cohort_frame <- cohort_frame %>%
  mutate(
    W8DBMI_num = as.numeric(W8DBMI),
    bmi25 = case_when(
      is.na(W8DBMI_num) ~ -3,
      W8DBMI_num == -9.0 ~ -9,
      W8DBMI_num == -8.0 ~ -8,
      W8DBMI_num == -1.0 ~ -1,
      TRUE ~ W8DBMI_num
    )
  )

# Process BMI for age 32 (Wave 9)
# Source variable: W9DBMI
# Missing values: -9 (Refused), -8 (Insufficient information), -1 (Not applicable)
# General guidance: R NA to -3 (Not asked)
cohort_frame <- cohort_frame %>%
  mutate(
    W9DBMI_num = as.numeric(W9DBMI),
    bmi32 = case_when(
      is.na(W9DBMI_num) ~ -3,
      W9DBMI_num == -9.0 ~ -9,
      W9DBMI_num == -8.0 ~ -8,
      W9DBMI_num == -1.0 ~ -1,
      TRUE ~ W9DBMI_num
    )
  )

# Final selection
final_data <- cohort_frame %>%
  select(NSID, bmi25, bmi32)

# Write output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')