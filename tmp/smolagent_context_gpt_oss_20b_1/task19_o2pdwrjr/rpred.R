# Load required libraries
library(readr)
library(dplyr)

# 1. Load all files listed in the metadata
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = cols())
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols())

# 2. Merge by NSID
merged <- wave_one %>%
  full_join(wave_four, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# 3. Derive BMI variables with standard missing handling
merged <- merged %>%
  mutate(
    bmi25 = case_when(
      is.na(W8DBMI) ~ -3,
      W8DBMI < 0 & !is.na(W8DBMI) ~ as.numeric(W8DBMI),
      TRUE ~ W8DBMI
    ),
    bmi32 = case_when(
      is.na(W9DBMI) ~ -3,
      W9DBMI < 0 & !is.na(W9DBMI) ~ as.numeric(W9DBMI),
      TRUE ~ W9DBMI
    )
  ) %>%
  select(NSID, bmi25, bmi32)

# 4. Write output
write_csv(merged, 'data/output/cleaned_data.csv')