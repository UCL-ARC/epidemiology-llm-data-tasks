library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Ensure output directory exists
if(!dir.exists('data/output')) dir.create('data/output', recursive = TRUE)

# File paths
file_wave1 <- 'data/input/wave_one_lsype_young_person_2020.tab'
file_wave4 <- 'data/input/wave_four_lsype_young_person_2020.tab'
file_wave6 <- 'data/input/wave_six_lsype_young_person_2020.tab'
file_ns8 <- 'data/input/ns8_2015_derived.tab'
file_ns9 <- 'data/input/ns9_2022_derived_variables.tab'

# Load datasets
wave1 <- read_delim(file_wave1, delim = '\t', show_col_types = FALSE)
wave4 <- read_delim(file_wave4, delim = '\t', show_col_types = FALSE)
wave6 <- read_delim(file_wave6, delim = '\t', show_col_types = FALSE)
ns8 <- read_delim(file_ns8, delim = '\t', show_col_types = FALSE)
ns9 <- read_delim(file_ns9, delim = '\t', show_col_types = FALSE)

# Merge all by NSID
merged <- full_join(wave1, wave4, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Derived partnership/marital status variables
cleaned <- merged %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      W6MarStatYP == -1.0 ~ -8,
      W6MarStatYP == -91.0 ~ -1,
      W6MarStatYP == -92.0 ~ -9,
      W6MarStatYP == -97.0 ~ -9,
      W6MarStatYP == -997.0 ~ -2,
      W6MarStatYP %in% c(-999, -998, -995) ~ -2,
      W6MarStatYP %in% c(-9, -8, -7, -3, -2, -1) ~ W6MarStatYP,
      is.na(W6MarStatYP) ~ -3,
      TRUE ~ -3
    ),
    partnr25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT %in% c(6,7,8,9) ~ 2,
      W8DMARSTAT == -9.0 ~ -9,
      W8DMARSTAT == -8.0 ~ -8,
      W8DMARSTAT == -1.0 ~ -1,
      is.na(W8DMARSTAT) ~ -3,
      TRUE ~ -3
    ),
    partnradu25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 6 ~ 6,
      W8DMARSTAT == 7 ~ 7,
      W8DMARSTAT == 8 ~ 8,
      W8DMARSTAT == 9 ~ 9,
      W8DMARSTAT == -9.0 ~ -9,
      W8DMARSTAT == -8.0 ~ -8,
      W8DMARSTAT == -1.0 ~ -1,
      is.na(W8DMARSTAT) ~ -3,
      TRUE ~ -3
    ),
    partnr32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 3,
      W9DMARSTAT == 4 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT %in% c(6,7,8) ~ 2,
      W9DMARSTAT == -9.0 ~ -9,
      W9DMARSTAT == -8.0 ~ -8,
      is.na(W9DMARSTAT) ~ -3,
      TRUE ~ -3
    ),
    partnradu32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 3,
      W9DMARSTAT == 4 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 6 ~ 6,
      W9DMARSTAT == 7 ~ 7,
      W9DMARSTAT == 8 ~ 8,
      W9DMARSTAT == -9.0 ~ -9,
      W9DMARSTAT == -8.0 ~ -8,
      is.na(W9DMARSTAT) ~ -3,
      TRUE ~ -3
    )
  ) %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output
write_csv(cleaned, 'data/output/cleaned_data.csv')
