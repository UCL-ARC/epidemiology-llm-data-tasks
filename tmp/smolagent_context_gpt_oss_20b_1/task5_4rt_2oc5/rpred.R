# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Function to map missing codes for wave 6 (W6MarStatYP)
map_missing_w6 <- function(x) {
  case_when(
    !is.na(x) & x %in% 1:5 ~ x,                                   # substantive values
    !is.na(x) & x == -997 ~ -2,   # Script error
    !is.na(x) & x == -97  ~ -8,   # Respondent declined self completion
    !is.na(x) & x == -92  ~ -9,   # Refused
    !is.na(x) & x == -91  ~ -1,   # Not applicable
    !is.na(x) & x == -1   ~ -8,   # Don’t know
    !is.na(x) & x == -99  ~ -3,   # Not asked
    !is.na(x) & x < 0      ~ -3,   # any other negative value treated as NOT asked
    TRUE ~ NA_real_
  )
}

# Function to map missing codes for wave 8 (W8DMARSTAT)
map_missing_w8 <- function(x) {
  case_when(
    !is.na(x) & x %in% 1:9 ~ x,    # substantive values
    !is.na(x) & x == -9 ~ -9,       # Refused
    !is.na(x) & x == -8 ~ -8,       # Insufficient information
    !is.na(x) & x == -1 ~ -1,       # Not applicable
    !is.na(x) & x < 0  ~ -3,        # any other negative treated as NOT asked
    TRUE ~ NA_real_
  )
}

# Function to map missing codes for wave 9 (W9DMARSTAT)
map_missing_w9 <- function(x) {
  case_when(
    !is.na(x) & x %in% 1:8 ~ x,    # substantive values
    !is.na(x) & x == -9 ~ -9,       # Refused
    !is.na(x) & x == -8 ~ -8,       # Insufficient information
    !is.na(x) & x < 0  ~ -3,        # any other negative treated as NOT asked
    TRUE ~ NA_real_
  )
}

# Function to collapse detailed adult partnership categories into harmonised set
collapse_partnr <- function(x) {
  case_when(
    !is.na(x) & x == 1 ~ 1,                        # Single
    !is.na(x) & x == 2 ~ 2,                        # Married
    !is.na(x) & x %in% c(3,7) ~ 3,                # Separated (including CP separated)
    !is.na(x) & x == 4 ~ 4,                        # Divorced
    !is.na(x) & x == 5 ~ 5,                        # Widowed
    !is.na(x) & x %in% c(6,8,9) ~ 6,              # Civil Partner (any CP status)
    # propagate existing missing codes directly
    !is.na(x) & x %in% c(-9,-8,-1,-2,-3,-7) ~ x,
    TRUE ~ NA_real_
  )
}

# Load all data files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
ns8  <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols())
ns9  <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols())

# Merge all datasets preserving all IDs (full join)
merged <- wave1 %>%
  full_join(wave4, by = "NSID", suffix = c(".w1", ".w4")) %>%
  full_join(wave6, by = "NSID") %>%
  full_join(ns8,  by = "NSID") %>%
  full_join(ns9,  by = "NSID")

# Derive variables
output <- merged %>%
  mutate(
    partnr19 = replace_na(map_missing_w6(W6MarStatYP), -3),
    partnradu25 = replace_na(map_missing_w8(W8DMARSTAT), -3),
    partnradu32 = replace_na(map_missing_w9(W9DMARSTAT), -3),
    partnr25 = replace_na(collapse_partnr(partnradu25), -3),
    partnr32 = replace_na(collapse_partnr(partnradu32), -3)
  ) %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write the cleaned data
write_csv(output, "data/output/cleaned_data.csv")
