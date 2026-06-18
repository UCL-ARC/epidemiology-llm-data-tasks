library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define file paths
path_prefix <- 'data/input/'

# Read files
wave_one <- read_delim(paste0(path_prefix, 'wave_one_lsype_young_person_2020.tab'), delim = '\t', show_col_types = FALSE)
wave_four <- read_delim(paste0(path_prefix, 'wave_four_lsype_young_person_2020.tab'), delim = '\t', show_col_types = FALSE)
wave_six <- read_delim(paste0(path_prefix, 'wave_six_lsype_young_person_2020.tab'), delim = '\t', show_col_types = FALSE)
ns8 <- read_delim(paste0(path_prefix, 'ns8_2015_derived.tab'), delim = '\t', show_col_types = FALSE)
ns9 <- read_delim(paste0(path_prefix, 'ns9_2022_derived_variables.tab'), delim = '\t', show_col_types = FALSE)

# Merge datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = 'NSID') %>%
  full_join(wave_six, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Helper functions to harmonise missing values
map_w6 <- function(x) {
  rec <- x
  rec[rec == -997] <- -2  # script error
  rec[rec == -97]  <- -9  # respondent declined self completion -> refusal
  rec[rec == -92]  <- -9  # refused
  rec[rec == -91]  <- -1  # not applicable
  rec[rec == -1]   <- -8  # don't know -> insufficient information
  rec[rec == -999] <- -2  # schedule not applicable
  rec[is.na(rec)] <- -3   # not asked
  return(rec)
}

map_w8 <- function(x) {
  rec <- x
  rec[rec == -9] <- -9
  rec[rec == -8] <- -8
  rec[rec == -1] <- -1
  rec[is.na(rec)] <- -3
  return(rec)
}

map_w9 <- function(x) {
  rec <- x
  rec[rec == -9] <- -9
  rec[rec == -8] <- -8
  rec[rec == -1] <- -1
  rec[is.na(rec)] <- -3
  return(rec)
}

# Collapse detailed adult categories for waves 8 and 9
collapse_w8_adult <- function(x) {
  rec <- x
  rec[is.na(rec)] <- -3
  missing <- rec %in% c(-9, -8, -1, -3)
  adult <- rec
  adult[!missing & rec == 1] <- 1
  adult[!missing & rec == 2] <- 2
  adult[!missing & rec %in% c(3,4)] <- 3
  adult[!missing & rec == 5] <- 4
  adult[!missing & rec %in% c(6,7,8,9)] <- 5
  adult[missing] <- rec[missing]
  return(adult)
}

collapse_w9_adult <- function(x) {
  rec <- x
  rec[is.na(rec)] <- -3
  missing <- rec %in% c(-9, -8, -1, -3)
  adult <- rec
  adult[!missing & rec == 1] <- 1
  adult[!missing & rec == 2] <- 2
  adult[!missing & rec %in% c(3,4)] <- 3
  adult[!missing & rec == 5] <- 4
  adult[!missing & rec %in% c(6,7,8)] <- 5
  adult[missing] <- rec[missing]
  return(adult)
}

# Create derived variables
final_df <- merged_data %>%
  mutate(
    partnr19   = map_w6(W6MarStatYP),
    partnr25   = map_w8(W8DMARSTAT),
    partnr32   = map_w9(W9DMARSTAT),
    partnradu25 = collapse_w8_adult(W8DMARSTAT),
    partnradu32 = collapse_w9_adult(W9DMARSTAT)
  ) %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Apply labels (names are labels, values are codes)
# partnr19
final_df$partnr19 <- labelled::labelled(final_df$partnr19,
  labels = c(
    'Single, that is never married' = 1,
    'Married' = 2,
    'Separated' = 3,
    'Divorced' = 4,
    'Widowed' = 5,
    'Refusal' = -9,
    'Insufficient information' = -8,
    'Prefer not to say' = -7,
    'Not asked' = -3,
    'Schedule not applicable' = -2,
    'Item not applicable' = -1
  ))

# partnr25
final_df$partnr25 <- labelled::labelled(final_df$partnr25,
  labels = c(
    'Single and never married or in a CP' = 1,
    'Married' = 2,
    'Separated but still legally married' = 3,
    'Divorced' = 4,
    'Widowed' = 5,
    'A Civil Partner' = 6,
    'Separated but still legally in a CP' = 7,
    'A former Civil Partner' = 8,
    'A surviving Civil Partner' = 9,
    'Refusal' = -9,
    'Insufficient information' = -8,
    'Not applicable' = -1,
    'Not asked' = -3
  ))

# partnr32
final_df$partnr32 <- labelled::labelled(final_df$partnr32,
  labels = c(
    'Single that is never married or never in a CP' = 1,
    'Married' = 2,
    'Divorced' = 3,
    'Legally separated' = 4,
    'Widowed' = 5,
    'A Civil Partner in a legally recognised CP' = 6,
    'A former Civil Partner (where CP legally dissolved)' = 7,
    'A surviving Civil Partner (where CP has died)' = 8,
    'Refusal' = -9,
    'Insufficient information' = -8,
    'Not applicable' = -1,
    'Not asked' = -3
  ))

# partnradu25
final_df$partnradu25 <- labelled::labelled(final_df$partnradu25,
  labels = c(
    'Single' = 1,
    'Married' = 2,
    'Separated or Divorced' = 3,
    'Widowed' = 4,
    'Civil Partner' = 5,
    'Refusal' = -9,
    'Insufficient information' = -8,
    'Not applicable' = -1,
    'Not asked' = -3
  ))

# partnradu32
final_df$partnradu32 <- labelled::labelled(final_df$partnradu32,
  labels = c(
    'Single' = 1,
    'Married' = 2,
    'Separated or Divorced' = 3,
    'Widowed' = 4,
    'Civil Partner' = 5,
    'Refusal' = -9,
    'Insufficient information' = -8,
    'Not applicable' = -1,
    'Not asked' = -3
  ))

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')
