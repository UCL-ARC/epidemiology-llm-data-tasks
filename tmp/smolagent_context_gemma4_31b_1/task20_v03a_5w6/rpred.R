library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

load_data <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = 'numeric'))
}

# Since NSID is string, we need to handle that
load_data_with_nsid <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'numeric'))
}

# Load each explicitly
w1 <- load_data_with_nsid('wave_one_lsype_young_person_2020.tab')
w2 <- load_data_with_nsid('wave_two_lsype_young_person_2020.tab')
w3 <- load_data_with_nsid('wave_three_lsype_young_person_2020.tab')
w4 <- load_data_with_nsid('wave_four_lsype_young_person_2020.tab')
w6 <- load_data_with_nsid('wave_six_lsype_young_person_2020.tab')
w7 <- load_data_with_nsid('wave_seven_lsype_young_person_2020.tab')
w8 <- load_data_with_nsid('ns8_2015_self_completion.tab')
w9 <- load_data_with_nsid('ns9_2022_main_interview.tab')

# Merge datasets
full_df <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID') %>%
  full_join(w6, by = 'NSID') %>%
  full_join(w7, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9, by = 'NSID')

# Define drinking indicators for each wave
# S1: age 14 -> W1alceverYP == 1 AND W1alcmonYP == 1
# S2: age 15 -> W2alceverYP == 1
# S3: age 16 -> W3alceverYP == 1
# S4: age 17 -> W4AlcEverYP == 1
# S6: age 19 -> W6AlcEverYP == 1
# S7: age 20 -> W7AlcEverYP == 1
# S8: age 25 -> W8AUDIT1 > 1
# S9: age 32 -> W9AUDIT1 > 1

# Process missing values based on metadata (simplified here to focus on the logic)
# Standard missing values logic
# For drinking indicator, we need to know if it's: 1 (Drinking), 0 (Not Drinking), or NA (Missing)

get_drinking_status <- function(val, is_audit = FALSE) {
  if (is.na(val)) return(NA)
  if (is_audit) {
    if (val > 1) return(1) # Drinking
    if (val == 1) return(0) # Never
    return(NA) # Missing (codes -9, -8, etc)
  } else {
    if (val == 1) return(1) # Yes
    if (val == 2) return(0) # No
    return(NA) # Missing
  }
}

# Apply logic for each wave
full_df <- full_df %>%
  mutate(
    dr14 = case_when(
      W1alceverYP == 1 & W1alcmonYP == 1 ~ 1,
      W1alceverYP == 2 | W1alcmonYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    dr15 = case_when(
      W2alceverYP == 1 ~ 1,
      W2alceverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    dr16 = case_when(
      W3alceverYP == 1 ~ 1,
      W3alceverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    dr17 = case_when(
      W4AlcEverYP == 1 ~ 1,
      W4AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    dr19 = case_when(
      W6AlcEverYP == 1 ~ 1,
      W6AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    dr20 = case_when(
      W7AlcEverYP == 1 ~ 1,
      W7AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    dr25 = case_when(
      W8AUDIT1 > 1 ~ 1,
      W8AUDIT1 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    dr32 = case_when(
      W9AUDIT1 > 1 ~ 1,
      W9AUDIT1 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Calculate alcfst
calc_alcfst <- function(dr14, dr15, dr16, dr17, dr19, dr20, dr25, dr32) {
  ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
  vals <- c(dr14, dr15, dr16, dr17, dr19, dr20, dr25, dr32)
  
  # Find earliest age of drinking
  drinking_idx <- which(vals == 1)
  if (length(drinking_idx) > 0) return(ages[min(drinking_idx)])
  
  # If no drinking observed
  all_observed <- !is.na(vals)
  if (all(all_observed)) {
    # All observed and none are drinking
    return(99)
  } else {
    # No drinking observed, but at least one is missing
    # Only if no drinking is actually observed
    return(-8)
  }
}

full_df <- full_df %>%
  rowwise() %>%
  mutate(alcfst_val = calc_alcfst(dr14, dr15, dr16, dr17, dr19, dr20, dr25, dr32)) %>%
  ungroup()

# Convert to factor
levels_vals <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
levels_labels <- c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")

full_df <- full_df %>%
  mutate(alcfst = factor(alcfst_val, levels = levels_vals, labels = levels_labels))

# Final select
final_data <- full_df %>%
  select(NSID, alcfst)

# Write output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')
