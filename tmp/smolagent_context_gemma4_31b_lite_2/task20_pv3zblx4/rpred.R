library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
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

load_file <- function(fname) {
  readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(.default = 'c'))
}

# Load all and convert specific numeric columns to numeric for processing
# Since we only need a few variables, we will cast them later
data_list <- map(files, load_file)
names(data_list) <- files

# Merge datasets using full_join by NSID
cohort_df <- data_list %>% reduce(full_join, by = 'NSID')

# Helper to map missing values based on labels provided in metadata
# Standard: -9=Refusal, -8=DK, -7=Prefer not to say, -3=Not asked, -2=Not applicable/error, -1=Item not applicable
# Note: Metadata labels take priority

# Processing Alcohol consumption variables
# Wave 1 (14): W1alceverYP (1=Yes, 2=No), W1alcmonYP (1=Yes, 2=No)
# Wave 2 (15): W2alceverYP (1=Yes, 2=No)
# Wave 3 (16): W3alceverYP (1=Yes, 2=No)
# Wave 4 (17): W4AlcEverYP (1=Yes, 2=No)
# Wave 6 (19): W6AlcEverYP (1=Yes, 2=No)
# Wave 7 (20): W7AlcEverYP (1=Yes, 2=No)
# Wave 8 (25): W8AUDIT1 (1=Never, 2-5=Consumed)
# Wave 9 (32): W9AUDIT1 (1=Never, 2-5=Consumed)

# We need to determine the earliest age of alcohol consumption
# Age mapping: wave1->14, wave2->15, wave3->16, wave4->17, wave6->19, wave7->20, wave8->25, wave9->32

# Convert target columns to numeric for processing
vars_to_num <- c('W1alceverYP', 'W1alcmonYP', 'W2alceverYP', 'W3alceverYP', 'W4AlcEverYP', 'W6AlcEverYP', 'W7AlcEverYP', 'W8AUDIT1', 'W9AUDIT1')
cohort_df <- cohort_df %>% mutate(across(all_of(vars_to_num), as.numeric))

# Function to determine if person drank alcohol at a specific wave
# Returns 1 if Yes, 0 if No, NA if missing
get_drank_status <- function(col, type = 'ever') {
  if (type == 'ever') {
    # For waves 1-7: 1=Yes, 2=No
    case_when(
      col == 1 ~ 1, # Yes
      col == 2 ~ 0, # No
      TRUE ~ NA_real_
    )
  } else if (type == 'audit') {
    # For waves 8-9: 1=Never, 2-5=Consumed
    case_when(
      col >= 2 & col <= 5 ~ 1, # Consumed
      col == 1 ~ 0, # Never
      TRUE ~ NA_real_
    )
  }
}

# Create a helper table for each wave
cohort_df <- cohort_df %>%
  mutate(
    drank14 = get_drank_status(W1alceverYP),
    drank15 = get_drank_status(W2alceverYP),
    drank16 = get_drank_status(W3alceverYP),
    drank17 = get_drank_status(W4AlcEverYP),
    drank19 = get_drank_status(W6AlcEverYP),
    drank20 = get_drank_status(W7AlcEverYP),
    drank25 = get_drank_status(W8AUDIT1, 'audit'),
    drank32 = get_drank_status(W9AUDIT1, 'audit')
  )

# Determine alcfst
# 1. Earliest age where drank == 1
# 2. If’s’s always 0 or NA, but we have a 'No' response in any wave, we need to identify if they "never drank"
# The requirement says: "identifying if they also have never drunk alcohol (alcfst = 99)"

calculate_alcfst <- function(...) {
  vals <- c(...)
  # Find first wave where drank == 1
  first_yes <- which(vals == 1)[1]
  
  # Map index to age
  age_map <- c(14, 15, 16, 19, 20, 25, 32) # This is slightly wrong, I missed wave 3/4 indices
  # Let's use a direct list
  ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
  
  if (!is.na(first_yes)) {
    return(ages[first_yes])
  }
  
  # If no 'Yes', check if there is any 'No' (0)
  if (any(vals == 0, na.rm = TRUE)) {
    return(99)
  }
  
  return(NA_real_)
}

# Apply the calculation row-wise
cohort_df$alcfst <- apply(cohort_df[, c('drank14', 'drank15', 'drank16', 'drank17', 'drank19', 'drank20', 'drank25', 'drank32')], 1, calculate_alcfst)

# Handle missing values for alcfst: Convert NA to -3 (Not asked)
cohort_df$alcfst <- ifelse(is.na(cohort_df$alcfst), -3, cohort_df$alcfst)

# Final selection
final_df <- cohort_df %>% select(NSID, alcfst)

# Write to CSV
readr::write_csv(final_df, 'data/output/cleaned_data.csv')
