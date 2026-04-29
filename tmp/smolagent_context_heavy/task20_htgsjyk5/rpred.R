library(haven)
library(dplyr)
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
  readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(.default = 'numeric'))
}

# Note: NSID is string, but read_delim might guess numeric if only numbers. 
# Re-reading with specific col_types for NSID to be safe.
load_file_fixed <- function(fname) {
  readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'numeric'))
}

df1 <- load_file_fixed('wave_one_lsype_young_person_2020.tab')
df2 <- load_file_fixed('wave_two_lsype_young_person_2020.tab')
df3 <- load_file_fixed('wave_three_lsype_young_person_2020.tab')
df4 <- load_file_fixed('wave_four_lsype_young_person_2020.tab')
df6 <- load_file_fixed('wave_six_lsype_young_person_2020.tab')
df7 <- load_file_fixed('wave_seven_lsype_young_person_2020.tab')
df8 <- load_file_fixed('ns8_2015_self_completion.tab')
df9 <- load_file_fixed('ns9_2022_main_interview.tab')

# Merge datasets
full_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df6, by = 'NSID') %>%
  full_join(df7, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# 2. Identify Drinking Status per Wave
# Wave 1: Age 14. Special rule: W1alceverYP == 1 AND W1alcmonYP == 1
# Waves 2,3,4,6,7: Ever had alcoholic drink == 1
# Waves 8,9: AUDIT1 > 1 (Never is 1)

# Define a helper to check if a value is missing according to standard scheme
# Labels: -99 (not interviewed), -97 (refused), -92 (refused), -91 (NA), -1 (DK)
# We need to map these to standard: -9 (Refusal), -8 (DK), -7 (Prefer not to say), -3 (Not asked), -2 (NA), -1 (Item NA)

map_missing <- function(val, wave) {
  if (is.na(val)) return(-3)
  if (val == -99) return(-3) # YP not interviewed
  if (val == -97) return(-9) # Refused self completion
  if (val == -92) return(-9) # Refused
  if (val == -91) return(-1) # Not applicable
  if (val == -1) return(-8)  # Don't know
  return(val)
}

# Since we need to process row by row or column by column for the minimum age,
# let's create binary indicators: 1 = Drinking, 0 = Not Drinking, NA = Missing/Unknown

# W1
drink_14 <- with(full_df, ifelse(W1alceverYP == 1 & W1alcmonYP == 1, 1, 
                                 ifelse(W1alceverYP == 2 | W1alcmonYP == 2, 0, NA)))

# W2
drink_15 <- with(full_df, ifelse(W2alceverYP == 1, 1, 
                                 ifelse(W2alceverYP == 2, 0, NA)))

# W3
drink_16 <- with(full_df, ifelse(W3alceverYP == 1, 1, 
                                 ifelse(W3alceverYP == 2, 0, NA)))

# W4
drink_17 <- with(full_df, ifelse(W4AlcEverYP == 1, 1, 
                                 ifelse(W4AlcEverYP == 2, 0, NA)))

# W6
drink_19 <- with(full_df, ifelse(W6AlcEverYP == 1, 1, 
                                 ifelse(W6AlcEverYP == 2, 0, NA)))

# W7
drink_20 <- with(full_df, ifelse(W7AlcEverYP == 1, 1, 
                                 ifelse(W7AlcEverYP == 2, 0, NA)))

# W8
drink_25 <- with(full_df, ifelse(W8AUDIT1 > 1, 1, 
                                 ifelse(W8AUDIT1 == 1, 0, NA)))

# W9
drink_32 <- with(full_df, ifelse(W9AUDIT1 > 1, 1, 
                                 ifelse(W9AUDIT1 == 1, 0, NA)))

# Combine into a matrix for easier calculation
drink_matrix <- cbind(drink_14, drink_15, drink_16, drink_17, drink_19, drink_20, drink_25, drink_32)
ages <- c(14, 15, 16, 17, 19, 20, 25, 32)

alcfst_vec <- apply(drink_matrix, 1, function(row) {
  # Find first wave where drinking == 1
  drink_idx <- which(row == 1)
  if (length(drink_idx) > 0) {
    return(ages[drink_idx[1]])
  }
  
  # If no drinking, check if all observed are 0
  # Remove NAs
  observed <- row[!is.na(row)]
  if (length(observed) == 0) return(-8)
  if (all(observed == 0)) return(99)
  
  # Otherwise, some are missing and none are drinking
  return(-8)
})

# Convert to factor with labels
final_levels <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
final_labels <- c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")

alcfst_factor <- factor(alcfst_vec, levels = final_levels, labels = final_labels)

# Prepare output
output_df <- data.frame(NSID = full_df$NSID, alcfst = alcfst_factor)

readr::write_csv(output_df, 'data/output/cleaned_data.csv')
