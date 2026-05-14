library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files and target variables
file_vars <- list(
  'wave_one_lsype_family_background_2020.tab' = c('NSID', 'W1hous12HH'),
  'wave_two_lsype_family_background_2020.tab' = c('NSID', 'W2Hous12HH'),
  'wave_three_lsype_family_background_2020.tab' = c('NSID', 'W3hous12HH'),
  'wave_four_lsype_family_background_2020.tab' = c('NSID', 'W4Hous12HH'),
  'wave_five_lsype_family_background_2020.tab' = c('NSID', 'W5Hous12HH', 'W5Hous12BHH', 'W5Hous12CHH'),
  'wave_six_lsype_young_person_2020.tab' = c('NSID', 'W6Hous12YP', 'W6Hous12bYP', 'W6Hous12cYP'),
  'wave_seven_lsype_young_person_2020.tab' = c('NSID', 'W7Hous12YP', 'W7Hous12bYP', 'W7Hous12cYP'),
  'ns8_2015_main_interview.tab' = c('NSID', 'W8TENURE'),
  'ns9_2022_derived_variables.tab' = c('NSID', 'W9DTENURE')
)

# Load only required columns and ensure NSID is treated as character to prevent join issues
load_reduced <- function(f, vars) {
  df <- read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(NSID = col_character(), .default = 'd'))
  df %>% select(all_of(vars))
}

# Load and merge
# Using distinct on NSID to prevent Cartesian product if duplicates exist
cleaned_data <- map2(names(file_vars), file_vars, function(f, v) {
  load_reduced(f, v) %>% distinct(NSID, .keep_all = TRUE)
}) %>% 
  reduce(full_join, by = 'NSID')

# Helper functions for processing
process_split_wave <- function(data, type_var, own_var, rent_var, age) {
  data <- data %>% mutate(!!paste0('hownteen', age) := case_when(
    !!sym(type_var) == 1 & !!sym(own_var) == 1 ~ 1,
    !!sym(type_var) == 1 & !!sym(own_var) == 2 ~ 2,
    !!sym(type_var) == 1 & !!sym(own_var) == 3 ~ 3,
    !!sym(type_var) == 1 & !!sym(own_var) == 4 ~ 8,
    !!sym(type_var) == 2 & !!sym(rent_var) == 1 ~ 4,
    !!sym(type_var) == 2 & !!sym(rent_var) == 2 ~ 5,
    !!sym(type_var) == 2 & !!sym(rent_var) == 3 ~ 6,
    !!sym(type_var) == 2 & !!sym(rent_var) == 4 ~ 7,
    !!sym(type_var) == 2 & !!sym(rent_var) == 5 ~ 8,
    !!sym(type_var) == 3 ~ 8,
    !!sym(type_var) == -92 ~ -9,
    !!sym(type_var) == -91 ~ -1,
    !!sym(type_var) == -1 ~ -8,
    !!sym(type_var) == -999 ~ -3,
    TRUE ~ -3
  ))
  
  hownteen_col <- paste0('hownteen', age)
  data <- data %>% mutate(!!paste0('hown', age) := case_when(
    !!sym(hownteen_col) == 1 ~ 1,
    !!sym(hownteen_col) == 2 ~ 2,
    !!sym(hownteen_col) == 3 ~ 3,
    !!sym(hownteen_col) %in% c(4, 5, 6) ~ 4,
    !!sym(hownteen_col) == 7 ~ 5,
    !!sym(hownteen_col) == 8 ~ 6,
    !!sym(hownteen_col) == -9 ~ -9,
    !!sym(hownteen_col) == -8 ~ -8,
    !!sym(hownteen_col) == -1 ~ -1,
    !!sym(hownteen_col) == -3 ~ -3,
    !!sym(hownteen_col) == -2 ~ -2,
    TRUE ~ -3
  ))
  return(data)
}

process_simple_wave <- function(data, var, age) {
  data <- data %>% mutate(!!paste0('hownteen', age) := case_when(
    !!sym(var) == 1 ~ 1, !!sym(var) == 2 ~ 2, !!sym(var) == 3 ~ 3,
    !!sym(var) == 4 ~ 4, !!sym(var) == 5 ~ 5, !!sym(var) == 6 ~ 6,
    !!sym(var) == 7 ~ 7, !!sym(var) == 8 ~ 8,
    !!sym(var) == -92 ~ -9, !!sym(var) == -91 ~ -1, !!sym(var) == -1 ~ -8,
    !!sym(var) == -999 ~ -3, TRUE ~ -3
  ))
  
  hownteen_col <- paste0('hownteen', age)
  data <- data %>% mutate(!!paste0('hown', age) := case_when(
    !!sym(hownteen_col) == 1 ~ 1, !!sym(hownteen_col) == 2 ~ 2, !!sym(hownteen_col) == 3 ~ 3,
    !!sym(hownteen_col) %in% c(4, 5, 6) ~ 4, !!sym(hownteen_col) == 7 ~ 5, !!sym(hownteen_col) == 8 ~ 6,
    !!sym(hownteen_col) == -9 ~ -9, !!sym(hownteen_col) == -8 ~ -8, !!sym(hownteen_col) == -1 ~ -1,
    !!sym(hownteen_col) == -3 ~ -3, !!sym(hownteen_col) == -2 ~ -2, TRUE ~ -3
  ))
  return(data)
}

# Process waves
cleaned_data <- process_simple_wave(cleaned_data, 'W1hous12HH', 14)
cleaned_data <- process_simple_wave(cleaned_data, 'W2Hous12HH', 15)
cleaned_data <- process_simple_wave(cleaned_data, 'W3hous12HH', 16)
cleaned_data <- process_simple_wave(cleaned_data, 'W4Hous12HH', 17)
cleaned_data <- process_split_wave(cleaned_data, 'W5Hous12HH', 'W5Hous12BHH', 'W5Hous12CHH', 18)
cleaned_data <- process_split_wave(cleaned_data, 'W6Hous12YP', 'W6Hous12bYP', 'W6Hous12cYP', 19)
cleaned_data <- process_split_wave(cleaned_data, 'W7Hous12YP', 'W7Hous12bYP', 'W7Hous12cYP', 20)

# Adult Waves
cleaned_data <- cleaned_data %>% mutate(
  hown25 = case_when(
    W8TENURE == 1 ~ 1, W8TENURE == 2 ~ 2, W8TENURE == 3 ~ 3,
    W8TENURE %in% c(4, 6) ~ 4, W8TENURE == 5 ~ 5, W8TENURE == 7 ~ 6,
    W8TENURE == -9 ~ -9, W8TENURE == -8 ~ -8, W8TENURE == -1 ~ -1,
    TRUE ~ -3
  ),
  hown32 = case_when(
    W9DTENURE == 1 ~ 1, W9DTENURE == 2 ~ 2, W9DTENURE == 3 ~ 3,
    W9DTENURE == 4 ~ 4, W9DTENURE == 5 ~ 5, W9DTENURE %in% c(6, 7) ~ 6,
    W9DTENURE == -8 ~ -8, TRUE ~ -3
  )
)

# Final selection
final_cols <- c('NSID', 
               paste0('hown', c(14, 15, 16, 17, 18, 19, 20, 25, 32)), 
               paste0('hownteen', c(14, 15, 16, 17, 18, 19, 20)))

cleaned_data_final <- cleaned_data %>% select(all_of(final_cols))

# Output
write.csv(cleaned_data_final, 'data/output/cleaned_data.csv', row.names = FALSE)