
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave8_self <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all datasets by NSID
merged_data <- full_join(wave2, wave4, by = 'NSID')
merged_data <- full_join(merged_data, wave8_self, by = 'NSID')
merged_data <- full_join(merged_data, wave9_main, by = 'NSID')

# Define the GHQ-12 variables for each wave
ghq_vars_wave2 <- c(
  'W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP',
  'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP',
  'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP'
)

ghq_vars_wave4 <- c(
  'W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP',
  'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP',
  'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP'
)

ghq_vars_wave8 <- paste0('W8GHQ12_', 1:12)
ghq_vars_wave9 <- paste0('W9GHQ12_', 1:12)

# Function to calculate GHQ-12 summed scores and caseness
calculate_ghq <- function(data, ghq_vars, age_suffix) {
  # Calculate GHQ-12 summed score
  summed_name <- paste0('ghqtl', age_suffix)

  data <- data %>%
    mutate(!!summed_name := rowSums(across(all_of(ghq_vars), ~ ifelse(. >= 1 & . <= 4, ., NA)), na.rm = TRUE)) %>%
    mutate(!!summed_name := ifelse(is.na(!!sym(summed_name)), -3, !!sym(summed_name)))

  # Calculate GHQ-12 caseness (1 if score >= 4, else 0)
  caseness_name <- paste0('ghq', age_suffix)

  data <- data %>%
    mutate(!!caseness_name := ifelse(is.na(!!sym(summed_name)), NA,
                                    ifelse(!!sym(summed_name) >= 4, 1, 0))) %>%
    mutate(!!caseness_name := ifelse(is.na(!!sym(caseness_name)), -3, !!sym(caseness_name)))

  return(data)
}

# Calculate GHQ-12 scores for each wave
merged_data <- calculate_ghq(merged_data, ghq_vars_wave2, '15')
merged_data <- calculate_ghq(merged_data, ghq_vars_wave4, '17')
merged_data <- calculate_ghq(merged_data, ghq_vars_wave8, '25')
merged_data <- calculate_ghq(merged_data, ghq_vars_wave9, '32')

# Select only the ID variable and the derived GHQ variables
final_vars <- c('NSID', 'ghqtl15', 'ghq15', 'ghqtl17', 'ghq17', 'ghqtl25', 'ghq25', 'ghqtl32', 'ghq32')
final_data <- merged_data %>% select(all_of(final_vars))

# Write final output
write_csv(final_data, 'data/output/cleaned_data.csv')
