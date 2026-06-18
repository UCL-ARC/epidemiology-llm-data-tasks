
library(dplyr)
library(readr)
library(purrr)

# Load the required files
wave2 <- readr::read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
ns8_self_completion <- readr::read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', show_col_types = FALSE)
ns9_main_interview <- readr::read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3
    x[x == -999] <- -2
    x[x == -998] <- -2
    x[x == -997] <- -2
    x[x == -995] <- -2
    x[x == -99] <- -3
    x[x == -97] <- -9
    x[x == -96] <- -2
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -9] <- -9
    x[x == -8] <- -8
    x[x == -3] <- -3
    x[x == -1] <- -1
  }
  return(x)
}

# Function to calculate GHQ-12 total score and caseness
calculate_ghq <- function(data, age) {
  ghq_vars <- NULL

  # Identify GHQ variables based on wave
  if (age == 15) {
    ghq_vars <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP',
                  'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP',
                  'W2wthlessYP', 'W2happyYP')
  } else if (age == 17) {
    ghq_vars <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP',
                  'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP',
                  'W4WthlessYP', 'W4HappyYP')
  } else if (age == 25) {
    ghq_vars <- c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4', 'W8GHQ12_5',
                  'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8', 'W8GHQ12_9', 'W8GHQ12_10',
                  'W8GHQ12_11', 'W8GHQ12_12')
  } else if (age == 32) {
    ghq_vars <- c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4', 'W9GHQ12_5',
                  'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8', 'W9GHQ12_9', 'W9GHQ12_10',
                  'W9GHQ12_11', 'W9GHQ12_12')
  }

  # Apply missing value mapping
  data <- data %>%
    mutate(across(all_of(ghq_vars), map_missing_values))

  # Calculate GHQ-12 total score (only if all values are non-negative)
  data <- data %>%
    mutate(ghqtl = rowSums(select(., all_of(ghq_vars)), na.rm = TRUE)) %>%
    mutate(ghqtl = case_when(
      all(select(., all_of(ghq_vars)) >= 0) ~ ghqtl,
      TRUE ~ NA_integer_
    )) %>%
    mutate(ghq = ifelse(ghqtl >= 4 & !is.na(ghqtl), 1, 0))

  # Assign output variable names
  data <- data %>%
    rename(!!paste0('ghqtl', age) := ghqtl,
           !!paste0('ghq', age) := ghq)

  return(data)
}

# Calculate GHQ scores for each wave
wave2_ghq <- calculate_ghq(wave2, 15)
wave4_ghq <- calculate_ghq(wave4, 17)
ns8_ghq <- calculate_ghq(ns8_self_completion, 25)
ns9_ghq <- calculate_ghq(ns9_main_interview, 32)

# Merge datasets
merged_data <- full_join(wave2_ghq, wave4_ghq, by = 'NSID')
merged_data <- full_join(merged_data, ns8_ghq, by = 'NSID')
merged_data <- full_join(merged_data, ns9_ghq, by = 'NSID')

# Select only the final derived variables
final_vars <- c('NSID', 'ghqtl15', 'ghq15', 'ghqtl17', 'ghq17', 'ghqtl25', 'ghq25', 'ghqtl32', 'ghq32')
output_data <- merged_data %>% select(all_of(final_vars))

# Write output to CSV
readr::write_csv(output_data, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Cleaned data written to data/output/cleaned_data.csv\n')
