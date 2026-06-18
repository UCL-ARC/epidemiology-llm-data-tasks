
# Load required libraries
library(readr)
library(dplyr)

# Define file paths
files <- list(
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_self = 'data/input/ns8_2015_self_completion.tab',
  ns8_derived = 'data/input/ns8_2015_derived.tab',
  ns9_main = 'data/input/ns9_2022_main_interview.tab',
  ns9_derived = 'data/input/ns9_2022_derived_variables.tab'
)

# Load datasets
wave2 <- read_delim(files$wave_two, delim = '\t')
wave4 <- read_delim(files$wave_four, delim = '\t')
ns8_self <- read_delim(files$ns8_self, delim = '\t')
ns8_derived <- read_delim(files$ns8_derived, delim = '\t')
ns9_main <- read_delim(files$ns9_main, delim = '\t')
ns9_derived <- read_delim(files$ns9_derived, delim = '\t')

# Merge all datasets
merged_data <- wave2 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8_self, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Function to create GHQ summed scores
create_ghq_summed <- function(data, vars, age) {
  existing_vars <- intersect(vars, names(data))
  if (length(existing_vars) == 12) {
    data$ghqtl <- rowSums(data[existing_vars], na.rm = TRUE)
    data$ghqtl[is.na(data$ghqtl) | rowSums(!is.na(data[existing_vars])) != 12] <- NA
    colname <- paste0('ghqtl', age)
    data[[colname]] <- data$ghqtl
    return(data)
  }
  return(data)
}

# Function to create GHQ caseness scores
create_ghq_caseness <- function(data, var, age) {
  if (var %in% names(data)) {
    colname <- paste0('ghq', age)
    data[[colname]] <- data[[var]]
    return(data)
  }
  return(data)
}

# GHQ variables for each wave
ghq_vars_15 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP',
                 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP',
                 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')

ghq_vars_17 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP',
                 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP',
                 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')

ghq_vars_25 <- c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4',
                 'W8GHQ12_5', 'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8',
                 'W8GHQ12_9', 'W8GHQ12_10', 'W8GHQ12_11', 'W8GHQ12_12')

ghq_vars_32 <- c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4',
                 'W9GHQ12_5', 'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8',
                 'W9GHQ12_9', 'W9GHQ12_10', 'W9GHQ12_11', 'W9GHQ12_12')

# Create GHQ scores for each wave
merged_data <- create_ghq_summed(merged_data, ghq_vars_15, 15)
merged_data <- create_ghq_summed(merged_data, ghq_vars_17, 17)
merged_data <- create_ghq_summed(merged_data, ghq_vars_25, 25)
merged_data <- create_ghq_summed(merged_data, ghq_vars_32, 32)

# Create caseness scores
merged_data <- create_ghq_caseness(merged_data, 'W2ghq12scr', 15)
merged_data <- create_ghq_caseness(merged_data, 'W4ghq12scr', 17)
merged_data <- create_ghq_caseness(merged_data, 'W8DGHQSC', 25)
merged_data <- create_ghq_caseness(merged_data, 'W9DGHQSC', 32)

# Select final variables
final_vars <- c('NSID', 'ghqtl15', 'ghq15', 'ghqtl17', 'ghq17', 'ghqtl25', 'ghq25', 'ghqtl32', 'ghq32')
final_vars <- final_vars[final_vars %in% names(merged_data)]

# Write final output
write_csv(merged_data %>% select(all_of(final_vars)), 'data/output/cleaned_data.csv')
