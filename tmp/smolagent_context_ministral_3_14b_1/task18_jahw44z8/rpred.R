
library(readr)
library(dplyr)
library(tidyr)

# Load all required files
wave_two <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8_self_completion <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_main_interview <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Function to harmonize missing values for pre-derived GHQ variables
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -97] <- -9
  x[x == -92] <- -9
  x[x == -99] <- -3
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -96] <- -8
  return(x)
}

# Function to compute GHQ item-summed scores for a specific wave
compute_ghq_sum <- function(df, ghq_items, prefix) {
  ghq_items_df <- df %>% select(all_of(ghq_items))

  # Check for negative values in any item
  negative_check <- sapply(ghq_items_df, function(x) any(x < 0, na.rm = TRUE))

  # Replace negative values with -8 and NA with -3
  ghq_items_df[ghq_items_df < 0] <- -8
  ghq_items_df[is.na(ghq_items_df)] <- -3

  # Compute the sum of all items, ignoring NAs
  ghq_sum <- rowSums(ghq_items_df, na.rm = TRUE)

  # Ensure all items are accounted for
  df <- df %>% mutate(!!prefix := ghq_sum)

  return(df)
}

# Merge datasets by NSID
merged_data <- full_join(wave_two, wave_four, by = 'NSID') %>%
  full_join(ns8_self_completion, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_main_interview, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Define GHQ items for each wave
ghq_items_w2 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP',
                  'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP',
                  'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')

ghq_items_w4 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP',
                  'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP',
                  'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')

ghq_items_w8 <- c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4',
                  'W8GHQ12_5', 'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8',
                  'W8GHQ12_9', 'W8GHQ12_10', 'W8GHQ12_11', 'W8GHQ12_12')

ghq_items_w9 <- c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4',
                  'W9GHQ12_5', 'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8',
                  'W9GHQ12_9', 'W9GHQ12_10', 'W9GHQ12_11', 'W9GHQ12_12')

# Compute GHQ item-summed scores for W2 (Age 15)
merged_data <- compute_ghq_sum(merged_data, ghq_items_w2, 'ghqtl15')

# Compute GHQ item-summed scores for W4 (Age 17)
merged_data <- compute_ghq_sum(merged_data, ghq_items_w4, 'ghqtl17')

# Harmonize and add pre-derived GHQ scores for W2 (Age 15)
merged_data <- merged_data %>% mutate(ghq15 = harmonize_missing(W2ghq12scr))

# Harmonize and add pre-derived GHQ scores for W4 (Age 17)
merged_data <- merged_data %>% mutate(ghq17 = harmonize_missing(W4ghq12scr))

# Compute GHQ item-summed scores for W8 (Age 25)
merged_data <- compute_ghq_sum(merged_data, ghq_items_w8, 'ghqtl25')

# Harmonize and add pre-derived GHQ scores for W8 (Age 25)
merged_data <- merged_data %>% mutate(ghq25 = harmonize_missing(W8DGHQSC))

# Compute GHQ item-summed scores for W9 (Age 32)
merged_data <- compute_ghq_sum(merged_data, ghq_items_w9, 'ghqtl32')

# Harmonize and add pre-derived GHQ scores for W9 (Age 32)
merged_data <- merged_data %>% mutate(ghq32 = harmonize_missing(W9DGHQSC))

# Ensure all required columns exist
required_columns <- c('NSID', 'ghqtl15', 'ghq15', 'ghqtl17', 'ghq17', 'ghqtl25', 'ghq25', 'ghqtl32', 'ghq32')
final_data <- merged_data %>% select(all_of(required_columns))

# Write the final output
write_csv(final_data, 'data/output/cleaned_data.csv', na = '')

# Print confirmation message
message('Cleaned data successfully written to data/output/cleaned_data.csv')
