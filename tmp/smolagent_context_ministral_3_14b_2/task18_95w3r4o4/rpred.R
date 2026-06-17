
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Define file paths
files <- list(
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_self_completion = 'data/input/ns8_2015_self_completion.tab',
  ns8_derived = 'data/input/ns8_2015_derived.tab',
  ns9_main_interview = 'data/input/ns9_2022_main_interview.tab',
  ns9_derived = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all files
wave_two_data <- read_delim(files$wave_two, delim = '\t')
wave_four_data <- read_delim(files$wave_four, delim = '\t')
ns8_self_completion_data <- read_delim(files$ns8_self_completion, delim = '\t')
ns8_derived_data <- read_delim(files$ns8_derived, delim = '\t')
ns9_main_interview_data <- read_delim(files$ns9_main_interview, delim = '\t')
ns9_derived_data <- read_delim(files$ns9_derived, delim = '\t')

# Function to harmonize missing values for a single column
harmonize_missing <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3
    x[x == -97] <- -9
    x[x == -92] <- -9
    x[x == -999] <- -2
    x[x == -998] <- -2
    x[x == -997] <- -2
    x[x == -995] <- -2
    x[x == -99] <- -3
    x[x == -96] <- -2
    x[x == -91] <- -1
    x[x == -8] <- -8
    x[x == -1] <- -1
  }
  return(x)
}

# Merge datasets by NSID
merged_data <- full_join(wave_two_data, wave_four_data, by = 'NSID') %>%
  full_join(ns8_self_completion_data, by = 'NSID') %>%
  full_join(ns8_derived_data, by = 'NSID') %>%
  full_join(ns9_main_interview_data, by = 'NSID') %>%
  full_join(ns9_derived_data, by = 'NSID')

# Define GHQ-12 columns explicitly for each wave
wave2_ghq_columns <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP',
                       'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP',
                       'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')

wave4_ghq_columns <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP',
                       'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP',
                       'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')

wave8_ghq_columns <- c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4',
                       'W8GHQ12_5', 'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8',
                       'W8GHQ12_9', 'W8GHQ12_10', 'W8GHQ12_11', 'W8GHQ12_12')

wave9_ghq_columns <- c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4',
                       'W9GHQ12_5', 'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8',
                       'W9GHQ12_9', 'W9GHQ12_10', 'W9GHQ12_11', 'W9GHQ12_12')

# Ensure columns exist
wave2_ghq_columns <- intersect(wave2_ghq_columns, names(merged_data))
wave4_ghq_columns <- intersect(wave4_ghq_columns, names(merged_data))
wave8_ghq_columns <- intersect(wave8_ghq_columns, names(merged_data))
wave9_ghq_columns <- intersect(wave9_ghq_columns, names(merged_data))

# Function to compute GHQ-12 sum
compute_ghq_sum <- function(data, columns) {
  if (length(columns) != 12) {
    message(paste('Expected 12 columns, got', length(columns)))
    return(rep(-3, nrow(data)))
  }

  # Harmonize all columns
  data_harm <- data %>% mutate(across(all_of(columns), harmonize_missing))

  # Compute the sum for each row
  row_sums <- sapply(1:nrow(data_harm), function(i) {
    row <- data_harm[i, columns]

    # Check if all items are NA
    if (all(is.na(row))) {
      return(-3)
    }

    # Check for invalid negative values
    if (any(row < 0 & !row %in% c(-3, -8, -9, -1, -2))) {
      return(-8)
    }

    # Sum valid values
    sum(row[row >= 0], na.rm = TRUE)
  })

  return(row_sums)
}

# Compute GHQ-12 scores
merged_data$ghqtl15 <- compute_ghq_sum(merged_data, wave2_ghq_columns)
merged_data$ghqtl17 <- compute_ghq_sum(merged_data, wave4_ghq_columns)
merged_data$ghqtl25 <- compute_ghq_sum(merged_data, wave8_ghq_columns)
merged_data$ghqtl32 <- compute_ghq_sum(merged_data, wave9_ghq_columns)

# Harmonize pre-derived scores
merged_data$ghq15 <- harmonize_missing(merged_data$W2ghq12scr)
merged_data$ghq17 <- harmonize_missing(merged_data$W4ghq12scr)
merged_data$ghq25 <- harmonize_missing(merged_data$W8DGHQSC)
merged_data$ghq32 <- harmonize_missing(merged_data$W9DGHQSC)

# Select required variables
final_data <- merged_data %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print summary to verify output
message('Output file created with the following variables:')
message(paste(names(final_data), collapse = ', '))
