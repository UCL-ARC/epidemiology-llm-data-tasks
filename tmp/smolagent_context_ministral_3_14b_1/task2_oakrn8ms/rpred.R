
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Load datasets from specified files
wave1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- readr::read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8_derived <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_derived <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets by NSID to ensure full cohort frame
merged_data <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Define the priority order for deriving the 'eth' variable
priority_order <- c('W1ethnic2YP', 'W2ethnicYP', 'w4ethnic2YP', 'W8DETHN15', 'W9DETHN15')

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(x == -999 | x == -998 | x == -997 | x == -995 | x == -9 | x == -8, -2, x)
    x <- ifelse(x == -94 | x == -92, -9, x)
    x <- ifelse(x == -91, -1, x)
    x <- ifelse(x == -99, -3, x)
  }
  return(x)
}

# Apply missing value mapping to each source variable
for (var in priority_order) {
  if (var %in% colnames(merged_data)) {
    merged_data[[var]] <- map_missing_values(merged_data[[var]])
  }
}

# Create the consolidated 'eth' variable using earliest valid response
merged_data <- merged_data %>%
  mutate(
    eth = coalesce(
      ifelse(W1ethnic2YP > 0 & W1ethnic2YP <= 16, W1ethnic2YP, NA_real_),
      ifelse(W2ethnicYP > 0 & W2ethnicYP <= 16, W2ethnicYP, NA_real_),
      ifelse(w4ethnic2YP > 0 & w4ethnic2YP <= 16, w4ethnic2YP, NA_real_),
      ifelse(W8DETHN15 > 0 & W8DETHN15 <= 16, W8DETHN15, NA_real_),
      ifelse(W9DETHN15 > 0 & W9DETHN15 <= 16, W9DETHN15, NA_real_)
    )
  )

# Convert NA to -3 (Not asked at the fieldwork stage / not interviewed)
merged_data$eth[is.na(merged_data$eth)] <- -3

# Define the labels for the 'eth' variable
eth_labels <- c(
  '-9' = 'Refusal',
  '-8' = 'Insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable',
  '1' = 'White - British',
  '2' = 'White - Irish',
  '3' = 'Any other White background',
  '4' = 'Mixed - White and Black Caribbean',
  '5' = 'Mixed - White and Black African',
  '6' = 'Mixed - White and Asian',
  '7' = 'Any other mixed background',
  '8' = 'Indian',
  '9' = 'Pakistani',
  '10' = 'Bangladeshi',
  '11' = 'Any other Asian background',
  '12' = 'Black Caribbean',
  '13' = 'Black African',
  '14' = 'Any other Black background',
  '15' = 'Chinese',
  '16' = 'Any other ethnic background'
)

# Create a factor with the correct levels and labels
merged_data$eth <- factor(
  merged_data$eth,
  levels = names(eth_labels),
  labels = eth_labels
)

# Keep only NSID and the derived 'eth' variable
final_data <- merged_data %>%
  select(NSID, eth)

# Write the final cleaned data to CSV
readr::write_csv(final_data, 'data/output/cleaned_data.csv')
