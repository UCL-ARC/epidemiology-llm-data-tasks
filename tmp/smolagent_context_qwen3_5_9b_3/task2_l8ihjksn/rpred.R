library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
w8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
w9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Full join all datasets
all_data <- full_join(w1, w2, by = 'NSID')
all_data <- full_join(all_data, w4, by = 'NSID')
all_data <- full_join(all_data, w8, by = 'NSID')
all_data <- full_join(all_data, w9, by = 'NSID')

# Map missing values to standard codes
# W1ethnic2YP: -999 to -1 are missing
all_data <- all_data %>%
  mutate(
    W1ethnic2YP = case_when(
      W1ethnic2YP >= -999 & W1ethnic2YP <= -1 ~ -3,
      TRUE ~ W1ethnic2YP
    )
  )

# W2ethnicYP: -999 to -1 are missing
all_data <- all_data %>%
  mutate(
    W2ethnicYP = case_when(
      W2ethnicYP >= -999 & W2ethnicYP <= -1 ~ -3,
      TRUE ~ W2ethnicYP
    )
  )

# w4ethnic2YP: -999 to -1 are missing
all_data <- all_data %>%
  mutate(
    w4ethnic2YP = case_when(
      w4ethnic2YP >= -999 & w4ethnic2YP <= -1 ~ -3,
      TRUE ~ w4ethnic2YP
    )
  )

# W8DETHN15: -9, -8, -1 are missing
all_data <- all_data %>%
  mutate(
    W8DETHN15 = case_when(
      W8DETHN15 == -9 | W8DETHN15 == -8 | W8DETHN15 == -1 ~ -3,
      TRUE ~ W8DETHN15
    )
  )

# W9DETHN15: -8 is missing
all_data <- all_data %>%
  mutate(
    W9DETHN15 = case_when(
      W9DETHN15 == -8 ~ -3,
      TRUE ~ W9DETHN15
    )
  )

# Create eth variable using earliest-valid-first logic
all_data <- all_data %>%
  mutate(
    eth = case_when(
      !is.na(W1ethnic2YP) & W1ethnic2YP > 0 ~ W1ethnic2YP,
      !is.na(W2ethnicYP) & W2ethnicYP > 0 ~ W2ethnicYP,
      !is.na(w4ethnic2YP) & w4ethnic2YP > 0 ~ w4ethnic2YP,
      !is.na(W8DETHN15) & W8DETHN15 > 0 ~ W8DETHN15,
      !is.na(W9DETHN15) & W9DETHN15 > 0 ~ W9DETHN15,
      TRUE ~ -3
    )
  )

# Create factor with labels based on detailed categories 1-16
all_data <- all_data %>%
  mutate(
    eth = factor(eth, 
      levels = c(-3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
      labels = c(
        'Missing - Not asked',
        'White - British',
        'White - Irish',
        'Any other White background',
        'Mixed - White and Black Caribbean',
        'Mixed - White and Black African',
        'Mixed - White and Asian',
        'Any other mixed background',
        'Indian',
        'Pakistani',
        'Bangladeshi',
        'Any other Asian background',
        'Black Caribbean',
        'Black African',
        'Any other Black background',
        'Chinese',
        'Any other ethnic background'
      )
    )
  )

# Keep only ID and final derived variable
final_data <- all_data %>%
  select(NSID, eth)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')