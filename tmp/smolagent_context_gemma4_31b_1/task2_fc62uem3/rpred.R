library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge datasets
cohort <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Define a function to map missing values according to general guidance and metadata labels
map_missing <- function(var, wave_id) {
  var[is.na(var)] <- -3
  
  if (wave_id == 'w1') {
    var <- case_when(
      var == -999 ~ -2,
      var == -94  ~ -8,
      var == -92  ~ -9,
      var == -91  ~ -1,
      var == -1   ~ -8,
      TRUE        ~ var
    )
  } else if (wave_id == 'w2') {
    var <- case_when(
      var == -998 ~ -3,
      var == -997 ~ -2,
      var == -995 ~ -2,
      var == -99  ~ -3,
      var == -92  ~ -9,
      var == -91  ~ -1,
      var == -1   ~ -8,
      TRUE        ~ var
    )
  } else if (wave_id == 'w4') {
    var <- case_when(
      var == -94  ~ -8,
      var == -1   ~ -8,
      TRUE        ~ var
    )
  } else if (wave_id == 'w8') {
    var <- case_when(
      var == -9 ~ -9,
      var == -8 ~ -8,
      var == -1 ~ -1,
      TRUE      ~ var
    )
  } else if (wave_id == 'w9') {
    var <- case_when(
      var == -8 ~ -8,
      TRUE      ~ var
    )
  }
  return(var)
}

# Pre-process variables
cohort <- cohort %>%
  mutate(
    w1_eth_clean = map_missing(W1ethnic2YP, 'w1'),
    w2_eth_clean = map_missing(W2ethnicYP, 'w2'),
    w4_eth_clean = map_missing(w4ethnic2YP, 'w4'),
    w8_eth_clean = map_missing(W8DETHN15, 'w8'),
    w9_eth_clean = map_missing(W9DETHN15, 'w9')
  )

# Earliest valid positive response logic
cohort <- cohort %>%
  mutate(eth = case_when(
    w1_eth_clean >= 1 & w1_eth_clean <= 16 ~ w1_eth_clean,
    w2_eth_clean >= 1 & w2_eth_clean <= 16 ~ w2_eth_clean,
    w4_eth_clean >= 1 & w4_eth_clean <= 16 ~ w4_eth_clean,
    w8_eth_clean >= 1 & w8_eth_clean <= 16 ~ w8_eth_clean,
    w9_eth_clean >= 1 & w9_eth_clean <= 16 ~ w9_eth_clean,
    w1_eth_clean != -3 ~ w1_eth_clean,
    w2_eth_clean != -3 ~ w2_eth_clean,
    w4_eth_clean != -3 ~ w4_eth_clean,
    w8_eth_clean != -3 ~ w8_eth_clean,
    w9_eth_clean != -3 ~ w9_eth_clean,
    TRUE ~ -3
  ))

# Labels for eth
eth_labels <- c(
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
  '16' = 'Any other ethnic background',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Apply labels as a factor
cohort$eth <- factor(cohort$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

# Final Selection
final_data <- cohort %>%
  select(NSID, eth)

write_csv(final_data, 'data/output/cleaned_data.csv')
