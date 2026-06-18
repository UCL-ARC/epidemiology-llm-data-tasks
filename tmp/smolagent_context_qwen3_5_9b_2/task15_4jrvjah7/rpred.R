library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files from data/input/
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets by NSID using full_join
data <- full_join(wave1, wave4, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Define value labels for income bands (16 substantive categories)
inc_labels <- c(
  'less than 25',
  '25 to 50',
  '50 to 90',
  '90 to 140',
  '140 to 240',
  '240 to 300',
  '300 to 350',
  '350 to 400',
  '400 to 500',
  '500 to 600',
  '600 to 700',
  '700 to 800',
  '800 to 900',
  '900 to 1200',
  '1200 to 1400',
  'more than 1400'
)

# Derive inc25 (age 25) from W8DINCB
# Missing value handling: -1/-1.0 -> -1 (Not applicable), NA -> -3 (Not asked/not interviewed)
data <- data %>%
  mutate(
    inc25 = ifelse(W8DINCB %in% c(-1, -1.0), -1,
                    ifelse(is.na(W8DINCB), -3, W8DINCB))
  )

# Derive inc32 (age 32) from W9DINCB
# Same missing value handling
data <- data %>%
  mutate(
    inc32 = ifelse(W9DINCB %in% c(-1, -1.0), -1,
                    ifelse(is.na(W9DINCB), -3, W9DINCB))
  )

# Convert to character for labeling, then back to numeric if needed
data <- data %>%
  mutate(
    inc25_char = case_when(
      inc25 == -1 ~ 'Not applicable',
      inc25 == -3 ~ 'Not asked/not interviewed',
      TRUE ~ as.character(inc25)
    ),
    inc32_char = case_when(
      inc32 == -1 ~ 'Not applicable',
      inc32 == -3 ~ 'Not asked/not interviewed',
      TRUE ~ as.character(inc32)
    )
  ) %>%
  mutate(
    inc25_lbl = case_when(
      inc25 == -1 ~ 'Not applicable',
      inc25 == -3 ~ 'Not asked/not interviewed',
      TRUE ~ paste('Income band', inc25)
    ),
    inc32_lbl = case_when(
      inc32 == -1 ~ 'Not applicable',
      inc32 == -3 ~ 'Not asked/not interviewed',
      TRUE ~ paste('Income band', inc32)
    )
  )

# Use labelled package to create value labels
data <- data %>%
  mutate(
    inc25_lbl = as_factor(inc25_lbl),
    inc32_lbl = as_factor(inc32_lbl)
  )

# Write cleaned data to CSV with only NSID and derived income variables
write_csv(data %>% select(NSID, inc25, inc32, inc25_lbl, inc32_lbl), 'data/output/cleaned_data.csv')

# Print summary
print('Script completed successfully')
print(paste('Rows:', nrow(data)))
print(paste('Columns:', ncol(data)))