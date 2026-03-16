library(readr)
library(dplyr)
library(purrr)
library(haven)
library(labelled)

# Input and output paths
input_dir <- paste0('data/input/')
output_dir <- paste0('data/output/')

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Load each dataset
cat('Loading wave 1...\n')
wave1 <- read_delim(paste0(input_dir, 'wave_one_lsype_young_person_2020.tab'), delim = '\t', col_types = cols(.default = 'c'))
cat('Loaded wave 1:', nrow(wave1), 'rows\n')

cat('Loading wave 4...\n')
wave4 <- read_delim(paste0(input_dir, 'wave_four_lsype_young_person_2020.tab'), delim = '\t', col_types = cols(NSID = col_character()))
cat('Loaded wave 4:', nrow(wave4), 'rows\n')

cat('Loading wave 5...\n')
wave5 <- read_delim(paste0(input_dir, 'wave_five_lsype_young_person_2020.tab'), delim = '\t', col_types = cols(NSID = col_character()))
cat('Loaded wave 5:', nrow(wave5), 'rows\n')

cat('Loading wave 6...\n')
wave6 <- read_delim(paste0(input_dir, 'wave_six_lsype_young_person_2020.tab'), delim = '\t', col_types = cols(NSID = col_character()))
cat('Loaded wave 6:', nrow(wave6), 'rows\n')

cat('Loading wave 7...\n')
wave7 <- read_delim(paste0(input_dir, 'wave_seven_lsype_young_person_2020.tab'), delim = '\t', col_types = cols(NSID = col_character()))
cat('Loaded wave 7:', nrow(wave7), 'rows\n')

cat('Loading wave 8...\n')
wave8 <- read_delim(paste0(input_dir, 'ns8_2015_derived.tab'), delim = '\t', col_types = cols(NSID = col_character()))
cat('Loaded wave 8:', nrow(wave8), 'rows\n')

cat('Loading wave 9...\n')
wave9 <- read_delim(paste0(input_dir, 'ns9_2022_derived_variables.tab'), delim = '\t', col_types = cols(NSID = col_character()))
cat('Loaded wave 9:', nrow(wave9), 'rows\n')

# Harmonize wave 4 (Age 17) - W4empsYP
wave4 <- wave4 %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP %in% c(-999, -998, -997, -995, -99) ~ -2,
      W4empsYP == -94 ~ -8,
      W4empsYP == -92 ~ -9,
      W4empsYP == -91 ~ -3,
      W4empsYP == 1 ~ 1,
      W4empsYP == 2 ~ 2,
      W4empsYP == 3 ~ 3,
      W4empsYP == 4 ~ 4,
      W4empsYP == 5 ~ 5,
      W4empsYP == 6 ~ 6,
      W4empsYP == 7 ~ 7,
      W4empsYP == 8 ~ 8,
      W4empsYP == 9 ~ 9,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-W4empsYP)

cat('Wave 4 harmonized: created ecoact17\n')

# Harmonize wave 5 (Age 18) - Detailed: W5mainactYP
wave5 <- wave5 %>%
  mutate(
    ecoactadu18 = case_when(
      W5mainactYP == -94 ~ -8,
      W5mainactYP %in% c(-999, -998, -997, -995, -99) ~ -2,
      W5mainactYP == -91 ~ -3,
      W5mainactYP == 1 ~ 1,
      W5mainactYP == 2 ~ 2,
      W5mainactYP == 3 ~ 3,
      W5mainactYP == 4 ~ 4,
      W5mainactYP == 5 ~ 5,
      W5mainactYP == 6 ~ 6,
      W5mainactYP == 7 ~ 7,
      W5mainactYP == 8 ~ 8,
      W5mainactYP == 9 ~ 9,
      W5mainactYP == 10 ~ 10,
      W5mainactYP == 11 ~ 11,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-W5mainactYP)

cat('Wave 5 harmonized: created ecoactadu18\n')

# Harmonize wave 6 (Age 19) - W6TCurrentAct
wave6 <- wave6 %>%
  mutate(
    ecoact19 = case_when(
      W6TCurrentAct %in% c(-999, -998, -997, -995, -99) ~ -2,
      W6TCurrentAct == -91 ~ -3,
      W6TCurrentAct == 1 ~ 1,
      W6TCurrentAct == 2 ~ 2,
      W6TCurrentAct == 3 ~ 3,
      W6TCurrentAct == 4 ~ 4,
      W6TCurrentAct == 5 ~ 5,
      W6TCurrentAct == 6 ~ 6,
      W6TCurrentAct == 7 ~ 7,
      W6TCurrentAct == 8 ~ 8,
      W6TCurrentAct == 9 ~ 9,
      W6TCurrentAct == 10 ~ 10,
      W6TCurrentAct == 11 ~ 11,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-W6TCurrentAct)

cat('Wave 6 harmonized: created ecoact19\n')

# Harmonize wave 7 (Age 20) - W7TCurrentAct
wave7 <- wave7 %>%
  mutate(
    ecoact20 = case_when(
      W7TCurrentAct %in% c(-999, -998, -997, -995, -99) ~ -2,
      W7TCurrentAct == -91 ~ -3,
      W7TCurrentAct == 1 ~ 1,
      W7TCurrentAct == 2 ~ 2,
      W7TCurrentAct == 3 ~ 3,
      W7TCurrentAct == 4 ~ 4,
      W7TCurrentAct == 5 ~ 5,
      W7TCurrentAct == 6 ~ 6,
      W7TCurrentAct == 7 ~ 7,
      W7TCurrentAct == 8 ~ 8,
      W7TCurrentAct == 9 ~ 9,
      W7TCurrentAct == 10 ~ 10,
      W7TCurrentAct == 11 ~ 11,
      W7TCurrentAct == 12 ~ 12,
      W7TCurrentAct == 13 ~ 13,
      W7TCurrentAct == 14 ~ 14,
      W7TCurrentAct == 15 ~ 15,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-W7TCurrentAct)

cat('Wave 7 harmonized: created ecoact20\n')

# Harmonize wave 8 (Age 25) - W8DACTIVITYC
wave8 <- wave8 %>%
  mutate(
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(-9, -8, -1) ~ NA_integer_,
      W8DACTIVITYC == 1 ~ 1,
      W8DACTIVITYC == 2 ~ 2,
      W8DACTIVITYC == 3 ~ 3,
      W8DACTIVITYC == 4 ~ 4,
      W8DACTIVITYC == 5 ~ 5,
      W8DACTIVITYC == 6 ~ 6,
      W8DACTIVITYC == 7 ~ 7,
      W8DACTIVITYC == 8 ~ 8,
      W8DACTIVITYC == 9 ~ 9,
      W8DACTIVITYC == 10 ~ 10,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-W8DACTIVITYC)

cat('Wave 8 harmonized: created ecoact25\n')

# Harmonize wave 9 (Age 32) - W9DACTIVITYC
wave9 <- wave9 %>%
  mutate(
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(-9, -8, -1) ~ NA_integer_,
      W9DACTIVITYC == 1 ~ 1,
      W9DACTIVITYC == 2 ~ 2,
      W9DACTIVITYC == 3 ~ 3,
      W9DACTIVITYC == 4 ~ 4,
      W9DACTIVITYC == 5 ~ 5,
      W9DACTIVITYC == 6 ~ 6,
      W9DACTIVITYC == 7 ~ 7,
      W9DACTIVITYC == 8 ~ 8,
      W9DACTIVITYC == 9 ~ 9,
      W9DACTIVITYC == 10 ~ 10,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-W9DACTIVITYC)

cat('Wave 9 harmonized: created ecoact32\n')

# Merge all waves by NSID
cat('\nStarting merge...\n')

final_data <- wave1

# Join wave4
final_data <- full_join(final_data, wave4, by = 'NSID')
cat('After wave1 + wave4:', nrow(final_data), 'rows\n')

# Join wave5
final_data <- full_join(final_data, wave5, by = 'NSID')
cat('After wave1 + wave4 + wave5:', nrow(final_data), 'rows\n')

# Join wave6
final_data <- full_join(final_data, wave6, by = 'NSID')
cat('After wave1 + wave4 + wave5 + wave6:', nrow(final_data), 'rows\n')

# Join wave7
final_data <- full_join(final_data, wave7, by = 'NSID')
cat('After wave1 + wave4 + wave5 + wave6 + wave7:', nrow(final_data), 'rows\n')

# Join wave8
final_data <- full_join(final_data, wave8, by = 'NSID')
cat('After wave1 + wave4 + wave5 + wave6 + wave7 + wave8:', nrow(final_data), 'rows\n')

# Join wave9
final_data <- full_join(final_data, wave9, by = 'NSID')
cat('After all waves:', nrow(final_data), 'rows\n')

# Check variables before selection
cat('\nAll variables before select:')
cat(paste(names(final_data), collapse = ', '), '\n')

# Select relevant variables: NSID + all harmonized ecoact variables
final_data <- final_data %>%
  select(NSID, matches('^ecoact.*$'))

cat('\nFinal variables:')
cat(paste(names(final_data), collapse = ', '), '\n')
cat('\nNumber of rows:', nrow(final_data), '\n')

# Convert harmonized variables to factors
final_data <- final_data %>%
  mutate(across(matches('^ecoact'), as_factor))

cat('\nConverting ecoact variables to factors...\n')

# Write output
write_csv(final_data, paste0(output_dir, 'cleaned_data.csv'))
cat('\nData written to', output_dir, 'cleaned_data.csv\n')
cat('Final output complete!\n')