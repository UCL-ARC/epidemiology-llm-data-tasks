
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_five = 'data/input/wave_five_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  wave_eight = 'data/input/ns8_2015_derived.tab',
  wave_nine = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all files
load_file <- function(file_path) {
  read_delim(file_path, delim = '\t')
}

loaded_files <- map(files, load_file)

# Merge all datasets by NSID
merged_data <- reduce(loaded_files, full_join, by = 'NSID')

# Mapping functions for missing values
map_missing <- function(x) {
  x %>%
    mutate(across(everything(), ~ replace_na(., -3))) %>%
    mutate(
      across(
        c(W4empsYP, W5mainactYP, W6TCurrentAct, W7TCurrentAct, W8DACTIVITYC, W9DACTIVITYC),
        ~ case_when(
          . %in% c(-999, -998, -997, -995) ~ -2,
          . %in% -94 ~ -8,
          . %in% -92 ~ -9,
          . %in% -91 ~ -1,
          . %in% -99 ~ -3,
          . %in% -100 ~ -3,
          . %in% -97 ~ -3,
          TRUE ~ .
        )
      )
    )
}

# Define variable mapping for collapsed ecoact variables
map_ecoact17 <- function(x) {
  x %>%
    mutate(
      ecoact17 = case_when(
        W4empsYP %in% c(1, 2, 3, 4) ~ 1,  # Employed or training
        W4empsYP %in% c(5, 6, 7, 8) ~ 2,  # Education or family
        W4empsYP %in% c(9) ~ 3,            # Other
        TRUE ~ -3
      )
    )
}

map_ecoact18 <- function(x) {
  x %>%
    mutate(
      ecoact18 = case_when(
        W5mainactYP %in% c(3, 5, 6) ~ 1,  # Employed or training
        W5mainactYP %in% c(4, 9, 10) ~ 2, # Education or waiting
        W5mainactYP %in% c(7, 8, 11) ~ 3, # Unemployed or family
        TRUE ~ -3
      )
    )
}

map_ecoact19 <- function(x) {
  x %>%
    mutate(
      ecoact19 = case_when(
        W6TCurrentAct %in% c(3, 4, 5) ~ 1,  # Employed or training
        W6TCurrentAct %in% c(1, 2) ~ 2,     # Education
        W6TCurrentAct %in% c(7, 8, 9, 11) ~ 3, # Unemployed or family
        TRUE ~ -3
      )
    )
}

map_ecoact20 <- function(x) {
  x %>%
    mutate(
      ecoact20 = case_when(
        W7TCurrentAct %in% c(3, 4, 5) ~ 1,  # Employed or training
        W7TCurrentAct %in% c(1, 2) ~ 2,     # Education
        W7TCurrentAct %in% c(7, 8, 13) ~ 3,  # Unemployed or family
        TRUE ~ -3
      )
    )
}

map_ecoact25 <- function(x) {
  x %>%
    mutate(
      ecoact25 = case_when(
        W8DACTIVITYC %in% c(1, 2, 6, 7) ~ 1,  # Employed or training
        W8DACTIVITYC %in% c(5) ~ 2,           # Education
        W8DACTIVITYC %in% c(4, 9) ~ 3,        # Unemployed or family
        TRUE ~ -3
      )
    )
}

map_ecoact32 <- function(x) {
  x %>%
    mutate(
      ecoact32 = case_when(
        W9DACTIVITYC %in% c(1, 2, 6, 7) ~ 1,  # Employed or training
        W9DACTIVITYC %in% c(5) ~ 2,           # Education
        W9DACTIVITYC %in% c(4, 9) ~ 3,        # Unemployed or family
        TRUE ~ -3
      )
    )
}

# Define variable mapping for detailed ecoactadu variables
map_ecoactadu25 <- function(x) {
  x %>%
    mutate(
      ecoactadu25 = W8DACTIVITYC
    )
}

map_ecoactadu32 <- function(x) {
  x %>%
    mutate(
      ecoactadu32 = W9DACTIVITYC
    )
}

# Apply transformations
processed_data <- merged_data %>%
  map_missing() %>%
  map_ecoact17() %>%
  map_ecoact18() %>%
  map_ecoact19() %>%
  map_ecoact20() %>%
  map_ecoact25() %>%
  map_ecoact32() %>%
  map_ecoactadu25() %>%
  map_ecoactadu32() %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Create labeled factors for ecoact variables
ecoact_labels <- c(
  `1` = 'Employed or training',
  `2` = 'Education',
  `3` = 'Unemployed or family'
)

ecoactadu_labels <- c(
  `1` = 'Employee - in paid work',
  `2` = 'Self employed',
  `3` = 'In unpaid/voluntary work',
  `4` = 'Unemployed',
  `5` = 'Education: School/college/university',
  `6` = 'Apprenticeship',
  `7` = 'On govt scheme for employment training',
  `8` = 'Sick or disabled',
  `9` = 'Looking after home or family',
  `10` = 'Something else'
)

missing_labels <- c(
  `-9` = 'Refused',
  `-8` = 'Insufficient information',
  `-7` = 'Prefer not to say',
  `-3` = 'Not asked',
  `-2` = 'Schedule not applicable',
  `-1` = 'Not applicable'
)

# Apply labels
processed_data <- processed_data %>%
  mutate(
    ecoact17 = factor(ecoact17, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3), labels = c(missing_labels, ecoact_labels)),
    ecoact18 = factor(ecoact18, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3), labels = c(missing_labels, ecoact_labels)),
    ecoact19 = factor(ecoact19, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3), labels = c(missing_labels, ecoact_labels)),
    ecoact20 = factor(ecoact20, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3), labels = c(missing_labels, ecoact_labels)),
    ecoact25 = factor(ecoact25, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3), labels = c(missing_labels, ecoact_labels)),
    ecoact32 = factor(ecoact32, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3), labels = c(missing_labels, ecoact_labels)),
    ecoactadu25 = factor(ecoactadu25, levels = c(-9, -8, -7, -3, -2, -1, 1:10), labels = c(missing_labels, ecoactadu_labels)),
    ecoactadu32 = factor(ecoactadu32, levels = c(-9, -8, -7, -3, -2, -1, 1:10), labels = c(missing_labels, ecoactadu_labels))
  )

# Write output
write_csv(processed_data, 'data/output/cleaned_data.csv')
