library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define mapping functions for each wave
map_wave4 <- function(x) {
  case_when(
    x == 1.0 | x == 2.0 ~ 1,  # In paid work
    x == 4.0 ~ 2,            # Apprenticeship / training
    x == 5.0 ~ 3,            # Education
    x == 3.0 ~ 4,            # Unemployed
    x == 6.0 ~ 5,            # Looking after home/family
    x == 7.0 | x == 8.0 | x == 9.0 ~ 6,  # Other
    x == -92.0 ~ -9,          # Refusal
    x == -94.0 ~ -8,          # Don't know
    x == -91.0 ~ -1,          # Not applicable
    x == -999.0 ~ -2,         # Script error
    TRUE ~ -3                 # Not interviewed
  )
}

map_wave5 <- function(x) {
  case_when(
    x == 3.0 ~ 1,            # In paid work
    x == 1.0 | x == 2.0 | x == 5.0 ~ 2,  # Apprenticeship / training
    x == 4.0 ~ 3,            # Education
    x == 7.0 ~ 4,            # Unemployed
    x == 8.0 ~ 5,            # Looking after home/family
    x == 6.0 | x == 9.0 | x == 10.0 | x == 11.0 ~ 6,  # Other
    x == -94.0 ~ -8,          # Don't know
    TRUE ~ -3                 # Not interviewed
  )
}

map_wave6 <- function(x) {
  case_when(
    x == 3.0 ~ 1,            # In paid work
    x == 5.0 | x == 4.0 ~ 2,  # Apprenticeship / training
    x == 1.0 | x == 2.0 ~ 3,  # Education
    x == 8.0 ~ 4,            # Unemployed
    x == 7.0 ~ 5,            # Looking after home/family
    x == 6.0 | x == 9.0 | x == 10.0 | x == 11.0 ~ 6,  # Other
    x == -91.0 ~ -2,          # Script error
    TRUE ~ -3                 # Not interviewed
  )
}

map_wave7 <- function(x) {
  case_when(
    x == 3.0 ~ 1,            # In paid work
    x == 5.0 | x == 4.0 ~ 2,  # Apprenticeship / training
    x == 1.0 | x == 2.0 ~ 3,  # Education
    x == 8.0 ~ 4,            # Unemployed
    x == 7.0 ~ 5,            # Looking after home/family
    x == 6.0 | x == 9.0 | x == 10.0 | x == 11.0 | x == 12.0 | x == 13.0 | x == 14.0 | x == 15.0 ~ 6,  # Other
    x == -91.0 ~ -1,          # Not applicable
    TRUE ~ -3                 # Not interviewed
  )
}

map_wave8 <- function(x) {
  case_when(
    x == 1.0 | x == 2.0 ~ 1,  # In paid work
    x == 7.0 ~ 2,            # Apprenticeship / training
    x == 5.0 ~ 3,            # Education
    x == 4.0 ~ 4,            # Unemployed
    x == 9.0 ~ 5,            # Looking after home/family
    x == 3.0 | x == 6.0 | x == 8.0 | x == 10.0 ~ 6,  # Other
    x == -9.0 ~ -9,           # Refusal
    x == -8.0 ~ -8,           # Don't know
    x == -1.0 ~ -1,           # Not applicable
    TRUE ~ -3                 # Not interviewed
  )
}

map_wave9 <- function(x) {
  case_when(
    x == 1.0 | x == 2.0 ~ 1,  # In paid work
    x == 7.0 ~ 2,            # Apprenticeship / training
    x == 5.0 ~ 3,            # Education
    x == 4.0 ~ 4,            # Unemployed
    x == 9.0 ~ 5,            # Looking after home/family
    x == 3.0 | x == 6.0 | x == 8.0 | x == 10.0 ~ 6,  # Other
    x == -9.0 ~ -9,           # Refusal
    x == -8.0 ~ -8,           # Don't know
    x == -1.0 ~ -1,           # Not applicable
    TRUE ~ -3                 # Not interviewed
  )
}

# Apply mappings
merged_data <- merged_data %>%
  mutate(
    ecoact17 = map_wave4(W4empsYP),
    ecoact18 = map_wave5(W5mainactYP),
    ecoact19 = map_wave6(W6TCurrentAct),
    ecoact20 = map_wave7(W7TCurrentAct),
    ecoact25 = map_wave8(W8DACTIVITYC),
    ecoact32 = map_wave9(W9DACTIVITYC),
    ecoactadu25 = W8DACTIVITYC,
    ecoactadu32 = W9DACTIVITYC
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")