library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_sc, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var) {
  case_when(
    var %in% c(-999, -998, -997, -995) ~ -2,
    var %in% c(-99, -97, -96) ~ -3,
    var == -92 ~ -9,
    var == -91 ~ -1,
    var == -1 ~ -8,
    var == -94 ~ -8,
    var == -100 ~ -2,
    TRUE ~ var
  )
}

# Function to map GHQ items to standard scoring (0-0-1-1 for Likert)
map_ghq_item <- function(item) {
  case_when(
    item %in% c(1, 2) ~ 0,
    item %in% c(3, 4) ~ 1,
    TRUE ~ NA_real_
  )
}

# Wave 2 (Age 15) - GHQ-12 Likert score
merged_data <- merged_data %>%
  mutate(across(c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP",
                  "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP",
                  "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP"),
                ~ map_missing(.x))) %>%
  mutate(ghqtl15 = rowSums(select(., "W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP",
                                   "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP",
                                   "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP"),
                          na.rm = TRUE))

# Set to -3 if any item was missing (sum would be less than expected)
merged_data <- merged_data %>%
  mutate(ghqtl15 = ifelse(ghqtl15 < 12, -3, ghqtl15))

# Wave 2 (Age 15) - GHQ-12 Caseness score
merged_data <- merged_data %>%
  mutate(across(c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP",
                  "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP",
                  "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP"),
                ~ map_ghq_item(.x))) %>%
  mutate(ghq15 = rowSums(select(., "W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP",
                                 "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP",
                                 "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP"),
                        na.rm = TRUE))

merged_data <- merged_data %>%
  mutate(ghq15 = ifelse(is.na(ghq15), -3, ghq15))

# Wave 4 (Age 17) - GHQ-12 Likert score
merged_data <- merged_data %>%
  mutate(across(c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP",
                  "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP",
                  "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP"),
                ~ map_missing(.x))) %>%
  mutate(ghqtl17 = rowSums(select(., "W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP",
                                   "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP",
                                   "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP"),
                          na.rm = TRUE))

merged_data <- merged_data %>%
  mutate(ghqtl17 = ifelse(ghqtl17 < 12, -3, ghqtl17))

# Wave 4 (Age 17) - GHQ-12 Caseness score
merged_data <- merged_data %>%
  mutate(across(c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP",
                  "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP",
                  "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP"),
                ~ map_ghq_item(.x))) %>%
  mutate(ghq17 = rowSums(select(., "W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP",
                                 "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP",
                                 "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP"),
                        na.rm = TRUE))

merged_data <- merged_data %>%
  mutate(ghq17 = ifelse(is.na(ghq17), -3, ghq17))

# Wave 8 (Age 25) - GHQ-12 Likert score
merged_data <- merged_data %>%
  mutate(across(c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4",
                  "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8",
                  "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12"),
                ~ map_missing(.x))) %>%
  mutate(ghqtl25 = rowSums(select(., "W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4",
                                   "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8",
                                   "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12"),
                          na.rm = TRUE))

merged_data <- merged_data %>%
  mutate(ghqtl25 = ifelse(ghqtl25 < 12, -3, ghqtl25))

# Wave 8 (Age 25) - GHQ-12 Caseness score (using pre-derived variable)
merged_data <- merged_data %>%
  mutate(ghq25 = map_missing(W8DGHQSC))

# Wave 9 (Age 32) - GHQ-12 Likert score
merged_data <- merged_data %>%
  mutate(across(c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4",
                  "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8",
                  "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12"),
                ~ map_missing(.x))) %>%
  mutate(ghqtl32 = rowSums(select(., "W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4",
                                   "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8",
                                   "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12"),
                          na.rm = TRUE))

merged_data <- merged_data %>%
  mutate(ghqtl32 = ifelse(ghqtl32 < 12, -3, ghqtl32))

# Wave 9 (Age 32) - GHQ-12 Caseness score (using pre-derived variable)
merged_data <- merged_data %>%
  mutate(ghq32 = map_missing(W9DGHQSC))

# Select only NSID and final derived variables
final_data <- merged_data %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")