library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- readr::read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Standard missing value codes mapping function
standardize_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -8,
    x == -99 ~ -3,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ x
  )
}

# Harmonize wave-specific variables to ecoact* variables
# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(ecoact17 = case_when(
    W4empsYP == 1 ~ 1,
    W4empsYP == 2 ~ 1,
    W4empsYP == 3 ~ 4,
    W4empsYP == 4 ~ 5,
    W4empsYP == 5 ~ 6,
    W4empsYP == 6 ~ 9,
    W4empsYP == 7 ~ 8,
    W4empsYP == 8 ~ 8,
    W4empsYP == 9 ~ 10,
    TRUE ~ standardize_missing(W4empsYP)
  )) %>%
  mutate(ecoact17 = factor(ecoact17,
    levels = c(-9, -8, -3, -2, -1, 1, 4, 5, 6, 8, 9, 10),
    labels = c("Refusal", "Don't know", "Not asked", "Schedule error", "Not applicable",
               "In paid work", "Unemployed", "Education", "Apprenticeship", "Sick/disabled", "Other", "Waiting for course/job")))

# Wave 5 (Age 18)
merged_data <- merged_data %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 1 ~ 6,
    W5mainactYP == 2 ~ 11,
    W5mainactYP == 3 ~ 1,
    W5mainactYP == 4 ~ 6,
    W5mainactYP == 5 ~ 5,
    W5mainactYP == 6 ~ 7,
    W5mainactYP == 7 ~ 4,
    W5mainactYP == 8 ~ 9,
    W5mainactYP == 9 ~ 10,
    W5mainactYP == 10 ~ 10,
    W5mainactYP == 11 ~ 10,
    TRUE ~ standardize_missing(W5mainactYP)
  )) %>%
  mutate(ecoact18 = factor(ecoact18,
    levels = c(-9, -8, -3, -2, -1, 1, 4, 5, 6, 7, 9, 10, 11),
    labels = c("Refusal", "Don't know", "Not asked", "Schedule error", "Not applicable",
               "In paid work", "Education", "Apprenticeship", "Training", "Unemployed",
               "Waiting for course/job", "Voluntary work", "Other")))

# Wave 6 (Age 19)
merged_data <- merged_data %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 1 ~ 6,
    W6TCurrentAct == 2 ~ 6,
    W6TCurrentAct == 3 ~ 1,
    W6TCurrentAct == 4 ~ 5,
    W6TCurrentAct == 5 ~ 6,
    W6TCurrentAct == 6 ~ 10,
    W6TCurrentAct == 7 ~ 9,
    W6TCurrentAct == 8 ~ 4,
    W6TCurrentAct == 9 ~ 10,
    W6TCurrentAct == 10 ~ 11,
    W6TCurrentAct == 11 ~ 10,
    TRUE ~ standardize_missing(W6TCurrentAct)
  )) %>%
  mutate(ecoact19 = factor(ecoact19,
    levels = c(-9, -8, -3, -2, -1, 1, 4, 5, 6, 9, 10, 11),
    labels = c("Refusal", "Don't know", "Not asked", "Schedule error", "Not applicable",
               "In paid work", "Education", "Apprenticeship", "Training", "Unemployed",
               "Waiting for course/job", "Voluntary work")))

# Wave 7 (Age 20)
merged_data <- merged_data %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 1 ~ 6,
    W7TCurrentAct == 2 ~ 6,
    W7TCurrentAct == 3 ~ 1,
    W7TCurrentAct == 4 ~ 5,
    W7TCurrentAct == 5 ~ 6,
    W7TCurrentAct == 6 ~ 10,
    W7TCurrentAct == 7 ~ 9,
    W7TCurrentAct == 8 ~ 4,
    W7TCurrentAct == 9 ~ 11,
    W7TCurrentAct == 10 ~ 10,
    W7TCurrentAct == 11 ~ 10,
    W7TCurrentAct == 12 ~ 10,
    W7TCurrentAct == 13 ~ 10,
    W7TCurrentAct == 14 ~ 8,
    W7TCurrentAct == 15 ~ 10,
    TRUE ~ standardize_missing(W7TCurrentAct)
  )) %>%
  mutate(ecoact20 = factor(ecoact20,
    levels = c(-9, -8, -3, -2, -1, 1, 4, 5, 6, 8, 9, 10, 11),
    labels = c("Refusal", "Don't know", "Not asked", "Schedule error", "Not applicable",
               "In paid work", "Education", "Apprenticeship", "Sick/disabled", "Training",
               "Unemployed", "Waiting for course/job", "Voluntary work")))

# Wave 8 (Age 25)
merged_data <- merged_data %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1 ~ 1,
    W8DACTIVITYC == 2 ~ 2,
    W8DACTIVITYC == 3 ~ 3,
    W8DACTIVITYC == 4 ~ 4,
    W8DACTIVITYC == 5 ~ 6,
    W8DACTIVITYC == 6 ~ 6,
    W8DACTIVITYC == 7 ~ 5,
    W8DACTIVITYC == 8 ~ 8,
    W8DACTIVITYC == 9 ~ 9,
    W8DACTIVITYC == 10 ~ 10,
    TRUE ~ standardize_missing(W8DACTIVITYC)
  )) %>%
  mutate(ecoact25 = factor(ecoact25,
    levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 8, 9, 10),
    labels = c("Refusal", "Don't know", "Not asked", "Schedule error", "Not applicable",
               "Employee", "Self-employed", "Unpaid/voluntary work", "Unemployed",
               "Training", "Education", "Sick/disabled", "Looking after home/family", "Other")))

# Wave 9 (Age 32)
merged_data <- merged_data %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1 ~ 1,
    W9DACTIVITYC == 2 ~ 2,
    W9DACTIVITYC == 3 ~ 3,
    W9DACTIVITYC == 4 ~ 4,
    W9DACTIVITYC == 5 ~ 6,
    W9DACTIVITYC == 6 ~ 6,
    W9DACTIVITYC == 7 ~ 5,
    W9DACTIVITYC == 8 ~ 8,
    W9DACTIVITYC == 9 ~ 9,
    W9DACTIVITYC == 10 ~ 10,
    TRUE ~ standardize_missing(W9DACTIVITYC)
  )) %>%
  mutate(ecoact32 = factor(ecoact32,
    levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 8, 9, 10),
    labels = c("Refusal", "Don't know", "Not asked", "Schedule error", "Not applicable",
               "Employee", "Self-employed", "Unpaid/voluntary work", "Unemployed",
               "Training", "Education", "Sick/disabled", "Looking after home/family", "Other")))

# Select final variables
final_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")