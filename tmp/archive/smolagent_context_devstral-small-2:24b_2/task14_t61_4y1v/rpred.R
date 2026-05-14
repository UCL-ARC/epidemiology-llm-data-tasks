library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
ns9_2022_derived_variables <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_four_lsype_family_background_2020 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave_three_lsype_family_background_2020 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_two_lsype_family_background_2020 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_one_lsype_family_background_2020 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
ns8_2015_main_interview <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_five_lsype_family_background_2020 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave_six_lsype_young_person_2020 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven_lsype_young_person_2020 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(ns9_2022_derived_variables, wave_four_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_three_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_two_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_one_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, ns8_2015_main_interview, by = "NSID")
merged_data <- full_join(merged_data, wave_five_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_six_lsype_young_person_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_seven_lsype_young_person_2020, by = "NSID")

# Recode missing values
merged_data <- merged_data %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), -3, .)))

# Create derived variables
merged_data <- merged_data %>% mutate(
  hownteen14 = W1hous12HH,
  hownteen15 = W2Hous12HH,
  hownteen16 = W3hous12HH,
  hownteen17 = W4Hous12HH,
  hownteen18 = W5Hous12HH,
  hownteen19 = W6Hous12YP,
  hownteen20 = W7Hous12YP,
  hown25 = W8TENURE,
  hown32 = W9DTENURE
)

# Create collapsed variables
merged_data <- merged_data %>% mutate(
  hown14 = case_when(
    W1hous12HH == 1 ~ 1,
    W1hous12HH == 2 ~ 2,
    W1hous12HH == 3 ~ 3,
    W1hous12HH == 4 ~ 4,
    W1hous12HH == 5 ~ 4,
    W1hous12HH == 6 ~ 4,
    W1hous12HH == 7 ~ 5,
    W1hous12HH == 8 ~ 7,
    TRUE ~ W1hous12HH
  ),
  hown15 = case_when(
    W2Hous12HH == 1 ~ 1,
    W2Hous12HH == 2 ~ 2,
    W2Hous12HH == 3 ~ 3,
    W2Hous12HH == 4 ~ 4,
    W2Hous12HH == 5 ~ 4,
    W2Hous12HH == 6 ~ 4,
    W2Hous12HH == 7 ~ 5,
    W2Hous12HH == 8 ~ 7,
    TRUE ~ W2Hous12HH
  ),
  hown16 = case_when(
    W3hous12HH == 1 ~ 1,
    W3hous12HH == 2 ~ 2,
    W3hous12HH == 3 ~ 3,
    W3hous12HH == 4 ~ 4,
    W3hous12HH == 5 ~ 4,
    W3hous12HH == 6 ~ 4,
    W3hous12HH == 7 ~ 5,
    W3hous12HH == 8 ~ 7,
    TRUE ~ W3hous12HH
  ),
  hown17 = case_when(
    W4Hous12HH == 1 ~ 1,
    W4Hous12HH == 2 ~ 2,
    W4Hous12HH == 3 ~ 3,
    W4Hous12HH == 4 ~ 4,
    W4Hous12HH == 5 ~ 4,
    W4Hous12HH == 6 ~ 4,
    W4Hous12HH == 7 ~ 5,
    W4Hous12HH == 8 ~ 7,
    TRUE ~ W4Hous12HH
  ),
  hown18 = case_when(
    W5Hous12HH == 1 ~ 1,
    W5Hous12HH == 2 ~ 2,
    W5Hous12HH == 3 ~ 3,
    W5Hous12HH == 4 ~ 4,
    W5Hous12HH == 5 ~ 4,
    W5Hous12HH == 6 ~ 4,
    W5Hous12HH == 7 ~ 5,
    W5Hous12HH == 8 ~ 7,
    TRUE ~ W5Hous12HH
  ),
  hown19 = case_when(
    W6Hous12YP == 1 ~ 1,
    W6Hous12YP == 2 ~ 4,
    W6Hous12YP == 3 ~ 7,
    TRUE ~ W6Hous12YP
  ),
  hown20 = case_when(
    W7Hous12YP == 1 ~ 1,
    W7Hous12YP == 2 ~ 4,
    W7Hous12YP == 3 ~ 7,
    TRUE ~ W7Hous12YP
  )
)

# Select variables to keep
cleaned_data <- merged_data %>% select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)