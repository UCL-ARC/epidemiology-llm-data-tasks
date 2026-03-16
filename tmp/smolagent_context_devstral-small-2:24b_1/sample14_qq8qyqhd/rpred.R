library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave_five <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8_2015, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Recode missing values
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~ case_when(
    is.na(.) ~ -3,
    . == -999 ~ -3,
    . == -998 ~ -2,
    . == -997 ~ -2,
    . == -995 ~ -3,
    . == -99 ~ -3,
    . == -92 ~ -9,
    . == -91 ~ -1,
    . == -9 ~ -9,
    . == -8 ~ -8,
    . == -1 ~ -1,
    TRUE ~ .
  )))

# Create detailed adolescent variables (hownteen14-20)
merged_data <- merged_data %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH == 1 ~ 1,
      W1hous12HH == 2 ~ 2,
      W1hous12HH == 3 ~ 3,
      W1hous12HH == 4 ~ 4,
      W1hous12HH == 5 ~ 5,
      W1hous12HH == 6 ~ 6,
      W1hous12HH == 7 ~ 7,
      W1hous12HH == 8 ~ 8,
      W1hous12HH %in% c(-9, -8, -1, -2, -3) ~ W1hous12HH,
      TRUE ~ -3
    ),
    hownteen15 = case_when(
      W2Hous12HH == 1 ~ 1,
      W2Hous12HH == 2 ~ 2,
      W2Hous12HH == 3 ~ 3,
      W2Hous12HH == 4 ~ 4,
      W2Hous12HH == 5 ~ 5,
      W2Hous12HH == 6 ~ 6,
      W2Hous12HH == 7 ~ 7,
      W2Hous12HH == 8 ~ 8,
      W2Hous12HH %in% c(-9, -8, -1, -2, -3) ~ W2Hous12HH,
      TRUE ~ -3
    ),
    hownteen16 = case_when(
      W3hous12HH == 1 ~ 1,
      W3hous12HH == 2 ~ 2,
      W3hous12HH == 3 ~ 3,
      W3hous12HH == 4 ~ 4,
      W3hous12HH == 5 ~ 5,
      W3hous12HH == 6 ~ 6,
      W3hous12HH == 7 ~ 7,
      W3hous12HH == 8 ~ 8,
      W3hous12HH %in% c(-9, -8, -1, -2, -3) ~ W3hous12HH,
      TRUE ~ -3
    ),
    hownteen17 = case_when(
      W4Hous12HH == 1 ~ 1,
      W4Hous12HH == 2 ~ 2,
      W4Hous12HH == 3 ~ 3,
      W4Hous12HH == 4 ~ 4,
      W4Hous12HH == 5 ~ 5,
      W4Hous12HH == 6 ~ 6,
      W4Hous12HH == 7 ~ 7,
      W4Hous12HH == 8 ~ 8,
      W4Hous12HH %in% c(-9, -8, -1, -2, -3) ~ W4Hous12HH,
      TRUE ~ -3
    ),
    hownteen18 = case_when(
      W5Hous12HH == 1 ~ 1,
      W5Hous12HH == 2 ~ 2,
      W5Hous12HH == 3 ~ 3,
      W5Hous12HH == 4 ~ 4,
      W5Hous12HH == 5 ~ 5,
      W5Hous12HH == 6 ~ 6,
      W5Hous12HH == 7 ~ 7,
      W5Hous12HH == 8 ~ 8,
      W5Hous12HH %in% c(-9, -8, -1, -2, -3) ~ W5Hous12HH,
      TRUE ~ -3
    ),
    hownteen19 = case_when(
      W6Hous12YP == 1 ~ 1,
      W6Hous12YP == 2 ~ 2,
      W6Hous12YP == 3 ~ 3,
      W6Hous12YP %in% c(-9, -8, -1, -2, -3) ~ W6Hous12YP,
      TRUE ~ -3
    ),
    hownteen20 = case_when(
      W7Hous12YP == 1 ~ 1,
      W7Hous12YP == 2 ~ 2,
      W7Hous12YP == 3 ~ 3,
      W7Hous12YP %in% c(-9, -8, -1, -2, -3) ~ W7Hous12YP,
      TRUE ~ -3
    )
  )

# Create collapsed variables (hown14-20, hown25, hown32)
merged_data <- merged_data %>%
  mutate(
    hown14 = case_when(
      hownteen14 == 1 ~ 1,
      hownteen14 == 2 ~ 2,
      hownteen14 == 3 ~ 3,
      hownteen14 == 4 ~ 4,
      hownteen14 == 5 ~ 5,
      hownteen14 == 6 ~ 6,
      hownteen14 == 7 ~ 7,
      hownteen14 == 8 ~ 8,
      hownteen14 %in% c(-9, -8, -1, -2, -3) ~ hownteen14,
      TRUE ~ -3
    ),
    hown15 = case_when(
      hownteen15 == 1 ~ 1,
      hownteen15 == 2 ~ 2,
      hownteen15 == 3 ~ 3,
      hownteen15 == 4 ~ 4,
      hownteen15 == 5 ~ 5,
      hownteen15 == 6 ~ 6,
      hownteen15 == 7 ~ 7,
      hownteen15 == 8 ~ 8,
      hownteen15 %in% c(-9, -8, -1, -2, -3) ~ hownteen15,
      TRUE ~ -3
    ),
    hown16 = case_when(
      hownteen16 == 1 ~ 1,
      hownteen16 == 2 ~ 2,
      hownteen16 == 3 ~ 3,
      hownteen16 == 4 ~ 4,
      hownteen16 == 5 ~ 5,
      hownteen16 == 6 ~ 6,
      hownteen16 == 7 ~ 7,
      hownteen16 == 8 ~ 8,
      hownteen16 %in% c(-9, -8, -1, -2, -3) ~ hownteen16,
      TRUE ~ -3
    ),
    hown17 = case_when(
      hownteen17 == 1 ~ 1,
      hownteen17 == 2 ~ 2,
      hownteen17 == 3 ~ 3,
      hownteen17 == 4 ~ 4,
      hownteen17 == 5 ~ 5,
      hownteen17 == 6 ~ 6,
      hownteen17 == 7 ~ 7,
      hownteen17 == 8 ~ 8,
      hownteen17 %in% c(-9, -8, -1, -2, -3) ~ hownteen17,
      TRUE ~ -3
    ),
    hown18 = case_when(
      hownteen18 == 1 ~ 1,
      hownteen18 == 2 ~ 2,
      hownteen18 == 3 ~ 3,
      hownteen18 == 4 ~ 4,
      hownteen18 == 5 ~ 5,
      hownteen18 == 6 ~ 6,
      hownteen18 == 7 ~ 7,
      hownteen18 == 8 ~ 8,
      hownteen18 %in% c(-9, -8, -1, -2, -3) ~ hownteen18,
      TRUE ~ -3
    ),
    hown19 = case_when(
      hownteen19 == 1 ~ 1,
      hownteen19 == 2 ~ 2,
      hownteen19 == 3 ~ 3,
      hownteen19 %in% c(-9, -8, -1, -2, -3) ~ hownteen19,
      TRUE ~ -3
    ),
    hown20 = case_when(
      hownteen20 == 1 ~ 1,
      hownteen20 == 2 ~ 2,
      hownteen20 == 3 ~ 3,
      hownteen20 %in% c(-9, -8, -1, -2, -3) ~ hownteen20,
      TRUE ~ -3
    ),
    hown25 = case_when(
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE == 4 ~ 4,
      W8TENURE == 5 ~ 5,
      W8TENURE == 6 ~ 6,
      W8TENURE == 7 ~ 7,
      W8TENURE %in% c(-9, -8, -1, -2, -3) ~ W8TENURE,
      TRUE ~ -3
    ),
    hown32 = case_when(
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4,
      W9DTENURE == 5 ~ 5,
      W9DTENURE == 6 ~ 6,
      W9DTENURE == 7 ~ 7,
      W9DTENURE %in% c(-9, -8, -1, -2, -3) ~ W9DTENURE,
      TRUE ~ -3
    )
  )

# Select final variables
cleaned_data <- merged_data %>%
  select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)