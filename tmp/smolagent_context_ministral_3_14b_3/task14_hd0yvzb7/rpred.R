
library(readr)
library(dplyr)

# Load all files
wave1 <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave5 <- readr::read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')
wave6 <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- readr::read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- readr::read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Process waves 1-4
wave1 <- wave1 %>%
  mutate(
    hownteen14 = ifelse(is.na(W1hous12HH), -3, ifelse(W1hous12HH == -1, -8, W1hous12HH)),
    hown14 = case_when(
      hownteen14 %in% c(4,5,6) ~ 4,  # Rent it
      hownteen14 == 8 ~ 6,           # Other
      TRUE ~ hownteen14
    )
  ) %>%
  select(NSID, hownteen14, hown14)

wave2 <- wave2 %>%
  mutate(
    hownteen15 = ifelse(is.na(W2Hous12HH), -3, ifelse(W2Hous12HH == -1, -8, W2Hous12HH)),
    hown15 = case_when(
      hownteen15 %in% c(4,5,6) ~ 4,  # Rent it
      hownteen15 == 8 ~ 6,           # Other
      TRUE ~ hownteen15
    )
  ) %>%
  select(NSID, hownteen15, hown15)

wave3 <- wave3 %>%
  mutate(
    hownteen16 = ifelse(is.na(W3hous12HH), -3, ifelse(W3hous12HH == -1, -8, W3hous12HH)),
    hown16 = case_when(
      hownteen16 %in% c(4,5,6) ~ 4,  # Rent it
      hownteen16 == 8 ~ 6,           # Other
      TRUE ~ hownteen16
    )
  ) %>%
  select(NSID, hownteen16, hown16)

wave4 <- wave4 %>%
  mutate(
    hownteen17 = ifelse(is.na(W4Hous12HH), -3, ifelse(W4Hous12HH == -1, -8, W4Hous12HH)),
    hown17 = case_when(
      hownteen17 %in% c(4,5,6) ~ 4,  # Rent it
      hownteen17 == 8 ~ 6,           # Other
      TRUE ~ hownteen17
    )
  ) %>%
  select(NSID, hownteen17, hown17)

# Process wave 5
wave5 <- wave5 %>%
  mutate(
    hownteen18 = ifelse(!is.na(W5Hous12BHH), W5Hous12BHH,
                       ifelse(!is.na(W5Hous12CHH), W5Hous12CHH,
                       ifelse(W5Hous12YP %in% c(1,2), -3, W5Hous12YP))),
    hown18 = case_when(
      hownteen18 %in% c(1,2,3) ~ 1,  # Owned
      hownteen18 == 4 ~ 6,           # Other (owned other)
      hownteen18 %in% c(1,2,3,4) ~ 4, # Rent it
      hownteen18 == 5 ~ 6,           # Other (rented other)
      TRUE ~ hownteen18
    )
  ) %>%
  select(NSID, hownteen18, hown18)

# Process wave 6
wave6 <- wave6 %>%
  mutate(
    hownteen19 = ifelse(!is.na(W6Hous12bYP), W6Hous12bYP,
                       ifelse(!is.na(W6Hous12cYP), W6Hous12cYP,
                       ifelse(W6Hous12YP %in% c(1,2), -3, W6Hous12YP))),
    hown19 = case_when(
      hownteen19 %in% c(1,2,3) ~ 1,  # Owned
      hownteen19 == 4 ~ 6,           # Other (owned other)
      hownteen19 %in% c(1,2,3,4) ~ 4, # Rent it
      hownteen19 == 5 ~ 6,           # Other (rented other)
      TRUE ~ hownteen19
    )
  ) %>%
  select(NSID, hownteen19, hown19)

# Process wave 7
wave7 <- wave7 %>%
  mutate(
    hownteen20 = ifelse(!is.na(W7Hous12bYP), W7Hous12bYP,
                       ifelse(!is.na(W7Hous12cYP), W7Hous12cYP,
                       ifelse(W7Hous12YP %in% c(1,2), -3, W7Hous12YP))),
    hown20 = case_when(
      hownteen20 %in% c(1,2,3) ~ 1,  # Owned
      hownteen20 == 4 ~ 6,           # Other (owned other)
      hownteen20 %in% c(1,2,3,4) ~ 4, # Rent it
      hownteen20 == 5 ~ 6,           # Other (rented other)
      TRUE ~ hownteen20
    )
  ) %>%
  select(NSID, hownteen20, hown20)

# Process wave 8
wave8 <- wave8 %>%
  mutate(
    hown25 = ifelse(is.na(W8TENURE), -3,
                   ifelse(W8TENURE == -1, -1,
                   ifelse(W8TENURE %in% c(6,7), 6, W8TENURE)))
  ) %>%
  select(NSID, hown25)

# Process wave 9
wave9 <- wave9 %>%
  mutate(
    hown32 = ifelse(is.na(W9DTENURE), -3,
                   ifelse(W9DTENURE == -1, -1,
                   ifelse(W9DTENURE %in% c(6,7), 6, W9DTENURE)))
  ) %>%
  select(NSID, hown32)

# Merge all waves
merged_data <- full_join(wave1, wave2, by = 'NSID')
merged_data <- full_join(merged_data, wave3, by = 'NSID')
merged_data <- full_join(merged_data, wave4, by = 'NSID')
merged_data <- full_join(merged_data, wave5, by = 'NSID')
merged_data <- full_join(merged_data, wave6, by = 'NSID')
merged_data <- full_join(merged_data, wave7, by = 'NSID')
merged_data <- full_join(merged_data, wave8, by = 'NSID')
merged_data <- full_join(merged_data, wave9, by = 'NSID')

# Write to CSV
readr::write_csv(merged_data, 'data/output/cleaned_data.csv')
