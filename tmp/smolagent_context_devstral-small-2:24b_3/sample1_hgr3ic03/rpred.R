library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- readr::read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8_2015, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Standard missing-value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed"
)

# Harmonize missing value codes
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~ case_when(
    .x == -999 ~ -3,
    .x == -998 ~ -3,
    .x == -997 ~ -3,
    .x == -995 ~ -3,
    .x == -99 ~ -3,
    .x == -92 ~ -9,
    .x == -91 ~ -1,
    .x == -1 ~ -8,
    TRUE ~ .x
  )))

# Create consolidated sex variable
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      !is.na(W9DSEX) & W9DSEX %in% c(1, 2) ~ W9DSEX,
      !is.na(W8CMSEX) & W8CMSEX %in% c(1, 2) ~ W8CMSEX,
      !is.na(W7Sex) & W7Sex %in% c(1, 2) ~ W7Sex,
      !is.na(W6Sex) & W6Sex %in% c(1, 2) ~ W6Sex,
      !is.na(W5SexYP) & W5SexYP %in% c(1, 2) ~ W5SexYP,
      !is.na(W4SexYP) & W4SexYP %in% c(1, 2) ~ W4SexYP,
      !is.na(W3sexYP) & W3sexYP %in% c(1, 2) ~ W3sexYP,
      !is.na(W2SexYP) & W2SexYP %in% c(1, 2) ~ W2SexYP,
      !is.na(W1sexYP) & W1sexYP %in% c(1, 2) ~ W1sexYP,
      TRUE ~ -3
    )
  )

# Create factor variables with labels
merged_data <- merged_data %>%
  mutate(
    sex = factor(sex, levels = c(-3, -1, -8, -9, 1, 2), labels = c("Not asked/interviewed", "Item not applicable", "Don't know", "Refused", "Male", "Female"))
  )

# Select only the derived variables and ID
cleaned_data <- merged_data %>%
  select(NSID, sex)

# Write the cleaned data to a CSV file
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
