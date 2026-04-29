library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Load the datasets
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

# Harmonize missing value codes for sex variables
merged_data <- merged_data %>%
  mutate(
    sex14 = case_when(
      W1sexYP == -999.0 ~ -3,
      W1sexYP == -99.0 ~ -3,
      W1sexYP == -92.0 ~ -9,
      W1sexYP == -91.0 ~ -1,
      W1sexYP == 1.0 ~ 1,
      W1sexYP == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex15 = case_when(
      W2SexYP == -998.0 ~ -3,
      W2SexYP == -997.0 ~ -3,
      W2SexYP == -995.0 ~ -3,
      W2SexYP == -99.0 ~ -3,
      W2SexYP == -92.0 ~ -9,
      W2SexYP == -91.0 ~ -1,
      W2SexYP == -1.0 ~ -8,
      W2SexYP == 1.0 ~ 1,
      W2SexYP == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex16 = case_when(
      W3sexYP == -99.0 ~ -3,
      W3sexYP == -92.0 ~ -9,
      W3sexYP == -91.0 ~ -1,
      W3sexYP == 1.0 ~ 1,
      W3sexYP == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex17 = case_when(
      W4SexYP == -99.0 ~ -3,
      W4SexYP == -92.0 ~ -9,
      W4SexYP == -91.0 ~ -1,
      W4SexYP == -1.0 ~ -8,
      W4SexYP == 1.0 ~ 1,
      W4SexYP == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex18 = case_when(
      W5SexYP == -1.0 ~ -8,
      W5SexYP == 1.0 ~ 1,
      W5SexYP == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex19 = case_when(
      W6Sex == -92.0 ~ -9,
      W6Sex == -91.0 ~ -1,
      W6Sex == 1.0 ~ 1,
      W6Sex == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex20 = case_when(
      W7Sex == -91.0 ~ -1,
      W7Sex == 1.0 ~ 1,
      W7Sex == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex25 = case_when(
      W8CMSEX == -9.0 ~ -9,
      W8CMSEX == -8.0 ~ -8,
      W8CMSEX == -1.0 ~ -1,
      W8CMSEX == 1.0 ~ 1,
      W8CMSEX == 2.0 ~ 2,
      TRUE ~ -3
    ),
    sex32 = case_when(
      W9DSEX == 1.0 ~ 1,
      W9DSEX == 2.0 ~ 2,
      TRUE ~ -3
    )
  )

# Create a consolidated sex variable
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      sex32 %in% c(1, 2) ~ sex32,
      sex25 %in% c(1, 2) ~ sex25,
      sex20 %in% c(1, 2) ~ sex20,
      sex19 %in% c(1, 2) ~ sex19,
      sex18 %in% c(1, 2) ~ sex18,
      sex17 %in% c(1, 2) ~ sex17,
      sex16 %in% c(1, 2) ~ sex16,
      sex15 %in% c(1, 2) ~ sex15,
      sex14 %in% c(1, 2) ~ sex14,
      TRUE ~ -3
    )
  )

# Convert sex to a factor with labels
merged_data <- merged_data %>%
  mutate(
    sex = factor(sex, levels = c(-9, -8, -1, -3, 1, 2), labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", "Not asked at the fieldwork stage/participated/interviewed", "Male", "Female"))
  )

# Select only the derived variables and the ID variable
cleaned_data <- merged_data %>%
  select(NSID, sex, sex14, sex15, sex16, sex17, sex18, sex19, sex20, sex25, sex32)

# Write the cleaned data to a CSV file
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
