library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Select and rename variables
cleaned_data <- merged_data %>%
  select(NSID, IMDRSCORE_w2 = IMDRSCORE.x, IMDRSCORE_w3 = IMDRSCORE.y, W9DIMDD) %>%
  rename(imd15 = IMDRSCORE_w2, imd16 = IMDRSCORE_w3, imd32 = W9DIMDD)

# Recode missing values
cleaned_data <- cleaned_data %>%
  mutate(
    imd15 = case_when(
      imd15 == -94 ~ -8,
      is.na(imd15) ~ -3,
      TRUE ~ imd15
    ),
    imd16 = case_when(
      imd16 == -94 ~ -8,
      is.na(imd16) ~ -3,
      TRUE ~ imd16
    ),
    imd32 = case_when(
      imd32 == -8 ~ -8,
      is.na(imd32) ~ -3,
      TRUE ~ imd32
    )
  )

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")