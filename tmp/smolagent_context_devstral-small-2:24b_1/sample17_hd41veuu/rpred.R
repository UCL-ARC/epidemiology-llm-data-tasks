library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset and select/rename variables before merging
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID)

wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t") %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd15 = IMDRSCORE)

wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t") %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd16 = IMDRSCORE)

wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID)

ns9_2022 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t") %>%
  select(NSID, W9DIMDD) %>%
  rename(imd32 = W9DIMDD)

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Harmonize missing value codes
cleaned_data <- merged_data %>%
  mutate(across(c(imd15, imd16, imd32), ~ case_when(
    .x == -94 ~ -8,
    is.na(.x) ~ -3,
    TRUE ~ .x
  )))

# Output the cleaned dataset
write_csv(cleaned_data, "data/output/cleaned_data.csv")
print("Data cleaning completed successfully!")