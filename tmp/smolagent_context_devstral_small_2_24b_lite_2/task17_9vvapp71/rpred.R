library(readr)
library(dplyr)
library(haven)
library(labelled)

# Load all required datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Derive imd15 from wave2 IMDRSCORE
# Handle missing values: -94 -> -8, other negatives -> -3
merged_data <- merged_data %>%
  mutate(imd15 = case_when(
    IMDRSCORE.x == -94 ~ -8,
    IMDRSCORE.x < 0 ~ -3,
    TRUE ~ IMDRSCORE.x
  ))

# Derive imd16 from wave3 IMDRSCORE
merged_data <- merged_data %>%
  mutate(imd16 = case_when(
    IMDRSCORE.y == -94 ~ -8,
    IMDRSCORE.y < 0 ~ -3,
    TRUE ~ IMDRSCORE.y
  ))

# Derive imd32 from wave9 W9DIMDD
# Handle missing values: -8 -> -8, other negatives -> -3
merged_data <- merged_data %>%
  mutate(imd32 = case_when(
    W9DIMDD == -8 ~ -8,
    W9DIMDD < 0 ~ -3,
    TRUE ~ W9DIMDD
  ))

# Select only NSID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, imd15, imd16, imd32)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return path to output
"data/output/cleaned_data.csv"