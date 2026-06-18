library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Derive sori19 from W6SexualityYP
merged_data <- merged_data %>%
  mutate(sori19 = case_when(
    W6SexualityYP == 1 ~ 1,
    W6SexualityYP == 2 ~ 2,
    W6SexualityYP == 3 ~ 3,
    W6SexualityYP == 4 ~ 4,
    W6SexualityYP == -97 ~ -9,
    W6SexualityYP == -92 ~ -9,
    W6SexualityYP == -91 ~ -1,
    W6SexualityYP == -1 ~ -8,
    TRUE ~ -3
  ))

# Derive sori20 from W7SexualityYP
merged_data <- merged_data %>%
  mutate(sori20 = case_when(
    W7SexualityYP == 1 ~ 1,
    W7SexualityYP == 2 ~ 2,
    W7SexualityYP == 3 ~ 3,
    W7SexualityYP == 4 ~ 4,
    W7SexualityYP == -100 ~ -9,
    W7SexualityYP == -97 ~ -9,
    W7SexualityYP == -92 ~ -9,
    W7SexualityYP == -91 ~ -1,
    W7SexualityYP == -1 ~ -8,
    TRUE ~ -3
  ))

# Derive sori25 from W8SEXUALITY
merged_data <- merged_data %>%
  mutate(sori25 = case_when(
    W8SEXUALITY == 1 ~ 1,
    W8SEXUALITY == 2 ~ 2,
    W8SEXUALITY == 3 ~ 3,
    W8SEXUALITY == 4 ~ 4,
    W8SEXUALITY == -9 ~ -9,
    W8SEXUALITY == -8 ~ -8,
    W8SEXUALITY == -1 ~ -1,
    TRUE ~ -3
  ))

# Derive sori32 from W9SORI
merged_data <- merged_data %>%
  mutate(sori32 = case_when(
    W9SORI == 1 ~ 1,
    W9SORI == 2 ~ 2,
    W9SORI == 3 ~ 3,
    W9SORI == 4 ~ 4,
    W9SORI == 5 ~ -7,
    W9SORI == -9 ~ -9,
    W9SORI == -8 ~ -8,
    W9SORI == -3 ~ -3,
    W9SORI == -1 ~ -1,
    TRUE ~ -3
  ))

# Select only NSID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")