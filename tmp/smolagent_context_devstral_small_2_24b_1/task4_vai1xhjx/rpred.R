library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
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

# Define a function to recode missing values based on the metadata
recode_missing <- function(var, wave) {
  case_when(
    wave == "wave_six" & var %in% c(-97, -100) ~ -9,
    wave == "wave_seven" & var %in% c(-97, -100) ~ -9,
    wave == "wave_eight" & var == -9 ~ -9,
    wave == "wave_eight" & var == -8 ~ -8,
    wave == "wave_eight" & var == -1 ~ -1,
    wave == "wave_nine" & var == -9 ~ -9,
    wave == "wave_nine" & var == -8 ~ -8,
    wave == "wave_nine" & var == -3 ~ -3,
    wave == "wave_nine" & var == -1 ~ -1,
    wave == "wave_nine" & var == 5 ~ -7,
    TRUE ~ var
  )
}

# Derive sori19 from W6SexualityYP
merged_data <- merged_data %>%
  mutate(sori19 = recode_missing(W6SexualityYP, "wave_six"))

# Derive sori20 from W7SexualityYP
merged_data <- merged_data %>%
  mutate(sori20 = recode_missing(W7SexualityYP, "wave_seven"))

# Derive sori25 from W8SEXUALITY
merged_data <- merged_data %>%
  mutate(sori25 = recode_missing(W8SEXUALITY, "wave_eight"))

# Derive sori32 from W9SORI
merged_data <- merged_data %>%
  mutate(sori32 = recode_missing(W9SORI, "wave_nine"))

# Select only the ID variable and the derived variables
output_data <- merged_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"