library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define a function to harmonize missing values based on metadata
harmonize_missing <- function(var, wave) {
  if (wave == "wave6") {
    var <- case_when(
      var == -97 ~ -9,
      var == -92 ~ -9,
      var == -91 ~ -1,
      var == -1 ~ -8,
      TRUE ~ var
    )
  } else if (wave == "wave7") {
    var <- case_when(
      var == -100 ~ -9,
      var == -97 ~ -9,
      var == -92 ~ -9,
      var == -91 ~ -1,
      var == -1 ~ -8,
      TRUE ~ var
    )
  } else if (wave == "wave8") {
    var <- case_when(
      var == -9 ~ -9,
      var == -8 ~ -8,
      var == -1 ~ -1,
      TRUE ~ var
    )
  } else if (wave == "wave9") {
    var <- case_when(
      var == -9 ~ -9,
      var == -8 ~ -8,
      var == -3 ~ -3,
      var == -1 ~ -1,
      var == 5 ~ -7,
      TRUE ~ var
    )
  }
  return(var)
}

# Process each wave's sexual orientation variable
merged_data <- merged_data %>%
  mutate(
    sori19 = harmonize_missing(W6SexualityYP, "wave6"),
    sori20 = harmonize_missing(W7SexualityYP, "wave7"),
    sori25 = harmonize_missing(W8SEXUALITY, "wave8"),
    sori32 = harmonize_missing(W9SORI, "wave9")
  )

# Select only the ID and derived variables
output_data <- merged_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"