library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required datasets
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

# Define a function to map missing values according to the task requirements
harmonize_missing <- function(x, wave) {
  if (wave == "W6SexualityYP") {
    x <- case_when(
      x == -97 ~ -9,
      x == -100 ~ -9,
      TRUE ~ x
    )
  } else if (wave == "W7SexualityYP") {
    x <- case_when(
      x == -97 ~ -9,
      x == -100 ~ -9,
      TRUE ~ x
    )
  } else if (wave == "W8SEXUALITY") {
    x <- case_when(
      x == -97 ~ -9,
      x == -100 ~ -9,
      TRUE ~ x
    )
  } else if (wave == "W9SORI") {
    x <- case_when(
      x == 5 ~ -7,
      x == -97 ~ -9,
      x == -100 ~ -9,
      TRUE ~ x
    )
  }
  return(x)
}

# Derive sori19 from W6SexualityYP
merged_data <- merged_data %>%
  mutate(sori19 = harmonize_missing(W6SexualityYP, "W6SexualityYP"))

# Derive sori20 from W7SexualityYP
merged_data <- merged_data %>%
  mutate(sori20 = harmonize_missing(W7SexualityYP, "W7SexualityYP"))

# Derive sori25 from W8SEXUALITY
merged_data <- merged_data %>%
  mutate(sori25 = harmonize_missing(W8SEXUALITY, "W8SEXUALITY"))

# Derive sori32 from W9SORI
merged_data <- merged_data %>%
  mutate(sori32 = harmonize_missing(W9SORI, "W9SORI"))

# Select only the ID variable and the derived variables
output_data <- merged_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write the output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"