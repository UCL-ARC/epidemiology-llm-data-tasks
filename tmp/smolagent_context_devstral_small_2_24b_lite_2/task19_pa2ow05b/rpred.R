library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Function to standardize missing values
standardize_missing <- function(var) {
  case_when(
    var == -9 ~ -9,
    var == -8 ~ -8,
    var == -7 ~ -7,
    var == -3 ~ -3,
    var == -2 ~ -2,
    var == -1 ~ -1,
    TRUE ~ var
  )
}

# Derive BMI variables directly from the merged dataset
merged_data <- merged_data %>%
  mutate(
    bmi25 = standardize_missing(W8DBMI),
    bmi32 = standardize_missing(W9DBMI)
  )

# Select only the required columns
cleaned_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write the output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"