library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to apply standard missing value codes
harmonise_missing <- function(var) {
  case_when(
    var == -9 ~ -9,
    var == -8 ~ -8,
    var == -7 ~ -7,
    var == -3 ~ -3,
    var == -2 ~ -2,
    var == -1 ~ -1,
    is.na(var) ~ -3,
    TRUE ~ var
  )
}

# Derive bmi25 from W8DBMI (wave8, age 25)
merged_data <- merged_data %>%
  mutate(bmi25 = harmonise_missing(W8DBMI))

# Derive bmi32 from W9DBMI (wave9, age 32)
merged_data <- merged_data %>%
  mutate(bmi32 = harmonise_missing(W9DBMI))

# Select only NSID and derived variables
output_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")