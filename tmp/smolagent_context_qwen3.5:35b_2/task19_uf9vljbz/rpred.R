# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the input files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all files using full_join by NSID
merged <- full_join(wave1, wave4, by = "NSID")
merged <- full_join(merged, wave8, by = "NSID")
merged <- full_join(merged, wave9, by = "NSID")

# Create the cleaned dataset with only required variables
cleaned_data <- merged %>%
  select(NSID, W8DBMI, W9DBMI) %>%
  mutate(
    bmi25 = case_when(
      W8DBMI == -9 ~ -9,
      W8DBMI == -8 ~ -8,
      W8DBMI == -1 ~ -1,
      W8DBMI == -2 ~ -2,
      is.na(W8DBMI) ~ -3,
      TRUE ~ W8DBMI
    ),
    bmi32 = case_when(
      W9DBMI == -9 ~ -9,
      W9DBMI == -8 ~ -8,
      W9DBMI == -1 ~ -1,
      W9DBMI == -2 ~ -2,
      is.na(W9DBMI) ~ -3,
      TRUE ~ W9DBMI
    )
  ) %>%
  select(NSID, bmi25, bmi32)

# Apply labels for standard missing codes (using numeric labels)
cleaned_data$bmi25 <- labelled(cleaned_data$bmi25, 
  labels = c(`-9` = -9, `-8` = -8, `-1` = -1, `-3` = -3, `-2` = -2))

cleaned_data$bmi32 <- labelled(cleaned_data$bmi32, 
  labels = c(`-9` = -9, `-8` = -8, `-1` = -1, `-3` = -3, `-2` = -2))

# Write output to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Cleaned data saved to data/output/cleaned_data.csv\n")
