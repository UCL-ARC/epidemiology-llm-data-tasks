library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define a function to map missing values according to the task requirements
map_missing <- function(x) {
  case_when(
    x == -94 ~ -2,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

# Derive the consolidated 'lang' variable using earliest valid positive response
merged_data <- merged_data %>%
  mutate(
    lang = coalesce(
      ifelse(W1englangYP > 0, W1englangYP, NA_real_),
      ifelse(W2EnglangYP > 0, W2EnglangYP, NA_real_),
      ifelse(W3englangHH > 0, W3englangHH, NA_real_),
      ifelse(W4EngLangHH > 0, W4EngLangHH, NA_real_)
    )
  ) %>%
  mutate(lang = map_missing(lang))

# Convert NA to -3 as per standard missing-value codes
merged_data$lang[is.na(merged_data$lang)] <- -3

# Select only the ID variable and the derived 'lang' variable
output_data <- merged_data %>%
  select(NSID, lang)

# Write the output CSV file
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"