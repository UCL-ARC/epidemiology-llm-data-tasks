library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all datasets from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to recode adolescent waves (14-17) to standard codes
recode_adolescent_simple <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 5 ~ 5,
    x == 6 ~ 6,
    x == 7 ~ 7,
    x == 8 ~ 8,
    x == -92 ~ -9,
    x == -1 ~ -8,
    x == -91 ~ -1,
    x == -997 ~ -2,
    x == -998 ~ -2,
    x == -999 ~ -3,
    x == -995 ~ -3,
    x == -99 ~ -3,
    is.na(x) ~ -3,
    TRUE ~ as.numeric(x)
  )
}

# Function to combine type + sub-type for waves 5-7 (ages 18-20)
combine_tenure <- function(type_var, owned_sub, rented_sub) {
  case_when(
    type_var == 1 & owned_sub == 1 ~ 1,
    type_var == 1 & owned_sub == 2 ~ 2,
    type_var == 1 & owned_sub == 3 ~ 3,
    type_var == 1 & owned_sub == 4 ~ 8,
    type_var == 2 & rented_sub == 1 ~ 4,
    type_var == 2 & rented_sub == 2 ~ 5,
    type_var == 2 & rented_sub == 3 ~ 6,
    type_var == 2 & rented_sub == 4 ~ 7,
    type_var == 2 & rented_sub == 5 ~ 8,
    type_var == 3 ~ 8,
    type_var == -92 ~ -9,
    type_var == -1 ~ -8,
    type_var == -91 ~ -1,
    type_var == -999 ~ -3,
    is.na(type_var) ~ -3,
    TRUE ~ -3
  )
}

# Create detailed adolescent variables (hownteen14-20)
merged_data <- merged_data %>%
  mutate(
    hownteen14 = recode_adolescent_simple(W1hous12HH),
    hownteen15 = recode_adolescent_simple(W2Hous12HH),
    hownteen16 = recode_adolescent_simple(W3hous12HH),
    hownteen17 = recode_adolescent_simple(W4Hous12HH),
    hownteen18 = combine_tenure(W5Hous12HH, W5Hous12BHH, W5Hous12CHH),
    hownteen19 = combine_tenure(W6Hous12YP, W6Hous12bYP, W6Hous12cYP),
    hownteen20 = combine_tenure(W7Hous12YP, W7Hous12bYP, W7Hous12cYP)
  )

# Function to recode to collapsed categories
recode_collapsed <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x %in% c(4, 5, 6) ~ 4,
    x == 7 ~ 5,
    x == 8 ~ 6,
    x == -1 ~ -1,
    x == -2 ~ -2,
    x == -3 ~ -3,
    x == -8 ~ -8,
    x == -9 ~ -9,
    TRUE ~ as.numeric(x)
  )
}

# Create collapsed adolescent variable (hown14-20)
merged_data <- merged_data %>%
  mutate(
    hown14_20 = recode_collapsed(hownteen20)
  )

# Recode adult waves (25 and 32)
recode_adult_w8 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 5 ~ 5,
    x == 6 ~ 6,
    x == 7 ~ 6,
    x == -1 ~ -1,
    x == -8 ~ -8,
    x == -9 ~ -9,
    is.na(x) ~ -3,
    TRUE ~ as.numeric(x)
  )
}

recode_adult_w9 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 5 ~ 5,
    x == 6 ~ 6,
    x == 7 ~ 6,
    x == -1 ~ -1,
    x == -8 ~ -8,
    x == -9 ~ -9,
    is.na(x) ~ -3,
    TRUE ~ as.numeric(x)
  )
}

merged_data <- merged_data %>%
  mutate(
    hown25 = recode_adult_w8(W8TENURE),
    hown32 = recode_adult_w9(W9DTENURE)
  )

# Select only required variables for output
cleaned_data <- merged_data %>%
  select(NSID, hown14_20, hown25, hown32, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(cleaned_data), "\n")
cat("Number of columns:", ncol(cleaned_data), "\n")