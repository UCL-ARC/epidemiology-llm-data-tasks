library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_sc, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Define a function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable / script error / information lost
    var == -99 ~ -3,  # Not asked at the fieldwork stage / not interviewed
    var == -97 ~ -9,  # Refused self completion
    var == -96 ~ -2,  # Using interpreter (script error / information lost)
    var == -92 ~ -9,  # Refused
    var == -91 ~ -1,  # Not applicable
    var == -8 ~ -8,   # Don't know / insufficient information
    var == -3 ~ -3,   # Not asked at fieldwork stage
    var == -1 ~ -1,   # Not applicable
    TRUE ~ var
  )
}

# Function to calculate GHQ-12 Likert score (summing if all values are non-negative)
calculate_ghqtl <- function(data, items, age) {
  # Check if all items are non-negative (valid responses)
  valid_rows <- data %>%
    mutate(all_non_negative = if_else(all(c_across(all_of(items)) >= 0), TRUE, FALSE))
  
  # Calculate the sum for valid rows
  valid_rows <- valid_rows %>%
    mutate(ghqtl = if_else(all_non_negative, rowSums(across(all_of(items))), NA_real_))
  
  # Map missing values
  valid_rows$ghqtl <- map_missing(valid_rows$ghqtl, age)
  
  return(valid_rows$ghqtl)
}

# Function to extract pre-derived GHQ-12 caseness scores
extract_ghq_caseness <- function(data, caseness_var, age) {
  ghq <- data[[caseness_var]]
  
  # Map missing values
  ghq <- map_missing(ghq, age)
  
  return(ghq)
}

# Calculate GHQ-12 Likert scores for each wave
# Wave 2 (age 15)
items_w2 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP",
               "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
merged_data$ghqtl15 <- calculate_ghqtl(merged_data, items_w2, 15)

# Wave 4 (age 17)
items_w4 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP",
               "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
merged_data$ghqtl17 <- calculate_ghqtl(merged_data, items_w4, 17)

# Wave 8 (age 25)
items_w8 <- paste0("W8GHQ12_", 1:12)
merged_data$ghqtl25 <- calculate_ghqtl(merged_data, items_w8, 25)

# Wave 9 (age 32)
items_w9 <- paste0("W9GHQ12_", 1:12)
merged_data$ghqtl32 <- calculate_ghqtl(merged_data, items_w9, 32)

# Extract GHQ-12 caseness scores for each wave
merged_data$ghq15 <- extract_ghq_caseness(merged_data, "W2ghq12scr", 15)
merged_data$ghq17 <- extract_ghq_caseness(merged_data, "W4ghq12scr", 17)
merged_data$ghq25 <- extract_ghq_caseness(merged_data, "W8DGHQSC", 25)
merged_data$ghq32 <- extract_ghq_caseness(merged_data, "W9DGHQSC", 32)

# Select only the required variables for output
output_data <- merged_data %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"