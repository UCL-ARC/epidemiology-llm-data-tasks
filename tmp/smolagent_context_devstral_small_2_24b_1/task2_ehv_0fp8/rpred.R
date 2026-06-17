library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset explicitly
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the ethnicity variables and their order
eth_vars <- c("W1ethnic2YP", "W2ethnicYP", "w4ethnic2YP", "W8DETHN15", "W9DETHN15")

# Function to standardize missing values
standardize_missing <- function(var) {
  case_when(
    var %in% c(-999, -998, -997, -995, -94, -92, -91, -9, -8, -1) ~ NA_real_,
    TRUE ~ var
  )
}

# Apply standardization to each ethnicity variable
for (var in eth_vars) {
  merged_data[[var]] <- standardize_missing(merged_data[[var]])
}

# Derive the consolidated ethnicity variable 'eth'
merged_data <- merged_data %>%
  mutate(eth = case_when(
    !is.na(W1ethnic2YP) ~ W1ethnic2YP,
    !is.na(W2ethnicYP) ~ W2ethnicYP,
    !is.na(w4ethnic2YP) ~ w4ethnic2YP,
    !is.na(W8DETHN15) ~ W8DETHN15,
    !is.na(W9DETHN15) ~ W9DETHN15,
    TRUE ~ NA_real_
  ))

# Convert NA to -3 for missing values
merged_data$eth[is.na(merged_data$eth)] <- -3

# Select only the NSID and the derived 'eth' variable
output_data <- merged_data %>%
  select(NSID, eth)

# Write the output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"