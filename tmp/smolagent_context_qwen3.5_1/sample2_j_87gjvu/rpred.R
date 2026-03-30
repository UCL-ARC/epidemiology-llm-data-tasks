# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID using full_join
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Recode Wave 1 ethnicity missing values to standard codes
# -999 -> -3 (Missing - household data lost -> Not asked)
# -94 -> -8 (Insufficient information -> Don't know/insufficient)
# -92 -> -9 (Refused -> Refusal)
# -91 -> -1 (Not applicable -> Item not applicable)
# -1 -> -8 (Don't know -> Don't know/insufficient)
merged_data <- merged_data %>%
  mutate(eth14 = case_when(
    W1ethnic2YP == -999 ~ -3,
    W1ethnic2YP == -94 ~ -8,
    W1ethnic2YP == -92 ~ -9,
    W1ethnic2YP == -91 ~ -1,
    W1ethnic2YP == -1 ~ -8,
    W1ethnic2YP > 0 ~ as.numeric(W1ethnic2YP),
    is.na(W1ethnic2YP) ~ -3,
    TRUE ~ as.numeric(W1ethnic2YP)
  ))

# Recode Wave 2 ethnicity missing values to standard codes
# -998, -997, -995, -99 -> -3 (various not asked codes)
# -92 -> -9 (Refused)
# -91 -> -1 (Not applicable)
# -1 -> -8 (Don't know)
merged_data <- merged_data %>%
  mutate(eth15 = case_when(
    W2ethnicYP %in% c(-998, -997, -995, -99) ~ -3,
    W2ethnicYP == -92 ~ -9,
    W2ethnicYP == -91 ~ -1,
    W2ethnicYP == -1 ~ -8,
    W2ethnicYP > 0 ~ as.numeric(W2ethnicYP),
    is.na(W2ethnicYP) ~ -3,
    TRUE ~ as.numeric(W2ethnicYP)
  ))

# Recode Wave 4 ethnicity missing values to standard codes
# -94 -> -8 (Insufficient information)
# -1 -> -8 (Don't know)
merged_data <- merged_data %>%
  mutate(eth17 = case_when(
    w4ethnic2YP == -94 ~ -8,
    w4ethnic2YP == -1 ~ -8,
    w4ethnic2YP > 0 ~ as.numeric(w4ethnic2YP),
    is.na(w4ethnic2YP) ~ -3,
    TRUE ~ as.numeric(w4ethnic2YP)
  ))

# Recode Wave 8 ethnicity missing values to standard codes
# -9 -> -9 (Refused)
# -8 -> -8 (Insufficient information)
# -1 -> -1 (Not applicable)
merged_data <- merged_data %>%
  mutate(eth_wave8 = case_when(
    W8DETHN15 == -9 ~ -9,
    W8DETHN15 == -8 ~ -8,
    W8DETHN15 == -1 ~ -1,
    W8DETHN15 > 0 ~ as.numeric(W8DETHN15),
    is.na(W8DETHN15) ~ -3,
    TRUE ~ as.numeric(W8DETHN15)
  ))

# Recode Wave 9 ethnicity missing values to standard codes
# -8 -> -8 (Insufficient information)
merged_data <- merged_data %>%
  mutate(eth_wave9 = case_when(
    W9DETHN15 == -8 ~ -8,
    W9DETHN15 > 0 ~ as.numeric(W9DETHN15),
    is.na(W9DETHN15) ~ -3,
    TRUE ~ as.numeric(W9DETHN15)
  ))

# Create consolidated ethnicity variable (time-invariant characteristic)
# For ethnicity (stable characteristic established early), prioritize earliest valid response
# Only use positive values in prioritisation; missing codes as fallback
merged_data <- merged_data %>%
  mutate(eth = case_when(
    eth14 > 0 ~ eth14,
    eth15 > 0 ~ eth15,
    eth17 > 0 ~ eth17,
    eth_wave8 > 0 ~ eth_wave8,
    eth_wave9 > 0 ~ eth_wave9,
    # Fallback to first available missing code
    !is.na(eth14) & eth14 < 0 ~ eth14,
    !is.na(eth15) & eth15 < 0 ~ eth15,
    !is.na(eth17) & eth17 < 0 ~ eth17,
    !is.na(eth_wave8) & eth_wave8 < 0 ~ eth_wave8,
    !is.na(eth_wave9) & eth_wave9 < 0 ~ eth_wave9,
    TRUE ~ -3
  ))

# Convert eth to factor with explicit labels for valid categories and missing value codes
merged_data <- merged_data %>%
  mutate(eth = factor(eth,
    levels = c(-9, -8, -3, -1, 1:16),
    labels = c(
      "Refusal",
      "Don't know/insufficient information",
      "Not asked at fieldwork stage",
      "Item not applicable",
      "White - British",
      "White - Irish",
      "Any other White background",
      "Mixed - White and Black Caribbean",
      "Mixed - White and Black African",
      "Mixed - White and Asian",
      "Any other mixed background",
      "Indian",
      "Pakistani",
      "Bangladeshi",
      "Any other Asian background",
      "Black Caribbean",
      "Black African",
      "Any other Black background",
      "Chinese",
      "Any other ethnic background"
    )
  ))

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Select final output variables: ID and derived variables only
output_data <- merged_data %>%
  select(NSID, eth14, eth15, eth17, eth_wave8, eth_wave9, eth)

# Write cleaned data to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Cleaned data saved to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(output_data), "\n")
cat("Number of variables:", ncol(output_data), "\n")
cat("\nEthnicity distribution:\n")
print(table(output_data$eth, useNA = "ifany"))