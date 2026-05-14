library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_2015, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Standard missing value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable"
)

# Harmonize missing value codes for ethnicity variables
merged_data <- merged_data %>%
  mutate(
    eth14 = case_when(
      W1ethnic2YP == -999.0 ~ -9,
      W1ethnic2YP == -94.0 ~ -8,
      W1ethnic2YP == -92.0 ~ -9,
      W1ethnic2YP == -91.0 ~ -1,
      W1ethnic2YP == -1.0 ~ -8,
      TRUE ~ W1ethnic2YP
    ),
    eth15 = case_when(
      W2ethnicYP == -998.0 ~ -3,
      W2ethnicYP == -997.0 ~ -3,
      W2ethnicYP == -995.0 ~ -3,
      W2ethnicYP == -99.0 ~ -3,
      W2ethnicYP == -92.0 ~ -9,
      W2ethnicYP == -91.0 ~ -1,
      W2ethnicYP == -1.0 ~ -8,
      TRUE ~ W2ethnicYP
    ),
    eth17 = case_when(
      w4ethnic2YP == -94.0 ~ -8,
      w4ethnic2YP == -1.0 ~ -8,
      TRUE ~ w4ethnic2YP
    ),
    eth25 = case_when(
      W8DETHN15 == -9.0 ~ -9,
      W8DETHN15 == -8.0 ~ -8,
      W8DETHN15 == -1.0 ~ -1,
      TRUE ~ W8DETHN15
    ),
    eth32 = case_when(
      W9DETHN15 == -8.0 ~ -8,
      TRUE ~ W9DETHN15
    )
  )

# Create consolidated ethnicity variable (prioritize earliest valid response)
merged_data <- merged_data %>%
  mutate(
    eth = case_when(
      !is.na(eth14) & eth14 > 0 ~ eth14,
      !is.na(eth15) & eth15 > 0 ~ eth15,
      !is.na(eth17) & eth17 > 0 ~ eth17,
      !is.na(eth25) & eth25 > 0 ~ eth25,
      !is.na(eth32) & eth32 > 0 ~ eth32,
      TRUE ~ NA_real_
    )
  )

# Convert to factor with labels
eth_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background"
)

merged_data <- merged_data %>%
  mutate(
    eth = factor(eth, levels = as.character(1:16), labels = eth_labels),
    eth14 = factor(eth14, levels = as.character(1:16), labels = eth_labels),
    eth15 = factor(eth15, levels = as.character(1:16), labels = eth_labels),
    eth17 = factor(eth17, levels = as.character(1:16), labels = eth_labels),
    eth25 = factor(eth25, levels = as.character(1:16), labels = eth_labels),
    eth32 = factor(eth32, levels = as.character(1:16), labels = eth_labels)
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, eth, eth14, eth15, eth17, eth25, eth32)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)