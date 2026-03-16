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
standard_missing_codes <- c(-9, -8, -1, -3, -2)

# Harmonize missing value codes
merged_data <- merged_data %>%
  mutate(
    W1ethnic2YP = case_when(
      W1ethnic2YP == -999.0 ~ -9,
      W1ethnic2YP == -94.0 ~ -8,
      W1ethnic2YP == -92.0 ~ -9,
      W1ethnic2YP == -91.0 ~ -1,
      W1ethnic2YP == -1.0 ~ -8,
      TRUE ~ W1ethnic2YP
    ),
    W2ethnicYP = case_when(
      W2ethnicYP == -998.0 ~ -3,
      W2ethnicYP == -997.0 ~ -3,
      W2ethnicYP == -995.0 ~ -3,
      W2ethnicYP == -99.0 ~ -3,
      W2ethnicYP == -92.0 ~ -9,
      W2ethnicYP == -91.0 ~ -1,
      W2ethnicYP == -1.0 ~ -8,
      TRUE ~ W2ethnicYP
    ),
    w4ethnic2YP = case_when(
      w4ethnic2YP == -94.0 ~ -8,
      w4ethnic2YP == -1.0 ~ -8,
      TRUE ~ w4ethnic2YP
    ),
    W8DETHN15 = case_when(
      W8DETHN15 == -9.0 ~ -9,
      W8DETHN15 == -8.0 ~ -8,
      W8DETHN15 == -1.0 ~ -1,
      TRUE ~ W8DETHN15
    ),
    W9DETHN15 = case_when(
      W9DETHN15 == -8.0 ~ -8,
      TRUE ~ W9DETHN15
    )
  )

# Create age-specific variables for ethnicity
merged_data <- merged_data %>%
  mutate(
    eth14 = W1ethnic2YP,
    eth15 = W2ethnicYP,
    eth17 = w4ethnic2YP,
    eth25 = W8DETHN15,
    eth32 = W9DETHN15
  )

# Consolidate ethnicity using earliest valid response
merged_data <- merged_data %>%
  mutate(
    eth_consolidated = case_when(
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
    eth14 = factor(eth14, levels = names(eth_labels), labels = eth_labels),
    eth15 = factor(eth15, levels = names(eth_labels), labels = eth_labels),
    eth17 = factor(eth17, levels = names(eth_labels), labels = eth_labels),
    eth25 = factor(eth25, levels = names(eth_labels), labels = eth_labels),
    eth32 = factor(eth32, levels = names(eth_labels), labels = eth_labels),
    eth_consolidated = factor(eth_consolidated, levels = names(eth_labels), labels = eth_labels)
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, eth14, eth15, eth17, eth25, eth32, eth_consolidated)

# Write to output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)