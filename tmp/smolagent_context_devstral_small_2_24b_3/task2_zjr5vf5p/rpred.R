library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
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

# Define missing value mappings for each ethnicity variable based on metadata
# W1ethnic2YP missing value mappings
merged_data <- merged_data %>%
  mutate(
    W1ethnic2YP = case_when(
      W1ethnic2YP == -999.0 ~ -2,  # Missing - household data lost -> -2
      W1ethnic2YP == -94.0 ~ -8,   # Insufficient information -> -8
      W1ethnic2YP == -92.0 ~ -9,   # Refused -> -9
      W1ethnic2YP == -91.0 ~ -1,   # Not applicable -> -1
      W1ethnic2YP == -1.0 ~ -8,    # Don't know -> -8
      TRUE ~ W1ethnic2YP
    )
  )

# W2ethnicYP missing value mappings
merged_data <- merged_data %>%
  mutate(
    W2ethnicYP = case_when(
      W2ethnicYP == -998.0 ~ -2,  # Interviewer missed question -> -2
      W2ethnicYP == -997.0 ~ -2,  # Script error -> -2
      W2ethnicYP == -995.0 ~ -2,  # Missing history section data - unexplained -> -2
      W2ethnicYP == -99.0 ~ -3,    # YP not interviewed -> -3
      W2ethnicYP == -92.0 ~ -9,   # Refused -> -9
      W2ethnicYP == -91.0 ~ -1,   # Not applicable -> -1
      W2ethnicYP == -1.0 ~ -8,    # Don't Know -> -8
      TRUE ~ W2ethnicYP
    )
  )

# w4ethnic2YP missing value mappings
merged_data <- merged_data %>%
  mutate(
    w4ethnic2YP = case_when(
      w4ethnic2YP == -94.0 ~ -8,   # Insufficient information -> -8
      w4ethnic2YP == -1.0 ~ -8,    # Don't know -> -8
      TRUE ~ w4ethnic2YP
    )
  )

# W8DETHN15 missing value mappings
merged_data <- merged_data %>%
  mutate(
    W8DETHN15 = case_when(
      W8DETHN15 == -9.0 ~ -9,     # Refused -> -9
      W8DETHN15 == -8.0 ~ -8,     # Insufficient information -> -8
      W8DETHN15 == -1.0 ~ -1,     # Not applicable -> -1
      TRUE ~ W8DETHN15
    )
  )

# W9DETHN15 missing value mappings
merged_data <- merged_data %>%
  mutate(
    W9DETHN15 = case_when(
      W9DETHN15 == -8.0 ~ -8,     # Insufficient information -> -8
      TRUE ~ W9DETHN15
    )
  )

# Derive the consolidated ethnicity variable 'eth' using earliest valid positive response
merged_data <- merged_data %>%
  mutate(
    eth = coalesce(W1ethnic2YP, W2ethnicYP, w4ethnic2YP, W8DETHN15, W9DETHN15)
  )

# Convert NA values to -3
merged_data$eth[is.na(merged_data$eth)] <- -3

# Create a labelled factor for 'eth'
eth_labels <- c(
  `1` = "White - British",
  `2` = "White - Irish",
  `3` = "Any other White background",
  `4` = "Mixed - White and Black Caribbean",
  `5` = "Mixed - White and Black African",
  `6` = "Mixed - White and Asian",
  `7` = "Any other mixed background",
  `8` = "Indian",
  `9` = "Pakistani",
  `10` = "Bangladeshi",
  `11` = "Any other Asian background",
  `12` = "Black Caribbean",
  `13` = "Black African",
  `14` = "Any other Black background",
  `15` = "Chinese",
  `16` = "Any other ethnic background",
  `-9` = "Refusal",
  `-8` = "Don't know / insufficient information",
  `-3` = "Not asked at the fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable"
)

merged_data$eth <- factor(merged_data$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

# Select only the ID variable and the derived ethnicity variable
output_data <- merged_data %>%
  select(NSID, eth)

# Write the output CSV file
write_csv(output_data, "data/output/cleaned_data.csv")