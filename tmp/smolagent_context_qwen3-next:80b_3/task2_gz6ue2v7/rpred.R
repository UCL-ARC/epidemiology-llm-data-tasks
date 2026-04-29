library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Process wave1 (age 14)
wave1 <- wave1 %>%
  mutate(eth14 = case_when(
    W1ethnic2YP == -999 ~ -8,
    W1ethnic2YP == -94 ~ -8,
    W1ethnic2YP == -92 ~ -9,
    W1ethnic2YP == -91 ~ -1,
    W1ethnic2YP == -1 ~ -8,
    TRUE ~ W1ethnic2YP
  )) %>%
  select(NSID, eth14)

# Process wave2 (age 15)
wave2 <- wave2 %>%
  mutate(eth15 = case_when(
    W2ethnicYP == -998 ~ -3,
    W2ethnicYP == -997 ~ -3,
    W2ethnicYP == -995 ~ -3,
    W2ethnicYP == -99 ~ -3,
    W2ethnicYP == -92 ~ -9,
    W2ethnicYP == -91 ~ -1,
    W2ethnicYP == -1 ~ -8,
    TRUE ~ W2ethnicYP
  )) %>%
  select(NSID, eth15)

# Process wave4 (age 17)
wave4 <- wave4 %>%
  mutate(eth17 = case_when(
    w4ethnic2YP == -94 ~ -8,
    w4ethnic2YP == -1 ~ -8,
    w4ethnic2YP < 0 ~ -3,
    TRUE ~ w4ethnic2YP
  )) %>%
  select(NSID, eth17)

# Process wave8 (age 25)
wave8 <- wave8 %>%
  mutate(eth25 = case_when(
    W8DETHN15 == -9 ~ -9,
    W8DETHN15 == -8 ~ -8,
    W8DETHN15 == -1 ~ -1,
    TRUE ~ W8DETHN15
  )) %>%
  select(NSID, eth25)

# Process wave9 (age 32)
wave9 <- wave9 %>%
  mutate(eth32 = case_when(
    W9DETHN15 == -8 ~ -8,
    TRUE ~ W9DETHN15
  )) %>%
  select(NSID, eth32)

# Merge all data
data_merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Create consolidated eth variable
data_merged <- data_merged %>%
  mutate(eth = case_when(
    eth14 >= 1 ~ eth14,
    eth15 >= 1 ~ eth15,
    eth17 >= 1 ~ eth17,
    eth25 >= 1 ~ eth25,
    eth32 >= 1 ~ eth32,
    TRUE ~ -3
  ))

# Select only required variables
cleaned_data <- data_merged %>% select(NSID, eth)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")