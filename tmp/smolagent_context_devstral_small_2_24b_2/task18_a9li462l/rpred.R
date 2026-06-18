library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required datasets
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave2 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_sc, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Define item-summed score function
compute_ghqtl <- function(...) {
  items <- list(...)
  # Check if all items are NA
  if (all(is.na(items))) {
    return(-3)
  }
  # Check for negative values
  if (any(unlist(items) < 0, na.rm = TRUE)) {
    return(-8)
  }
  # Compute sum
  sum(unlist(items), na.rm = TRUE)
}

# Compute item-summed scores for each wave
merged_data <- merged_data %>%
  mutate(ghqtl15 = compute_ghqtl(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP
                                 , W2strainYP, W2difficYP, W2activYP, W2probsYP
                                 , W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP)) %>%
  mutate(ghqtl17 = compute_ghqtl(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP
                                 , W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP
                                 , W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP)) %>%
  mutate(ghqtl25 = compute_ghqtl(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4
                                 , W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8
                                 , W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)) %>%
  mutate(ghqtl32 = compute_ghqtl(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4
                                 , W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8
                                 , W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12))

# Harmonize pre-derived caseness scores
merged_data <- merged_data %>%
  mutate(ghq15 = case_when(
    W2ghq12scr == -97 ~ -9,
    W2ghq12scr == -92 ~ -9,
    W2ghq12scr == -99 ~ -3,
    TRUE ~ W2ghq12scr
  )) %>%
  mutate(ghq17 = case_when(
    W4ghq12scr == -97 ~ -9,
    W4ghq12scr == -92 ~ -9,
    W4ghq12scr == -99 ~ -3,
    TRUE ~ W4ghq12scr
  )) %>%
  mutate(ghq25 = case_when(
    W8DGHQSC == -9 ~ -9,
    W8DGHQSC == -8 ~ -8,
    W8DGHQSC == -1 ~ -1,
    TRUE ~ W8DGHQSC
  )) %>%
  mutate(ghq32 = case_when(
    W9DGHQSC == -9 ~ -9,
    W9DGHQSC == -8 ~ -8,
    W9DGHQSC == -3 ~ -3,
    W9DGHQSC == -1 ~ -1,
    TRUE ~ W9DGHQSC
  ))

# Select final variables
cleaned_data <- merged_data %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")