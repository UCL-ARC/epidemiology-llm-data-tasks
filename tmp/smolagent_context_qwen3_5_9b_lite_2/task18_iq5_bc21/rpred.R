library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_interview <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Full join by NSID
all_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_sc, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_interview, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Convert R NA values to standard missing code -3
all_data <- all_data %>%
  mutate(across(everything(), ~ifelse(is.na(.), -3, .)))

# Calculate GHQ12 total scores (0-12 Likert scale)
# Use rowwise approach to check if all values are non-negative

# Wave 2 (age 15)
all_data <- all_data %>%
  rowwise() %>%
  mutate(
    ghqtl15 = if (all(c(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, 
                        W2strainYP, W2difficYP, W2activYP, W2probsYP, 
                        W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP) >= 0)) {
      W2concenYP + W2nosleepYP + W2usefulYP + W2decideYP +
      W2strainYP + W2difficYP + W2activYP + W2probsYP +
      W2depressYP + W2noconfYP + W2wthlessYP + W2happyYP
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

# Wave 4 (age 17)
all_data <- all_data %>%
  rowwise() %>%
  mutate(
    ghqtl17 = if (all(c(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, 
                        W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, 
                        W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP) >= 0)) {
      W4ConcenYP + W4NoSleepYP + W4UsefulYP + W4DecideYP +
      W4StrainYP + W4DifficYP + W4ActivYP + W4ProbsYP +
      W4DepressYP + W4NoConfYP + W4WthlessYP + W4HappyYP
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

# Wave 8 (age 25)
all_data <- all_data %>%
  rowwise() %>%
  mutate(
    ghqtl25 = if (all(c(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, 
                        W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, 
                        W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12) >= 0)) {
      W8GHQ12_1 + W8GHQ12_2 + W8GHQ12_3 + W8GHQ12_4 +
      W8GHQ12_5 + W8GHQ12_6 + W8GHQ12_7 + W8GHQ12_8 +
      W8GHQ12_9 + W8GHQ12_10 + W8GHQ12_11 + W8GHQ12_12
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

# Wave 9 (age 32)
all_data <- all_data %>%
  rowwise() %>%
  mutate(
    ghqtl32 = if (all(c(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, 
                        W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, 
                        W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12) >= 0)) {
      W9GHQ12_1 + W9GHQ12_2 + W9GHQ12_3 + W9GHQ12_4 +
      W9GHQ12_5 + W9GHQ12_6 + W9GHQ12_7 + W9GHQ12_8 +
      W9GHQ12_9 + W9GHQ12_10 + W9GHQ12_11 + W9GHQ12_12
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

# Caseness scores from pre-derived variables
all_data <- all_data %>%
  mutate(
    ghq15 = W2ghq12scr,
    ghq17 = W4ghq12scr,
    ghq25 = W8DGHQSC,
    ghq32 = W9DGHQSC
  )

# Write output
write_csv(all_data, "data/output/cleaned_data.csv")
