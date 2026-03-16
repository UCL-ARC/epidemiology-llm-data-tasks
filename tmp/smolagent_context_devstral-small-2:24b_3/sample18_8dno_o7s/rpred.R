library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load data files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t") %>% select(NSID)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t") %>% select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP, W2ghq12scr)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>% select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP, W4ghq12scr)
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t") %>% select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t") %>% select(NSID, W8DGHQSC)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t") %>% select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t") %>% select(NSID, W9DGHQSC)

# Merge datasets
merged <- full_join(wave1, wave2, by = "NSID") %>% 
  full_join(wave4, by = "NSID") %>% 
  full_join(wave8, by = "NSID") %>% 
  full_join(wave8_derived, by = "NSID") %>% 
  full_join(wave9, by = "NSID") %>% 
  full_join(wave9_derived, by = "NSID")

# Derive GHQ-12 sum scores
merged <- merged %>% 
  mutate(
    ghqtl15 = case_when(
      all(is.na(c(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP))) ~ -3,
      any(c(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP) < 0) ~ -8,
      TRUE ~ sum(c(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP), na.rm = TRUE)
    ),
    ghqtl17 = case_when(
      all(is.na(c(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP))) ~ -3,
      any(c(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP) < 0) ~ -8,
      TRUE ~ sum(c(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP), na.rm = TRUE)
    ),
    ghqtl25 = case_when(
      all(is.na(c(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12))) ~ -3,
      any(c(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12) < 0) ~ -8,
      TRUE ~ sum(c(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12), na.rm = TRUE)
    ),
    ghqtl32 = case_when(
      all(is.na(c(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12))) ~ -3,
      any(c(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12) < 0) ~ -8,
      TRUE ~ sum(c(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12), na.rm = TRUE)
    )
  )

# Harmonize pre-derived GHQ scores
merged <- merged %>% 
  mutate(
    ghq15 = case_when(
      W2ghq12scr == -96 | W2ghq12scr == -99 ~ -3,
      W2ghq12scr == -97 | W2ghq12scr == -92 ~ -9,
      is.na(W2ghq12scr) ~ -3,
      TRUE ~ W2ghq12scr
    ),
    ghq17 = case_when(
      W4ghq12scr == -96 | W4ghq12scr == -99 ~ -3,
      W4ghq12scr == -97 | W4ghq12scr == -92 ~ -9,
      is.na(W4ghq12scr) ~ -3,
      TRUE ~ W4ghq12scr
    ),
    ghq25 = case_when(
      is.na(W8DGHQSC) ~ -3,
      TRUE ~ W8DGHQSC
    ),
    ghq32 = case_when(
      is.na(W9DGHQSC) ~ -3,
      TRUE ~ W9DGHQSC
    )
  )

# Select and convert to factors
final_data <- merged %>% 
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32) %>% 
  mutate(across(where(is.numeric), as.factor))

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")