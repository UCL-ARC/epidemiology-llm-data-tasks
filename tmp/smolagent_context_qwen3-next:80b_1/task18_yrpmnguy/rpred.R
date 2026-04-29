library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load wave_two
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_select = c("NSID", "W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP", "W2ghq12scr"))

# Load wave_four
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_select = c("NSID", "W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP", "W4ghq12scr"))

# Load ns8_self
ns8_self <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", col_select = c("NSID", "W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12"))

# Load ns8_derived
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_select = c("NSID", "W8DGHQSC"))

# Load ns9_main
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_select = c("NSID", "W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12"))

# Load ns9_derived
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_select = c("NSID", "W9DGHQSC"))

# Process wave2 pre-derived
wave2 <- wave2 %>%
  mutate(ghq15 = case_when(
    is.na(W2ghq12scr) ~ -3,
    W2ghq12scr == -99 | W2ghq12scr == -96 ~ -3,
    W2ghq12scr == -97 | W2ghq12scr == -92 ~ -9,
    TRUE ~ W2ghq12scr
  )) %>%
  select(-W2ghq12scr)

# Process wave4 pre-derived
wave4 <- wave4 %>%
  mutate(ghq17 = case_when(
    is.na(W4ghq12scr) ~ -3,
    W4ghq12scr == -99 | W4ghq12scr == -96 ~ -3,
    W4ghq12scr == -97 | W4ghq12scr == -92 ~ -9,
    TRUE ~ W4ghq12scr
  )) %>%
  select(-W4ghq12scr)

# Process ns8_derived
ns8_derived <- ns8_derived %>%
  mutate(ghq25 = ifelse(is.na(W8DGHQSC), -3, W8DGHQSC)) %>%
  select(-W8DGHQSC)

# Process ns9_derived
ns9_derived <- ns9_derived %>%
  mutate(ghq32 = ifelse(is.na(W9DGHQSC), -3, W9DGHQSC)) %>%
  select(-W9DGHQSC)

# Process wave2 item-level
wave2_items <- wave2 %>%
  select(-ghq15) %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), -3, .x)))
wave2 <- wave2 %>%
  mutate(ghqtl15 = case_when(
    rowSums(across(starts_with("W2"), ~ .x == -3)) == 12 ~ -3,
    rowSums(across(starts_with("W2"), ~ .x < 0)) > 0 ~ -8,
    TRUE ~ rowSums(across(starts_with("W2")))
  )) %>%
  select(-starts_with("W2"))

# Process wave4 item-level
wave4_items <- wave4 %>%
  select(-ghq17) %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), -3, .x)))
wave4 <- wave4 %>%
  mutate(ghqtl17 = case_when(
    rowSums(across(starts_with("W4"), ~ .x == -3)) == 12 ~ -3,
    rowSums(across(starts_with("W4"), ~ .x < 0)) > 0 ~ -8,
    TRUE ~ rowSums(across(starts_with("W4")))
  )) %>%
  select(-starts_with("W4"))

# Process ns8_self item-level
ns8_self <- ns8_self %>%
  mutate(across(starts_with("W8"), ~ ifelse(is.na(.x), -3, .x))) %>%
  mutate(ghqtl25 = case_when(
    rowSums(across(starts_with("W8"), ~ .x == -3)) == 12 ~ -3,
    rowSums(across(starts_with("W8"), ~ .x < 0)) > 0 ~ -8,
    TRUE ~ rowSums(across(starts_with("W8")))
  )) %>%
  select(-starts_with("W8"))

# Process ns9_main item-level
ns9_main <- ns9_main %>%
  mutate(across(starts_with("W9"), ~ ifelse(is.na(.x), -3, .x))) %>%
  mutate(ghqtl32 = case_when(
    rowSums(across(starts_with("W9"), ~ .x == -3)) == 12 ~ -3,
    rowSums(across(starts_with("W9"), ~ .x < 0)) > 0 ~ -8,
    TRUE ~ rowSums(across(starts_with("W9")))
  )) %>%
  select(-starts_with("W9"))

# Merge all datasets
merged <- wave2 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns8_self, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# Select only the required variables
cleaned_data <- merged %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Apply labelled labels
cleaned_data <- cleaned_data %>%
  mutate(across(ghq15:ghqtl32, ~ labelled::labelled(.x, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))))

# Convert to factors
cleaned_data <- cleaned_data %>%
  mutate(across(-NSID, as.factor))

# Write to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")