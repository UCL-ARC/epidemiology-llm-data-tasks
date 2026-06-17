library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols())
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols())

# Merge datasets
data <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Harmonisation Mapping for 6-category ecoact
# Categories: 1. Paid work, 2. Education, 3. Training/Apprenticeship, 4. Unemployed, 5. Home/Family, 6. Other/Sick/Disabled

# Wave 4 (Age 17)
ecoact17 <- data %>%
  mutate(ecoact17 = case_when(
    W4empsYP == 1 ~ 1, W4empsYP == 2 ~ 1, 
    W4empsYP == 5 ~ 2, 
    W4empsYP == 4 ~ 3, 
    W4empsYP == 3 ~ 4, 
    W4empsYP == 6 ~ 5, 
    W4empsYP == 7 ~ 6, W4empsYP == 8 ~ 6, W4empsYP == 9 ~ 6, 
    W4empsYP == -92 ~ -9, W4empsYP == -94 ~ -8, W4empsYP == -91 ~ -1, W4empsYP == -999 ~ -2, 
    TRUE ~ -3
  ))

# Wave 5 (Age 18)
ecoact18 <- data %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 3 ~ 1, 
    W5mainactYP == 4 ~ 2, 
    W5mainactYP == 1 ~ 3, W5mainactYP == 2 ~ 3, W5mainactYP == 5 ~ 3, W5mainactYP == 6 ~ 3, 
    W5mainactYP == 7 ~ 4, 
    W5mainactYP == 8 ~ 5, 
    W5mainactYP == 9 ~ 6, W5mainactYP == 10 ~ 6, W5mainactYP == 11 ~ 6, 
    W5mainactYP == -94 ~ -8, 
    TRUE ~ -3
  ))

# Wave 6 (Age 19)
ecoact19 <- data %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 3 ~ 1, 
    W6TCurrentAct == 1 ~ 2, W6TCurrentAct == 2 ~ 2, 
    W6TCurrentAct == 4 ~ 3, W6TCurrentAct == 5 ~ 3, 
    W6TCurrentAct == 8 ~ 4, 
    W6TCurrentAct == 7 ~ 5, 
    W6TCurrentAct == 6 ~ 6, W6TCurrentAct == 9 ~ 6, W6TCurrentAct == 10 ~ 6, W6TCurrentAct == 11 ~ 6, 
    W6TCurrentAct == -91 ~ -1, 
    TRUE ~ -3
  ))

# Wave 7 (Age 20)
ecoact20 <- data %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 3 ~ 1, 
    W7TCurrentAct == 1 ~ 2, W7TCurrentAct == 2 ~ 2, 
    W7TCurrentAct == 4 ~ 3, W7TCurrentAct == 5 ~ 3, 
    W7TCurrentAct == 8 ~ 4, 
    W7TCurrentAct == 7 ~ 5, 
    W7TCurrentAct == 6 ~ 6, W7TCurrentAct == 9 ~ 6, W7TCurrentAct == 10 ~ 6, W7TCurrentAct == 11 ~ 6, W7TCurrentAct == 12 ~ 6, W7TCurrentAct == 13 ~ 6, W7TCurrentAct == 14 ~ 6, W7TCurrentAct == 15 ~ 6, 
    W7TCurrentAct == -91 ~ -1, 
    TRUE ~ -3
  ))

# Wave 8 (Age 25)
ecoact25 <- data %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1 ~ 1, W8DACTIVITYC == 2 ~ 1, 
    W8DACTIVITYC == 5 ~ 2, 
    W8DACTIVITYC == 6 ~ 3, W8DACTIVITYC == 7 ~ 3, 
    W8DACTIVITYC == 4 ~ 4, 
    W8DACTIVITYC == 9 ~ 5, 
    W8DACTIVITYC == 3 ~ 6, W8DACTIVITYC == 8 ~ 6, W8DACTIVITYC == 10 ~ 6, 
    W8DACTIVITYC == -9 ~ -9, W8DACTIVITYC == -8 ~ -8, W8DACTIVITYC == -1 ~ -1, 
    TRUE ~ -3
  ))

# Wave 9 (Age 32)
ecoact32 <- data %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1 ~ 1, W9DACTIVITYC == 2 ~ 1, 
    W9DACTIVITYC == 5 ~ 2, 
    W9DACTIVITYC == 6 ~ 3, W9DACTIVITYC == 7 ~ 3, 
    W9DACTIVITYC == 4 ~ 4, 
    W9DACTIVITYC == 9 ~ 5, 
    W9DACTIVITYC == 3 ~ 6, W9DACTIVITYC == 8 ~ 6, W9DACTIVITYC == 10 ~ 6, 
    W9DACTIVITYC == -9 ~ -9, W9DACTIVITYC == -8 ~ -8, W9DACTIVITYC == -1 ~ -1, 
    TRUE ~ -3
  ))

# Detailed variables for Age 25 and 32
ecoactadu25 <- data %>%
  mutate(ecoactadu25 = case_when(
    W8DACTIVITYC == 1 ~ 1, W8DACTIVITYC == 2 ~ 2, W8DACTIVITYC == 3 ~ 3, W8DACTIVITYC == 4 ~ 4, 
    W8DACTIVITYC == 5 ~ 5, W8DACTIVITYC == 6 ~ 6, W8DACTIVITYC == 7 ~ 7, W8DACTIVITYC == 8 ~ 8, 
    W8DACTIVITYC == 9 ~ 9, W8DACTIVITYC == 10 ~ 10, 
    W8DACTIVITYC == -9 ~ -9, W8DACTIVITYC == -8 ~ -8, W8DACTIVITYC == -1 ~ -1, 
    TRUE ~ -3
  ))

ecoactadu32 <- data %>%
  mutate(ecoactadu32 = case_when(
    W9DACTIVITYC == 1 ~ 1, W9DACTIVITYC == 2 ~ 2, W9DACTIVITYC == 3 ~ 3, W9DACTIVITYC == 4 ~ 4, 
    W9DACTIVITYC == 5 ~ 5, W9DACTIVITYC == 6 ~ 6, W9DACTIVITYC == 7 ~ 7, W9DACTIVITYC == 8 ~ 8, 
    W9DACTIVITYC == 9 ~ 9, W9DACTIVITYC == 10 ~ 10, 
    W9DACTIVITYC == -9 ~ -9, W9DACTIVITYC == -8 ~ -8, W9DACTIVITYC == -1 ~ -1, 
    TRUE ~ -3
  ))

# Final assembly
final_df <- data %>%
  mutate(
    ecoact17 = ecoact17$ecoact17,
    ecoact18 = ecoact18$ecoact18,
    ecoact19 = ecoact19$ecoact19,
    ecoact20 = ecoact20$ecoact20,
    ecoact25 = ecoact25$ecoact25,
    ecoact32 = ecoact32$ecoact32,
    ecoactadu25 = ecoactadu25$ecoactadu25,
    ecoactadu32 = ecoactadu32$ecoactadu32
  ) %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Add labels for the 6-category harmonised variables
cat_labels <- c("Paid work" = "1", "Education" = "2", "Training/Apprenticeship" = "3", "Unemployed" = "4", "Home/Family" = "5", "Other/Sick/Disabled" = "6", "Refusal" = "-9", "Don't know" = "-8", "Prefer not to say" = "-7", "Not asked" = "-3", "Not applicable" = "-1", "Script error" = "-2")

# Applying labels as factors
final_df <- final_df %>%
  mutate(across(starts_with("ecoact"), ~as.factor(.)))

write_csv(final_df, 'data/output/cleaned_data.csv')
