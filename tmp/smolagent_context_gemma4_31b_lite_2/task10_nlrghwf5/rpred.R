library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge datasets
full_df <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Harmonisation mapping for 6-category ecoact
# Categories: 1. Paid Work, 2. Education/Training, 3. Unemployed, 4. Home/Family, 5. Sick/Disabled, 6. Other

# Age 17: W4empsYP
full_df <- full_df %>%
  mutate(ecoact17 = case_when(
    W4empsYP == 1 | W4empsYP == 2 ~ 1,
    W4empsYP == 5 | W4empsYP == 4 ~ 2,
    W4empsYP == 3 ~ 3,
    W4empsYP == 6 ~ 4,
    W4empsYP == 8 ~ 5,
    W4empsYP == 7 | W4empsYP == 9 ~ 6,
    W4empsYP == -92 ~ -9,
    W4empsYP == -94 ~ -8,
    W4empsYP == -91 ~ -1,
    W4empsYP == -999 ~ -2,
    is.na(W4empsYP) ~ -3,
    TRUE ~ -3
  ))

# Age 18: W5mainactYP
full_df <- full_df %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 3 ~ 1,
    W5mainactYP == 1 | W5mainactYP == 2 | W5mainactYP == 4 | W5mainactYP == 5 | W5mainactYP == 6 ~ 2,
    W5mainactYP == 7 ~ 3,
    W5mainactYP == 8 ~ 4,
    W5mainactYP == 9 | W5mainactYP == 10 | W5mainactYP == 11 ~ 6,
    W5mainactYP == -94 ~ -8,
    is.na(W5mainactYP) ~ -3,
    TRUE ~ -3
  ))

# Age 19: W6TCurrentAct
full_df <- full_df %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 3 ~ 1,
    W6TCurrentAct == 1 | W6TCurrentAct == 2 | W6TCurrentAct == 4 | W6TCurrentAct == 5 | W6TCurrentAct == 10 ~ 2,
    W6TCurrentAct == 8 ~ 3,
    W6TCurrentAct == 7 ~ 4,
    W6TCurrentAct == 11 ~ 6, # voluntary work as other
    W6TCurrentAct == 6 | W6TCurrentAct == 9 ~ 6,
    W6TCurrentAct == -91 ~ -1,
    is.na(W6TCurrentAct) ~ -3,
    TRUE ~ -3
  ))

# Age 20: W7TCurrentAct
full_df <- full_df %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 3 ~ 1,
    W7TCurrentAct == 1 | W7TCurrentAct == 2 | W7TCurrentAct == 4 | W7TCurrentAct == 5 | W7TCurrentAct == 9 | W7TCurrentAct == 11 ~ 2,
    W7TCurrentAct == 8 ~ 3,
    W7TCurrentAct == 7 ~ 4,
    W7TCurrentAct == 14 ~ 5,
    W7TCurrentAct == 6 | W7TCurrentAct == 10 | W7TCurrentAct == 12 | W7TCurrentAct == 13 | W7TCurrentAct == 15 ~ 6,
    W7TCurrentAct == -91 ~ -1,
    is.na(W7TCurrentAct) ~ -3,
    TRUE ~ -3
  ))

# Age 25: W8DACTIVITYC
full_df <- full_df %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1 | W8DACTIVITYC == 2 ~ 1,
    W8DACTIVITYC == 5 | W8DACTIVITYC == 6 | W8DACTIVITYC == 7 ~ 2,
    W8DACTIVITYC == 4 ~ 3,
    W8DACTIVITYC == 9 ~ 4,
    W8DACTIVITYC == 8 ~ 5,
    W8DACTIVITYC == 3 | W8DACTIVITYC == 10 ~ 6,
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    is.na(W8DACTIVITYC) ~ -3,
    TRUE ~ -3
  ))

# Age 32: W9DACTIVITYC
full_df <- full_df %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1 | W9DACTIVITYC == 2 ~ 1,
    W9DACTIVITYC == 5 | W9DACTIVITYC == 6 | W9DACTIVITYC == 7 ~ 2,
    W9DACTIVITYC == 4 ~ 3,
    W9DACTIVITYC == 9 ~ 4,
    W9DACTIVITYC == 8 ~ 5,
    W9DACTIVITYC == 3 | W9DACTIVITYC == 10 ~ 6,
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    is.na(W9DACTIVITYC) ~ -3,
    TRUE ~ -3
  ))

# Detailed variables for 25 and 32
full_df <- full_df %>%
  mutate(
    ecoactadu25 = case_when(
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      is.na(W8DACTIVITYC) ~ -3,
      TRUE ~ W8DACTIVITYC
    ),
    ecoactadu32 = case_when(
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      is.na(W9DACTIVITYC) ~ -3,
      TRUE ~ W9DACTIVITYC
    )
  )

# Define factor labels for collapsed ecoact
ecoact_labels <- c("Paid work" = 1, "Education/Training" = 2, "Unemployed" = 3, "Home/Family" = 4, "Sick/Disabled" = 5, "Other" = 6, "Not applicable" = -1, "Schedule not applicable" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9)

# Apply factor labels to the collapsed variables
vars_to_factor <- c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32")
full_df <- full_df %>%
  mutate(across(all_of(vars_to_factor), ~ factor(.x, levels = names(ecoact_labels), labels = ecoact_labels)))

# Final selection
final_df <- full_df %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

write_csv(final_df, 'data/output/cleaned_data.csv')
