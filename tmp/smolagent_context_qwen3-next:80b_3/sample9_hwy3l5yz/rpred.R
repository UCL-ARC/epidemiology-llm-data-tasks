library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load data
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Select only needed variables
wave1 <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2 <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4 <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge full_join by NSID
data_merged <- full_join(wave1, wave2, by = 'NSID') %>% full_join(wave4, by = 'NSID')

# Harmonize missing codes for each variable
# Handle NA as -3 (Not asked), then map other missing codes

data_merged <- data_merged %>%
  mutate(W1hiqualmum = case_when(
    is.na(W1hiqualmum) ~ -3,
    W1hiqualmum == -999 ~ -2,
    W1hiqualmum == -99 ~ -3,
    W1hiqualmum == -98 ~ -3,
    W1hiqualmum == -94 ~ -8,
    W1hiqualmum == -92 ~ -9,
    W1hiqualmum == -91 ~ -1,
    TRUE ~ W1hiqualmum
  )) %>%
  mutate(W1hiqualdad = case_when(
    is.na(W1hiqualdad) ~ -3,
    W1hiqualdad == -999 ~ -2,
    W1hiqualdad == -99 ~ -3,
    W1hiqualdad == -98 ~ -3,
    W1hiqualdad == -94 ~ -8,
    W1hiqualdad == -92 ~ -9,
    W1hiqualdad == -91 ~ -1,
    TRUE ~ W1hiqualdad
  )) %>%
  mutate(W2hiqualmum = case_when(
    is.na(W2hiqualmum) ~ -3,
    W2hiqualmum == -999 ~ -2,
    W2hiqualmum == -99 ~ -3,
    W2hiqualmum == -98 ~ -3,
    W2hiqualmum == -94 ~ -8,
    W2hiqualmum == -92 ~ -9,
    W2hiqualmum == -91 ~ -1,
    TRUE ~ W2hiqualmum
  )) %>%
  mutate(W2hiqualdad = case_when(
    is.na(W2hiqualdad) ~ -3,
    W2hiqualdad == -999 ~ -2,
    W2hiqualdad == -99 ~ -3,
    W2hiqualdad == -98 ~ -3,
    W2hiqualdad == -94 ~ -8,
    W2hiqualdad == -92 ~ -9,
    W2hiqualdad == -91 ~ -1,
    TRUE ~ W2hiqualdad
  )) %>%
  mutate(w4hiqualmum = case_when(
    is.na(w4hiqualmum) ~ -3,
    w4hiqualmum == -999 ~ -2,
    w4hiqualmum == -99 ~ -3,
    w4hiqualmum == -98 ~ -3,
    w4hiqualmum == -94 ~ -8,
    TRUE ~ w4hiqualmum
  )) %>%
  mutate(w4hiqualdad = case_when(
    is.na(w4hiqualdad) ~ -3,
    w4hiqualdad == -999 ~ -2,
    w4hiqualdad == -99 ~ -3,
    w4hiqualdad == -98 ~ -3,
    w4hiqualdad == -94 ~ -8,
    TRUE ~ w4hiqualdad
  ))

# Consolidate detailed variables for mother and father

data_merged <- data_merged %>%
  mutate(educdtlma = case_when(
    W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ W1hiqualmum,
    W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ W2hiqualmum,
    w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ w4hiqualmum,
    TRUE ~ W1hiqualmum
  )) %>%
  mutate(educdtlpa = case_when(
    W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ W1hiqualdad,
    W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ W2hiqualdad,
    w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ w4hiqualdad,
    TRUE ~ W1hiqualdad
  ))

# Create collapsed variables

data_merged <- data_merged %>%
  mutate(educma = case_when(
    educdtlma == 1 ~ 0,
    educdtlma == 2 ~ 0,
    educdtlma == 3 ~ 0,
    educdtlma == 4 ~ 0,
    educdtlma == 5 ~ 1,
    educdtlma == 6 ~ 1,
    educdtlma == 7 ~ 1,
    educdtlma == 8 ~ 0,
    educdtlma == 9 ~ 1,
    educdtlma == 10 ~ 3,
    educdtlma == 11 ~ 1,
    educdtlma == 12 ~ 1,
    educdtlma == 13 ~ 1,
    educdtlma == 14 ~ 1,
    educdtlma == 15 ~ 1,
    educdtlma == 16 ~ 2,
    educdtlma == 17 ~ 1,
    educdtlma == 18 ~ 2,
    educdtlma == 19 ~ 3,
    educdtlma == 20 ~ 4,
    TRUE ~ educdtlma
  )) %>%
  mutate(educpa = case_when(
    educdtlpa == 1 ~ 0,
    educdtlpa == 2 ~ 0,
    educdtlpa == 3 ~ 0,
    educdtlpa == 4 ~ 0,
    educdtlpa == 5 ~ 1,
    educdtlpa == 6 ~ 1,
    educdtlpa == 7 ~ 1,
    educdtlpa == 8 ~ 0,
    educdtlpa == 9 ~ 1,
    educdtlpa == 10 ~ 3,
    educdtlpa == 11 ~ 1,
    educdtlpa == 12 ~ 1,
    educdtlpa == 13 ~ 1,
    educdtlpa == 14 ~ 1,
    educdtlpa == 15 ~ 1,
    educdtlpa == 16 ~ 2,
    educdtlpa == 17 ~ 1,
    educdtlpa == 18 ~ 2,
    educdtlpa == 19 ~ 3,
    educdtlpa == 20 ~ 4,
    TRUE ~ educdtlpa
  ))

# Set value labels for all variables using haven::labelled

data_merged <- data_merged %>%
  mutate(educdtlma = haven::labelled(educdtlma, labels = c(
    'Higher Degree' = 1,
    'First Degree' = 2,
    'HE Diploma' = 3,
    'HNC/HND/NVQ4' = 4,
    'Teaching qualification, non-degree' = 5,
    'Nursing qualification, non-degree' = 6,
    'A Levels' = 7,
    'OND/ONC' = 8,
    'City and guilds part III, NVQ3' = 9,
    'CSYS' = 10,
    'Scottish Higher Grade' = 11,
    'AS Level' = 12,
    'Trade apprenticeship' = 13,
    'City and guilds part II, NVQ2' = 14,
    'GCSE grade A-C and equivalent' = 15,
    'GCSE grade D-E and equivalent' = 16,
    'City and guilds part I, NVQ1' = 17,
    'Youth training, skill seekers' = 18,
    'Qualification, level unspecified' = 19,
    'No qualification mentioned' = 20,
    'Refusal' = -9,
    "Don't know/insufficient information" = -8,
    'Not asked at the fieldwork stage/participated/interviewed' = -3,
    'Schedule not applicable/Script error/information lost' = -2,
    'Item not applicable' = -1
  ))) %>%
  mutate(educdtlpa = haven::labelled(educdtlpa, labels = c(
    'Higher Degree' = 1,
    'First Degree' = 2,
    'HE Diploma' = 3,
    'HNC/HND/NVQ4' = 4,
    'Teaching qualification, non-degree' = 5,
    'Nursing qualification, non-degree' = 6,
    'A Levels' = 7,
    'OND/ONC' = 8,
    'City and guilds part III, NVQ3' = 9,
    'CSYS' = 10,
    'Scottish Higher Grade' = 11,
    'AS Level' = 12,
    'Trade apprenticeship' = 13,
    'City and guilds part II, NVQ2' = 14,
    'GCSE grade A-C and equivalent' = 15,
    'GCSE grade D-E and equivalent' = 16,
    'City and guilds part I, NVQ1' = 17,
    'Youth training, skill seekers' = 18,
    'Qualification, level unspecified' = 19,
    'No qualification mentioned' = 20,
    'Refusal' = -9,
    "Don't know/insufficient information" = -8,
    'Not asked at the fieldwork stage/participated/interviewed' = -3,
    'Schedule not applicable/Script error/information lost' = -2,
    'Item not applicable' = -1
  ))) %>%
  mutate(educma = haven::labelled(educma, labels = c(
    'NVQ 4–5: degree-level qualifications and above' = 0,
    'NVQ 1–3: sub-degree qualifications' = 1,
    'None/entry: training programmes below NVQ level' = 2,
    'Other' = 3,
    'No qualifications mentioned' = 4,
    'Refusal' = -9,
    "Don't know/insufficient information" = -8,
    'Not asked at the fieldwork stage/participated/interviewed' = -3,
    'Schedule not applicable/Script error/information lost' = -2,
    'Item not applicable' = -1
  ))) %>%
  mutate(educpa = haven::labelled(educpa, labels = c(
    'NVQ 4–5: degree-level qualifications and above' = 0,
    'NVQ 1–3: sub-degree qualifications' = 1,
    'None/entry: training programmes below NVQ level' = 2,
    'Other' = 3,
    'No qualifications mentioned' = 4,
    'Refusal' = -9,
    "Don't know/insufficient information" = -8,
    'Not asked at the fieldwork stage/participated/interviewed' = -3,
    'Schedule not applicable/Script error/information lost' = -2,
    'Item not applicable' = -1
  )))

# Select final variables

data_final <- data_merged %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write CSV

write_csv(data_final, 'data/output/cleaned_data.csv')