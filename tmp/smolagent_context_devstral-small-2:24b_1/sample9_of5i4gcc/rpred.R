library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Select variables
wave1 <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2 <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4 <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Rename variables
wave1 <- wave1 %>% rename(educdtlma = W1hiqualmum, educdtlpa = W1hiqualdad)
wave2 <- wave2 %>% rename(educdtlma = W2hiqualmum, educdtlpa = W2hiqualdad)
wave4 <- wave4 %>% rename(educdtlma = w4hiqualmum, educdtlpa = w4hiqualdad)

# Merge datasets
merged <- wave1 %>% full_join(wave2, by = "NSID") %>% full_join(wave4, by = "NSID")

# Consolidation logic for maternal education
merged <- merged %>% mutate(educdtlma_consolidated = case_when(
  !is.na(educdtlma.x) & educdtlma.x >= 1 ~ educdtlma.x,
  !is.na(educdtlma.y) & educdtlma.y >= 1 ~ educdtlma.y,
  !is.na(educdtlma) & educdtlma >= 1 ~ educdtlma,
  !is.na(educdtlma.x) & educdtlma.x < 0 ~ educdtlma.x,
  !is.na(educdtlma.y) & educdtlma.y < 0 ~ educdtlma.y,
  !is.na(educdtlma) & educdtlma < 0 ~ educdtlma,
  TRUE ~ NA_real_
))

# Consolidation logic for paternal education
merged <- merged %>% mutate(educdtlpa_consolidated = case_when(
  !is.na(educdtlpa.x) & educdtlpa.x >= 1 ~ educdtlpa.x,
  !is.na(educdtlpa.y) & educdtlpa.y >= 1 ~ educdtlpa.y,
  !is.na(educdtlpa) & educdtlpa >= 1 ~ educdtlpa,
  !is.na(educdtlpa.x) & educdtlpa.x < 0 ~ educdtlpa.x,
  !is.na(educdtlpa.y) & educdtlpa.y < 0 ~ educdtlpa.y,
  !is.na(educdtlpa) & educdtlpa < 0 ~ educdtlpa,
  TRUE ~ NA_real_
))

# Derived collapsed variables for maternal education
merged <- merged %>% mutate(educma = case_when(
  educdtlma_consolidated %in% c(1, 2, 3, 4) ~ 0,
  educdtlma_consolidated %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
  educdtlma_consolidated %in% c(18) ~ 2,
  educdtlma_consolidated %in% c(19) ~ 3,
  educdtlma_consolidated %in% c(20) ~ 4,
  educdtlma_consolidated < 0 ~ educdtlma_consolidated,
  TRUE ~ -3
))

# Derived collapsed variables for paternal education
merged <- merged %>% mutate(educpa = case_when(
  educdtlpa_consolidated %in% c(1, 2, 3, 4) ~ 0,
  educdtlpa_consolidated %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
  educdtlpa_consolidated %in% c(18) ~ 2,
  educdtlpa_consolidated %in% c(19) ~ 3,
  educdtlpa_consolidated %in% c(20) ~ 4,
  educdtlpa_consolidated < 0 ~ educdtlpa_consolidated,
  TRUE ~ -3
))

# Missing value code harmonization
merged <- merged %>% mutate(educma = case_when(
  educma == -999.0 ~ -9,
  educma == -99.0 ~ -8,
  educma == -98.0 ~ -8,
  educma == -94.0 ~ -8,
  educma == -92.0 ~ -9,
  educma == -91.0 ~ -1,
  educma == -1.0 ~ -8,
  TRUE ~ educma
))

merged <- merged %>% mutate(educpa = case_when(
  educpa == -999.0 ~ -9,
  educpa == -99.0 ~ -8,
  educpa == -98.0 ~ -8,
  educpa == -94.0 ~ -8,
  educpa == -92.0 ~ -9,
  educpa == -91.0 ~ -1,
  educpa == -1.0 ~ -8,
  TRUE ~ educpa
))

# Factor variables and labels
merged <- merged %>% mutate(educdtlma_consolidated = factor(educdtlma_consolidated, levels = c(-9, -8, -3, -2, -1, 1:20), labels = c('Refusal', 'Don\u2019t know/insufficient information', 'Not asked at the fieldwork stage/participated/interviewed', 'Schedule not applicable/Script error/information lost', 'Item not applicable', 'Higher Degree', 'First Degree', 'HE Diploma', 'HNC/HND/NVQ4', 'Teaching qualification, non-degree', 'Nursing qualification, non-degree', 'A Levels', 'OND/ONC', 'City and guilds part III, NVQ3', 'CSYS', 'Scottish Higher Grade', 'AS Level', 'Trade apprenticeship', 'City and guilds part II, NVQ2', 'GCSE grade A-C and equivalent', 'GCSE grade D-E and equivalent', 'City and guilds part I, NVQ1', 'Youth training, skill seekers', 'Qualification, level unspecified', 'No qualification mentioned')))

merged <- merged %>% mutate(educdtlpa_consolidated = factor(educdtlpa_consolidated, levels = c(-9, -8, -3, -2, -1, 1:20), labels = c('Refusal', 'Don\u2019t know/insufficient information', 'Not asked at the fieldwork stage/participated/interviewed', 'Schedule not applicable/Script error/information lost', 'Item not applicable', 'Higher Degree', 'First Degree', 'HE Diploma', 'HNC/HND/NVQ4', 'Teaching qualification, non-degree', 'Nursing qualification, non-degree', 'A Levels', 'OND/ONC', 'City and guilds part III, NVQ3', 'CSYS', 'Scottish Higher Grade', 'AS Level', 'Trade apprenticeship', 'City and guilds part II, NVQ2', 'GCSE grade A-C and equivalent', 'GCSE grade D-E and equivalent', 'City and guilds part I, NVQ1', 'Youth training, skill seekers', 'Qualification, level unspecified', 'No qualification mentioned')))

merged <- merged %>% mutate(educma = factor(educma, levels = c(-9, -8, -3, -2, -1, 0:4), labels = c('Refusal', 'Don\u2019t know/insufficient information', 'Not asked at the fieldwork stage/participated/interviewed', 'Schedule not applicable/Script error/information lost', 'Item not applicable', 'NVQ 4-5: degree-level qualifications and above', 'NVQ 1-3: sub-degree qualifications', 'None/entry: training programmes below NVQ level', 'Other: qualifications where the level is unspecified', 'No qualifications mentioned')))

merged <- merged %>% mutate(educpa = factor(educpa, levels = c(-9, -8, -3, -2, -1, 0:4), labels = c('Refusal', 'Don\u2019t know/insufficient information', 'Not asked at the fieldwork stage/participated/interviewed', 'Schedule not applicable/Script error/information lost', 'Item not applicable', 'NVQ 4-5: degree-level qualifications and above', 'NVQ 1-3: sub-degree qualifications', 'None/entry: training programmes below NVQ level', 'Other: qualifications where the level is unspecified', 'No qualifications mentioned')))

# Select final variables
final_data <- merged %>% select(NSID, educma, educpa, educdtlma_consolidated, educdtlpa_consolidated)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)