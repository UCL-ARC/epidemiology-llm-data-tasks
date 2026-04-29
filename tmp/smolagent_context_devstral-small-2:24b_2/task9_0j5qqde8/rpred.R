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
merged <- full_join(wave1, wave2, by = "NSID") %>% full_join(wave4, by = "NSID")

# Consolidation logic for maternal education
merged <- merged %>% mutate(
  educdtlma = case_when(
    !is.na(educdtlma.x) & educdtlma.x >= 0 ~ educdtlma.x,
    !is.na(educdtlma.y) & educdtlma.y >= 0 ~ educdtlma.y,
    !is.na(educdtlma) & educdtlma >= 0 ~ educdtlma,
    !is.na(educdtlma.x) & educdtlma.x < 0 ~ educdtlma.x,
    !is.na(educdtlma.y) & educdtlma.y < 0 ~ educdtlma.y,
    !is.na(educdtlma) & educdtlma < 0 ~ educdtlma,
    TRUE ~ NA_real_
  )
)

# Consolidation logic for paternal education
merged <- merged %>% mutate(
  educdtlpa = case_when(
    !is.na(educdtlpa.x) & educdtlpa.x >= 0 ~ educdtlpa.x,
    !is.na(educdtlpa.y) & educdtlpa.y >= 0 ~ educdtlpa.y,
    !is.na(educdtlpa) & educdtlpa >= 0 ~ educdtlpa,
    !is.na(educdtlpa.x) & educdtlpa.x < 0 ~ educdtlpa.x,
    !is.na(educdtlpa.y) & educdtlpa.y < 0 ~ educdtlpa.y,
    !is.na(educdtlpa) & educdtlpa < 0 ~ educdtlpa,
    TRUE ~ NA_real_
  )
)

# Derived collapsed variables
merged <- merged %>% mutate(
  educma = case_when(
    educdtlma %in% c(1, 2, 3, 4) ~ 0,
    educdtlma %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
    educdtlma %in% c(18) ~ 2,
    educdtlma %in% c(19) ~ 3,
    educdtlma %in% c(20) ~ 4,
    TRUE ~ educdtlma
  ),
  educpa = case_when(
    educdtlpa %in% c(1, 2, 3, 4) ~ 0,
    educdtlpa %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
    educdtlpa %in% c(18) ~ 2,
    educdtlpa %in% c(19) ~ 3,
    educdtlpa %in% c(20) ~ 4,
    TRUE ~ educdtlpa
  )
)

# Missing value code harmonization
merged <- merged %>% mutate(
  educdtlma = case_when(
    educdtlma == -999.0 ~ -9,
    educdtlma == -99.0 ~ -8,
    educdtlma == -98.0 ~ -8,
    educdtlma == -94.0 ~ -8,
    educdtlma == -92.0 ~ -9,
    educdtlma == -91.0 ~ -1,
    educdtlma == -1.0 ~ -8,
    TRUE ~ educdtlma
  ),
  educdtlpa = case_when(
    educdtlpa == -999.0 ~ -9,
    educdtlpa == -99.0 ~ -8,
    educdtlpa == -98.0 ~ -8,
    educdtlpa == -94.0 ~ -8,
    educdtlpa == -92.0 ~ -9,
    educdtlpa == -91.0 ~ -1,
    educdtlpa == -1.0 ~ -8,
    TRUE ~ educdtlpa
  ),
  educma = case_when(
    educma == -999.0 ~ -9,
    educma == -99.0 ~ -8,
    educma == -98.0 ~ -8,
    educma == -94.0 ~ -8,
    educma == -92.0 ~ -9,
    educma == -91.0 ~ -1,
    educma == -1.0 ~ -8,
    TRUE ~ educma
  ),
  educpa = case_when(
    educpa == -999.0 ~ -9,
    educpa == -99.0 ~ -8,
    educpa == -98.0 ~ -8,
    educpa == -94.0 ~ -8,
    educpa == -92.0 ~ -9,
    educpa == -91.0 ~ -1,
    educpa == -1.0 ~ -8,
    TRUE ~ educpa
  )
)

# Factor variables and labels
merged <- merged %>% mutate(
  educdtlma = factor(educdtlma, levels = c(-9, -8, -3, -2, -1, 1:20), labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable", "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4", "Teaching qualification, non-degree", "Nursing qualification, non-degree", "A Levels", "OND/ONC", "City and guilds part III, NVQ3", "CSYS", "Scottish Higher Grade", "AS Level", "Trade apprenticeship", "City and guilds part II, NVQ2", "GCSE grade A-C and equivalent", "GCSE grade D-E and equivalent", "City and guilds part I, NVQ1", "Youth training, skill seekers", "Qualification, level unspecified", "No qualification mentioned")),
  educdtlpa = factor(educdtlpa, levels = c(-9, -8, -3, -2, -1, 1:20), labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable", "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4", "Teaching qualification, non-degree", "Nursing qualification, non-degree", "A Levels", "OND/ONC", "City and guilds part III, NVQ3", "CSYS", "Scottish Higher Grade", "AS Level", "Trade apprenticeship", "City and guilds part II, NVQ2", "GCSE grade A-C and equivalent", "GCSE grade D-E and equivalent", "City and guilds part I, NVQ1", "Youth training, skill seekers", "Qualification, level unspecified", "No qualification mentioned")),
  educma = factor(educma, levels = c(-9, -8, -3, -2, -1, 0:4), labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable", "NVQ 4-5: degree-level qualifications and above", "NVQ 1-3: sub-degree qualifications", "None/entry: training programmes below NVQ level", "Other: qualifications where the level is unspecified or cannot be determined", "No qualifications mentioned")),
  educpa = factor(educpa, levels = c(-9, -8, -3, -2, -1, 0:4), labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable", "NVQ 4-5: degree-level qualifications and above", "NVQ 1-3: sub-degree qualifications", "None/entry: training programmes below NVQ level", "Other: qualifications where the level is unspecified or cannot be determined", "No qualifications mentioned"))
)

# Select final variables
cleaned_data <- merged %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)