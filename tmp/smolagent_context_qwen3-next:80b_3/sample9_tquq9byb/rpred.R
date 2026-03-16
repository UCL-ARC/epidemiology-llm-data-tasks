library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load data
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Select relevant variables
wave1_selected <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2_selected <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4_selected <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge datasets
merged <- full_join(wave1_selected, wave2_selected, by = "NSID") %>% full_join(wave4_selected, by = "NSID")

# Harmonize missing codes for all variables
merged <- merged %>% 
  mutate(
    W1hiqualmum = case_when(
      W1hiqualmum == -999.0 ~ -2,
      W1hiqualmum == -99.0 ~ -3,
      W1hiqualmum == -98.0 ~ -3,
      W1hiqualmum == -94.0 ~ -8,
      W1hiqualmum == -92.0 ~ -9,
      W1hiqualmum == -91.0 ~ -1,
      W1hiqualmum == -1.0 ~ -8,
      TRUE ~ W1hiqualmum
    ),
    W1hiqualdad = case_when(
      W1hiqualdad == -999.0 ~ -2,
      W1hiqualdad == -99.0 ~ -3,
      W1hiqualdad == -98.0 ~ -3,
      W1hiqualdad == -94.0 ~ -8,
      W1hiqualdad == -92.0 ~ -9,
      W1hiqualdad == -91.0 ~ -1,
      W1hiqualdad == -1.0 ~ -8,
      TRUE ~ W1hiqualdad
    ),
    W2hiqualmum = case_when(
      W2hiqualmum == -999.0 ~ -2,
      W2hiqualmum == -99.0 ~ -3,
      W2hiqualmum == -98.0 ~ -3,
      W2hiqualmum == -94.0 ~ -8,
      W2hiqualmum == -92.0 ~ -9,
      W2hiqualmum == -91.0 ~ -1,
      W2hiqualmum == -1.0 ~ -8,
      TRUE ~ W2hiqualmum
    ),
    W2hiqualdad = case_when(
      W2hiqualdad == -999.0 ~ -2,
      W2hiqualdad == -99.0 ~ -3,
      W2hiqualdad == -98.0 ~ -3,
      W2hiqualdad == -94.0 ~ -8,
      W2hiqualdad == -92.0 ~ -9,
      W2hiqualdad == -91.0 ~ -1,
      W2hiqualdad == -1.0 ~ -8,
      TRUE ~ W2hiqualdad
    ),
    w4hiqualmum = case_when(
      w4hiqualmum == -999.0 ~ -2,
      w4hiqualmum == -99.0 ~ -3,
      w4hiqualmum == -98.0 ~ -3,
      w4hiqualmum == -94.0 ~ -8,
      w4hiqualmum == -92.0 ~ -9,
      w4hiqualmum == -91.0 ~ -1,
      w4hiqualmum == -1.0 ~ -8,
      TRUE ~ w4hiqualmum
    ),
    w4hiqualdad = case_when(
      w4hiqualdad == -999.0 ~ -2,
      w4hiqualdad == -99.0 ~ -3,
      w4hiqualdad == -98.0 ~ -3,
      w4hiqualdad == -94.0 ~ -8,
      w4hiqualdad == -92.0 ~ -9,
      w4hiqualdad == -91.0 ~ -1,
      w4hiqualdad == -1.0 ~ -8,
      TRUE ~ w4hiqualdad
    )
  )

# Consolidate detailed variables
merged <- merged %>% 
  mutate(
    educdtlma = case_when(
      w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ w4hiqualmum,
      W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ W2hiqualmum,
      W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ W1hiqualmum,
      TRUE ~ w4hiqualmum
    ),
    educdtlpa = case_when(
      w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ w4hiqualdad,
      W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ W2hiqualdad,
      W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ W1hiqualdad,
      TRUE ~ w4hiqualdad
    )
  )

# Create collapsed variables
merged <- merged %>% 
  mutate(
    educma = case_when(
      educdtlma %in% c(1,2,3,4) ~ 0,
      educdtlma %in% 5:17 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3,
      educdtlma == 20 ~ 4,
      TRUE ~ educdtlma
    ),
    educpa = case_when(
      educdtlpa %in% c(1,2,3,4) ~ 0,
      educdtlpa %in% 5:17 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ educdtlpa
    )
  )

# Convert to labelled factors

# For detailed maternal
educdtlma_labels <- c(
  "Higher Degree" = 1,
  "First Degree" = 2,
  "HE Diploma" = 3,
  "HNC/HND/NVQ4" = 4,
  "Teaching qualification, non-degree" = 5,
  "Nursing qualification, non-degree" = 6,
  "A Levels" = 7,
  "OND/ONC" = 8,
  "City and guilds part III, NVQ3" = 9,
  "CSYS" = 10,
  "Scottish Higher Grade" = 11,
  "AS Level" = 12,
  "Trade apprenticeship" = 13,
  "City and guilds part II, NVQ2" = 14,
  "GCSE grade A-C and equivalent" = 15,
  "GCSE grade D-E and equivalent" = 16,
  "City and guilds part I, NVQ1" = 17,
  "Youth training, skill seekers" = 18,
  "Qualification, level unspecified" = 19,
  "No qualification mentioned" = 20,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not asked" = -3,
  "Schedule not applicable/Script error/information lost" = -2,
  "Item not applicable" = -1
)

merged$educdtlma <- labelled(merged$educdtlma, labels = educdtlma_labels)

# For detailed paternal
educdtlpa_labels <- c(
  "Higher Degree" = 1,
  "First Degree" = 2,
  "HE Diploma" = 3,
  "HNC/HND/NVQ4" = 4,
  "Teaching qualification, non-degree" = 5,
  "Nursing qualification, non-degree" = 6,
  "A Levels" = 7,
  "OND/ONC" = 8,
  "City and guilds part III, NVQ3" = 9,
  "CSYS" = 10,
  "Scottish Higher Grade" = 11,
  "AS Level" = 12,
  "Trade apprenticeship" = 13,
  "City and guilds part II, NVQ2" = 14,
  "GCSE grade A-C and equivalent" = 15,
  "GCSE grade D-E and equivalent" = 16,
  "City and guilds part I, NVQ1" = 17,
  "Youth training, skill seekers" = 18,
  "Qualification, level unspecified" = 19,
  "No qualification mentioned" = 20,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not asked" = -3,
  "Schedule not applicable/Script error/information lost" = -2,
  "Item not applicable" = -1
)

merged$educdtlpa <- labelled(merged$educdtlpa, labels = educdtlpa_labels)

# For collapsed maternal
educma_labels <- c(
  "NVQ4-5" = 0,
  "NVQ1-3" = 1,
  "None/entry" = 2,
  "Other" = 3,
  "No qualifications mentioned" = 4,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not asked" = -3,
  "Schedule not applicable/Script error/information lost" = -2,
  "Item not applicable" = -1
)

merged$educma <- labelled(merged$educma, labels = educma_labels)

# For collapsed paternal
educpa_labels <- c(
  "NVQ4-5" = 0,
  "NVQ1-3" = 1,
  "None/entry" = 2,
  "Other" = 3,
  "No qualifications mentioned" = 4,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not asked" = -3,
  "Schedule not applicable/Script error/information lost" = -2,
  "Item not applicable" = -1
)

merged$educpa <- labelled(merged$educpa, labels = educpa_labels)

# Select only required variables
cleaned_data <- merged %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")