library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Select relevant variables
wave1_sel <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2_sel <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4_sel <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Harmonize missing codes for wave1
wave1_sel <- wave1_sel %>%
  mutate(
    W1hiqualmum = case_when(
      W1hiqualmum == -999 ~ -2,
      W1hiqualmum == -99 ~ -3,
      W1hiqualmum == -98 ~ -3,
      W1hiqualmum == -94 ~ -8,
      W1hiqualmum == -92 ~ -9,
      W1hiqualmum == -91 ~ -1,
      TRUE ~ W1hiqualmum
    ),
    W1hiqualdad = case_when(
      W1hiqualdad == -999 ~ -2,
      W1hiqualdad == -99 ~ -3,
      W1hiqualdad == -98 ~ -3,
      W1hiqualdad == -94 ~ -8,
      W1hiqualdad == -92 ~ -9,
      W1hiqualdad == -91 ~ -1,
      W1hiqualdad == -1 ~ -8,
      TRUE ~ W1hiqualdad
    )
  )

# Harmonize missing codes for wave2
wave2_sel <- wave2_sel %>%
  mutate(
    W2hiqualmum = case_when(
      W2hiqualmum == -999 ~ -2,
      W2hiqualmum == -99 ~ -3,
      W2hiqualmum == -98 ~ -3,
      W2hiqualmum == -94 ~ -8,
      W2hiqualmum == -92 ~ -9,
      W2hiqualmum == -91 ~ -1,
      TRUE ~ W2hiqualmum
    ),
    W2hiqualdad = case_when(
      W2hiqualdad == -999 ~ -2,
      W2hiqualdad == -99 ~ -3,
      W2hiqualdad == -98 ~ -3,
      W2hiqualdad == -94 ~ -8,
      W2hiqualdad == -92 ~ -9,
      W2hiqualdad == -91 ~ -1,
      W2hiqualdad == -1 ~ -8,
      TRUE ~ W2hiqualdad
    )
  )

# Harmonize missing codes for wave4
wave4_sel <- wave4_sel %>%
  mutate(
    w4hiqualmum = case_when(
      w4hiqualmum == -99 ~ -3,
      w4hiqualmum == -98 ~ -3,
      w4hiqualmum == -94 ~ -8,
      TRUE ~ w4hiqualmum
    ),
    w4hiqualdad = case_when(
      w4hiqualdad == -99 ~ -3,
      w4hiqualdad == -98 ~ -3,
      w4hiqualdad == -94 ~ -8,
      TRUE ~ w4hiqualdad
    )
  )

# Merge datasets
merged <- wave1_sel %>%
  full_join(wave2_sel, by = 'NSID') %>%
  full_join(wave4_sel, by = 'NSID')

# Consolidate maternal education
merged <- merged %>%
  mutate(
    educdtlma = coalesce(
      ifelse(W1hiqualmum >= 1 & W1hiqualmum <= 20, W1hiqualmum, NA),
      ifelse(W2hiqualmum >= 1 & W2hiqualmum <= 20, W2hiqualmum, NA),
      ifelse(w4hiqualmum >= 1 & w4hiqualmum <= 20, w4hiqualmum, NA),
      W1hiqualmum,
      W2hiqualmum,
      w4hiqualmum
    )
  )

# Consolidate paternal education
merged <- merged %>%
  mutate(
    educdtlpa = coalesce(
      ifelse(W1hiqualdad >= 1 & W1hiqualdad <= 20, W1hiqualdad, NA),
      ifelse(W2hiqualdad >= 1 & W2hiqualdad <= 20, W2hiqualdad, NA),
      ifelse(w4hiqualdad >= 1 & w4hiqualdad <= 20, w4hiqualdad, NA),
      W1hiqualdad,
      W2hiqualdad,
      w4hiqualdad
    )
  )

# Create collapsed maternal education
merged <- merged %>%
  mutate(
    educma = case_when(
      educdtlma %in% c(1, 2, 3, 4, 8) ~ 0,
      educdtlma %in% c(5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3,
      educdtlma == 20 ~ 4,
      TRUE ~ educdtlma
    )
  )

# Create collapsed paternal education
merged <- merged %>%
  mutate(
    educpa = case_when(
      educdtlpa %in% c(1, 2, 3, 4, 8) ~ 0,
      educdtlpa %in% c(5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ educdtlpa
    )
  )

# Create labels for detailed variables
maternal_dtl_labels <- c(
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
  'Don\'t know/insufficient information' = -8,
  'Not asked at the fieldwork stage/participated/interviewed' = -3,
  'Schedule not applicable/Script error/information lost' = -2,
  'Item not applicable' = -1
)

paternal_dtl_labels <- maternal_dtl_labels

# Create labels for collapsed variables
maternal_collapsed_labels <- c(
  'NVQ 4–5: degree-level qualifications and above' = 0,
  'NVQ 1–3: sub-degree qualifications' = 1,
  'None/entry: training programmes below NVQ level' = 2,
  'Other: qualifications where level is unspecified' = 3,
  'No qualifications mentioned' = 4,
  'Refusal' = -9,
  'Don\'t know/insufficient information' = -8,
  'Not asked at the fieldwork stage/participated/interviewed' = -3,
  'Schedule not applicable/Script error/information lost' = -2,
  'Item not applicable' = -1
)

paternal_collapsed_labels <- maternal_collapsed_labels

# Apply labels
merged$educdtlma <- labelled(merged$educdtlma, labels = maternal_dtl_labels)
merged$educdtlpa <- labelled(merged$educdtlpa, labels = paternal_dtl_labels)
merged$educma <- labelled(merged$educma, labels = maternal_collapsed_labels)
merged$educpa <- labelled(merged$educpa, labels = paternal_collapsed_labels)

# Select final variables
final_data <- merged %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')