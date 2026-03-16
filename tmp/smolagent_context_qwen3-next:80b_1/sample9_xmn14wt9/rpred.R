library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Select only relevant variables
wave1_sel <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2_sel <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4_sel <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge all waves using full_join on NSID
combined <- full_join(wave1_sel, wave2_sel, by = "NSID") %>%
  full_join(wave4_sel, by = "NSID")

# Harmonize missing codes for each variable

# Wave1 mother
combined <- combined %>%
  mutate(W1hiqualmum = case_when(
    W1hiqualmum == -999.0 ~ -2,
    W1hiqualmum == -99.0 ~ -3,
    W1hiqualmum == -98.0 ~ -1,
    W1hiqualmum == -94.0 ~ -8,
    W1hiqualmum == -92.0 ~ -9,
    W1hiqualmum == -91.0 ~ -1,
    TRUE ~ W1hiqualmum
  ))

# Wave1 father
combined <- combined %>%
  mutate(W1hiqualdad = case_when(
    W1hiqualdad == -999.0 ~ -2,
    W1hiqualdad == -99.0 ~ -3,
    W1hiqualdad == -98.0 ~ -1,
    W1hiqualdad == -94.0 ~ -8,
    W1hiqualdad == -92.0 ~ -9,
    W1hiqualdad == -91.0 ~ -1,
    W1hiqualdad == -1.0 ~ -8,
    TRUE ~ W1hiqualdad
  ))

# Wave2 mother
combined <- combined %>%
  mutate(W2hiqualmum = case_when(
    W2hiqualmum == -999.0 ~ -2,
    W2hiqualmum == -99.0 ~ -3,
    W2hiqualmum == -98.0 ~ -1,
    W2hiqualmum == -94.0 ~ -8,
    W2hiqualmum == -92.0 ~ -9,
    W2hiqualmum == -91.0 ~ -1,
    TRUE ~ W2hiqualmum
  ))

# Wave2 father
combined <- combined %>%
  mutate(W2hiqualdad = case_when(
    W2hiqualdad == -999.0 ~ -2,
    W2hiqualdad == -99.0 ~ -3,
    W2hiqualdad == -98.0 ~ -1,
    W2hiqualdad == -94.0 ~ -8,
    W2hiqualdad == -92.0 ~ -9,
    W2hiqualdad == -91.0 ~ -1,
    W2hiqualdad == -1.0 ~ -8,
    TRUE ~ W2hiqualdad
  ))

# Wave4 mother
combined <- combined %>%
  mutate(w4hiqualmum = case_when(
    w4hiqualmum == -999.0 ~ -2,
    w4hiqualmum == -99.0 ~ -3,
    w4hiqualmum == -98.0 ~ -1,
    w4hiqualmum == -94.0 ~ -8,
    TRUE ~ w4hiqualmum
  ))

# Wave4 father
combined <- combined %>%
  mutate(w4hiqualdad = case_when(
    w4hiqualdad == -999.0 ~ -2,
    w4hiqualdad == -99.0 ~ -3,
    w4hiqualdad == -98.0 ~ -1,
    w4hiqualdad == -94.0 ~ -8,
    TRUE ~ w4hiqualdad
  ))

# Consolidate detailed variables
combined <- combined %>%
  mutate(educdtlma = case_when(
    W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ W1hiqualmum,
    W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ W2hiqualmum,
    w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ w4hiqualmum,
    TRUE ~ coalesce(W1hiqualmum, W2hiqualmum, w4hiqualmum)
  ))

combined <- combined %>%
  mutate(educdtlpa = case_when(
    W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ W1hiqualdad,
    W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ W2hiqualdad,
    w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ w4hiqualdad,
    TRUE ~ coalesce(W1hiqualdad, W2hiqualdad, w4hiqualdad)
  ))

# Create collapsed variables
combined <- combined %>%
  mutate(educma = case_when(
    educdtlma == 1.0 ~ 0,
    educdtlma == 2.0 ~ 0,
    educdtlma == 3.0 ~ 0,
    educdtlma == 4.0 ~ 0,
    educdtlma == 5.0 ~ 1,
    educdtlma == 6.0 ~ 1,
    educdtlma == 7.0 ~ 1,
    educdtlma == 8.0 ~ 1,
    educdtlma == 9.0 ~ 1,
    educdtlma == 10.0 ~ 1,
    educdtlma == 11.0 ~ 1,
    educdtlma == 12.0 ~ 1,
    educdtlma == 13.0 ~ 1,
    educdtlma == 14.0 ~ 1,
    educdtlma == 15.0 ~ 1,
    educdtlma == 16.0 ~ 1,
    educdtlma == 17.0 ~ 1,
    educdtlma == 18.0 ~ 2,
    educdtlma == 19.0 ~ 3,
    educdtlma == 20.0 ~ 4,
    TRUE ~ educdtlma
  ))

combined <- combined %>%
  mutate(educpa = case_when(
    educdtlpa == 1.0 ~ 0,
    educdtlpa == 2.0 ~ 0,
    educdtlpa == 3.0 ~ 0,
    educdtlpa == 4.0 ~ 0,
    educdtlpa == 5.0 ~ 1,
    educdtlpa == 6.0 ~ 1,
    educdtlpa == 7.0 ~ 1,
    educdtlpa == 8.0 ~ 1,
    educdtlpa == 9.0 ~ 1,
    educdtlpa == 10.0 ~ 1,
    educdtlpa == 11.0 ~ 1,
    educdtlpa == 12.0 ~ 1,
    educdtlpa == 13.0 ~ 1,
    educdtlpa == 14.0 ~ 1,
    educdtlpa == 15.0 ~ 1,
    educdtlpa == 16.0 ~ 1,
    educdtlpa == 17.0 ~ 1,
    educdtlpa == 18.0 ~ 2,
    educdtlpa == 19.0 ~ 3,
    educdtlpa == 20.0 ~ 4,
    TRUE ~ educdtlpa
  ))

# Define labels for detailed variables (using label text as names and code as values for haven)
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
  "Information lost" = -2,
  "Item not applicable" = -1
)

# Define labels for collapsed variables
educma_labels <- c(
  "NVQ 4–5: degree-level qualifications and above" = 0,
  "NVQ 1–3: sub-degree qualifications" = 1,
  "None/entry: training programmes below NVQ level" = 2,
  "Other: qualifications where level is unspecified" = 3,
  "No qualifications mentioned" = 4,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not asked" = -3,
  "Information lost" = -2,
  "Item not applicable" = -1
)

# Convert to labelled factors using haven
combined <- combined %>%
  mutate(
    educdtlma = haven::labelled(educdtlma, labels = educdtlma_labels),
    educdtlpa = haven::labelled(educdtlpa, labels = educdtlma_labels),
    educma = haven::labelled(educma, labels = educma_labels),
    educpa = haven::labelled(educpa, labels = educma_labels)
  )

# Select required variables
final_data <- combined %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")