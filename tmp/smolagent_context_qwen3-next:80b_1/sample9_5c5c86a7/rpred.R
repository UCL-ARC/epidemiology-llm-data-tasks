library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load data
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Harmonize missing codes for wave1
wave1 <- wave1 %>%
  mutate(
    W1hiqualmum = case_when(
      W1hiqualmum == -999.0 ~ -2,
      W1hiqualmum == -99.0 ~ -3,
      W1hiqualmum == -98.0 ~ -3,
      W1hiqualmum == -94.0 ~ -8,
      W1hiqualmum == -92.0 ~ -9,
      W1hiqualmum == -91.0 ~ -1,
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
    )
  )

# Harmonize missing codes for wave2
wave2 <- wave2 %>%
  mutate(
    W2hiqualmum = case_when(
      W2hiqualmum == -999.0 ~ -2,
      W2hiqualmum == -99.0 ~ -3,
      W2hiqualmum == -98.0 ~ -3,
      W2hiqualmum == -94.0 ~ -8,
      W2hiqualmum == -92.0 ~ -9,
      W2hiqualmum == -91.0 ~ -1,
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
    )
  )

# Harmonize missing codes for wave4
wave4 <- wave4 %>%
  mutate(
    w4hiqualmum = case_when(
      w4hiqualmum == -999.0 ~ -2,
      w4hiqualmum == -99.0 ~ -3,
      w4hiqualmum == -98.0 ~ -3,
      w4hiqualmum == -94.0 ~ -8,
      TRUE ~ w4hiqualmum
    ),
    w4hiqualdad = case_when(
      w4hiqualdad == -999.0 ~ -2,
      w4hiqualdad == -99.0 ~ -3,
      w4hiqualdad == -98.0 ~ -3,
      w4hiqualdad == -94.0 ~ -8,
      TRUE ~ w4hiqualdad
    )
  )

# Select needed variables
wave1_selected <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2_selected <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4_selected <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge datasets
combined <- wave1_selected %>%
  full_join(wave2_selected, by = "NSID") %>%
  full_join(wave4_selected, by = "NSID")

# Consolidate detailed maternal education
combined <- combined %>%
  mutate(
    educdtlma = case_when(
      W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ W1hiqualmum,
      W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ W2hiqualmum,
      w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ w4hiqualmum,
      TRUE ~ coalesce(W1hiqualmum, W2hiqualmum, w4hiqualmum)
    )
  )

# Consolidate detailed paternal education
combined <- combined %>%
  mutate(
    educdtlpa = case_when(
      W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ W1hiqualdad,
      W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ W2hiqualdad,
      w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ w4hiqualdad,
      TRUE ~ coalesce(W1hiqualdad, W2hiqualdad, w4hiqualdad)
    )
  )

# Create collapsed maternal education
combined <- combined %>%
  mutate(
    educma = case_when(
      educdtlma == 1 ~ 0,
      educdtlma == 2 ~ 0,
      educdtlma == 3 ~ 0,
      educdtlma == 4 ~ 0,
      educdtlma == 5 ~ 1,
      educdtlma == 6 ~ 1,
      educdtlma == 7 ~ 1,
      educdtlma == 8 ~ 1,
      educdtlma == 9 ~ 1,
      educdtlma == 10 ~ 1,
      educdtlma == 11 ~ 1,
      educdtlma == 12 ~ 1,
      educdtlma == 13 ~ 1,
      educdtlma == 14 ~ 1,
      educdtlma == 15 ~ 1,
      educdtlma == 16 ~ 1,
      educdtlma == 17 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3,
      educdtlma == 20 ~ 4,
      TRUE ~ educdtlma
    )
  )

# Create collapsed paternal education
combined <- combined %>%
  mutate(
    educpa = case_when(
      educdtlpa == 1 ~ 0,
      educdtlpa == 2 ~ 0,
      educdtlpa == 3 ~ 0,
      educdtlpa == 4 ~ 0,
      educdtlpa == 5 ~ 1,
      educdtlpa == 6 ~ 1,
      educdtlpa == 7 ~ 1,
      educdtlpa == 8 ~ 1,
      educdtlpa == 9 ~ 1,
      educdtlpa == 10 ~ 1,
      educdtlpa == 11 ~ 1,
      educdtlpa == 12 ~ 1,
      educdtlpa == 13 ~ 1,
      educdtlpa == 14 ~ 1,
      educdtlpa == 15 ~ 1,
      educdtlpa == 16 ~ 1,
      educdtlpa == 17 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ educdtlpa
    )
  )

# Set value labels for detailed maternal education
combined <- combined %>%
  mutate(
    educdtlma = {
      attr(educdtlma, "label") <- "Mother's detailed education"
      val_labels(educdtlma) <- c(
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
        "Schedule not applicable" = -2,
        "Item not applicable" = -1
      )
      educdtlma
    }
  )

# Set value labels for detailed paternal education
combined <- combined %>%
  mutate(
    educdtlpa = {
      attr(educdtlpa, "label") <- "Father's detailed education"
      val_labels(educdtlpa) <- c(
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
        "Schedule not applicable" = -2,
        "Item not applicable" = -1
      )
      educdtlpa
    }
  )

# Set value labels for collapsed maternal education
combined <- combined %>%
  mutate(
    educma = {
      attr(educma, "label") <- "Mother's collapsed education"
      val_labels(educma) <- c(
        "NVQ 4–5" = 0,
        "NVQ 1–3" = 1,
        "None/entry" = 2,
        "Other" = 3,
        "No qualifications mentioned" = 4,
        "Refusal" = -9,
        "Don't know/insufficient information" = -8,
        "Not asked" = -3,
        "Schedule not applicable" = -2,
        "Item not applicable" = -1
      )
      educma
    }
  )

# Set value labels for collapsed paternal education
combined <- combined %>%
  mutate(
    educpa = {
      attr(educpa, "label") <- "Father's collapsed education"
      val_labels(educpa) <- c(
        "NVQ 4–5" = 0,
        "NVQ 1–3" = 1,
        "None/entry" = 2,
        "Other" = 3,
        "No qualifications mentioned" = 4,
        "Refusal" = -9,
        "Don't know/insufficient information" = -8,
        "Not asked" = -3,
        "Schedule not applicable" = -2,
        "Item not applicable" = -1
      )
      educpa
    }
  )

# Select only required variables
cleaned_data <- combined %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")