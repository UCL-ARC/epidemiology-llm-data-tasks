library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022_derived_variables <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8_2015_derived, by = "NSID") %>%
  full_join(ns9_2022_derived_variables, by = "NSID")

# Standard missing-value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-7" = "Prefer not to say"
)

# Harmonize missing values for W6MarStatYP
merged_data <- merged_data %>%
  mutate(W6MarStatYP = case_when(
    W6MarStatYP == -999.0 ~ -3,
    W6MarStatYP == -997.0 ~ -2,
    W6MarStatYP == -97.0 ~ -3,
    W6MarStatYP == -92.0 ~ -9,
    W6MarStatYP == -91.0 ~ -1,
    W6MarStatYP == -1.0 ~ -8,
    TRUE ~ W6MarStatYP
  ))

# Harmonize missing values for W8DMARSTAT
merged_data <- merged_data %>%
  mutate(W8DMARSTAT = case_when(
    W8DMARSTAT == -9.0 ~ -9,
    W8DMARSTAT == -8.0 ~ -8,
    W8DMARSTAT == -1.0 ~ -1,
    TRUE ~ W8DMARSTAT
  ))

# Harmonize missing values for W9DMARSTAT
merged_data <- merged_data %>%
  mutate(W9DMARSTAT = case_when(
    W9DMARSTAT == -9.0 ~ -9,
    W9DMARSTAT == -8.0 ~ -8,
    TRUE ~ W9DMARSTAT
  ))

# Create harmonized marital status variables
merged_data <- merged_data %>%
  mutate(
    marstat14 = NA_integer_,
    marstat17 = NA_integer_,
    marstat19 = W6MarStatYP,
    marstat25 = W8DMARSTAT,
    marstat32 = W9DMARSTAT
  )

# Create detailed age-specific variables
merged_data <- merged_data %>%
  mutate(
    marstatadu19 = W6MarStatYP,
    marstatadu25 = W8DMARSTAT,
    marstatadu32 = W9DMARSTAT
  )

# Convert to factors with labels
merged_data <- merged_data %>%
  mutate(
    marstat19 = factor(marstat19, levels = c(-9, -8, -1, -3, -2, -7, 1, 2, 3, 4, 5),
                        labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                  "Not asked at the fieldwork stage/participated/interviewed", 
                                  "Schedule not applicable/Script error/information lost", 
                                  "Prefer not to say", "Single, that is never married", "Married", 
                                  "Separated", "Divorced", "Widowed")),
    marstat25 = factor(marstat25, levels = c(-9, -8, -1, -3, -2, -7, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                  "Not asked at the fieldwork stage/participated/interviewed", 
                                  "Schedule not applicable/Script error/information lost", 
                                  "Prefer not to say", "Single and never married or in a CP", "Married", 
                                  "Separated but still legally married", "Divorced", "Widowed", 
                                  "A Civil Partner", "Separated but still legally in a CP", 
                                  "A former Civil Partner", "A surviving Civil Partner")),
    marstat32 = factor(marstat32, levels = c(-9, -8, -1, -3, -2, -7, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                  "Not asked at the fieldwork stage/participated/interviewed", 
                                  "Schedule not applicable/Script error/information lost", 
                                  "Prefer not to say", "Single that is never married or never in a Civil Partnership", 
                                  "Married", "Divorced", "Legally separated", "Widowed", 
                                  "A Civil Partner in a legally recognised Civil Partnership", 
                                  "A former Civil Partner (where Civil Partnership legally dissolved)", 
                                  "A surviving Civil Partner (where Civil Partner has died)"))
  )

# Select final variables for output
final_data <- merged_data %>%
  select(NSID, marstat19, marstat25, marstat32, marstatadu19, marstatadu25, marstatadu32)

# Write to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)