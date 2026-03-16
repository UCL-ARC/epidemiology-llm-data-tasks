library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")

# Select variables
selected_data <- merged_data %>% 
  select(NSID, W1hiqualmum, W1hiqualdad, W2hiqualmum, W2hiqualdad, w4hiqualmum, w4hiqualdad)

# Consolidate maternal education
educdtlma <- coalesce(selected_data$w4hiqualmum, selected_data$W2hiqualmum, selected_data$W1hiqualmum)

# Consolidate paternal education
educdtlpa <- coalesce(selected_data$w4hiqualdad, selected_data$W2hiqualdad, selected_data$W1hiqualdad)

# Create collapsed variables
educma <- case_when(
  educdtlma %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4-5
  educdtlma %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,  # NVQ 1-3
  educdtlma %in% c(18) ~ 2,  # None/entry
  educdtlma %in% c(19) ~ 3,  # Other
  educdtlma %in% c(20) ~ 4,  # No qualifications mentioned
  TRUE ~ educdtlma  # Preserve missing codes
)

educpa <- case_when(
  educdtlpa %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4-5
  educdtlpa %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,  # NVQ 1-3
  educdtlpa %in% c(18) ~ 2,  # None/entry
  educdtlpa %in% c(19) ~ 3,  # Other
  educdtlpa %in% c(20) ~ 4,  # No qualifications mentioned
  TRUE ~ educdtlpa  # Preserve missing codes
)

# Harmonize missing value codes
educdtlma <- case_when(
  educdtlma == -999.0 ~ -9,
  educdtlma == -99.0 ~ -8,
  educdtlma == -98.0 ~ -8,
  educdtlma == -94.0 ~ -8,
  educdtlma == -92.0 ~ -9,
  educdtlma == -91.0 ~ -1,
  educdtlma == -1.0 ~ -8,
  TRUE ~ educdtlma
)

educdtlpa <- case_when(
  educdtlpa == -999.0 ~ -9,
  educdtlpa == -99.0 ~ -8,
  educdtlpa == -98.0 ~ -8,
  educdtlpa == -94.0 ~ -8,
  educdtlpa == -92.0 ~ -9,
  educdtlpa == -91.0 ~ -1,
  educdtlpa == -1.0 ~ -8,
  TRUE ~ educdtlpa
)

educma <- case_when(
  educma == -999.0 ~ -9,
  educma == -99.0 ~ -8,
  educma == -98.0 ~ -8,
  educma == -94.0 ~ -8,
  educma == -92.0 ~ -9,
  educma == -91.0 ~ -1,
  educma == -1.0 ~ -8,
  TRUE ~ educma
)

educpa <- case_when(
  educpa == -999.0 ~ -9,
  educpa == -99.0 ~ -8,
  educpa == -98.0 ~ -8,
  educpa == -94.0 ~ -8,
  educpa == -92.0 ~ -9,
  educpa == -91.0 ~ -1,
  educpa == -1.0 ~ -8,
  TRUE ~ educpa
)

# Convert to factor variables with labels
educdtlma <- factor(educdtlma, levels = c(-9, -8, -3, -2, -1, 1:20),
                     labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable",
                               "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4", "Teaching qualification, non-degree", "Nursing qualification, non-degree", "A Levels", "OND/ONC", "City and guilds part III, NVQ3", "CSYS", "Scottish Higher Grade", "AS Level", "Trade apprenticeship", "City and guilds part II, NVQ2", "GCSE grade A-C and equivalent", "GCSE grade D-E and equivalent", "City and guilds part I, NVQ1", "Youth training, skill seekers", "Qualification, level unspecified", "No qualification mentioned"))

educdtlpa <- factor(educdtlpa, levels = c(-9, -8, -3, -2, -1, 1:20),
                     labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable",
                               "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4", "Teaching qualification, non-degree", "Nursing qualification, non-degree", "A Levels", "OND/ONC", "City and guilds part III, NVQ3", "CSYS", "Scottish Higher Grade", "AS Level", "Trade apprenticeship", "City and guilds part II, NVQ2", "GCSE grade A-C and equivalent", "GCSE grade D-E and equivalent", "City and guilds part I, NVQ1", "Youth training, skill seekers", "Qualification, level unspecified", "No qualification mentioned"))

educma <- factor(educma, levels = c(-9, -8, -3, -2, -1, 0:4),
                  labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable",
                            "NVQ 4-5: degree-level qualifications and above", "NVQ 1-3: sub-degree qualifications", "None/entry: training programmes below NVQ level", "Other: qualifications where the level is unspecified", "No qualifications mentioned"))

educpa <- factor(educpa, levels = c(-9, -8, -3, -2, -1, 0:4),
                  labels = c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable/Script error/information lost", "Item not applicable",
                            "NVQ 4-5: degree-level qualifications and above", "NVQ 1-3: sub-degree qualifications", "None/entry: training programmes below NVQ level", "Other: qualifications where the level is unspecified", "No qualifications mentioned"))

# Create final dataset
final_data <- data.frame(NSID = selected_data$NSID, educma, educpa, educdtlma, educdtlpa)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)