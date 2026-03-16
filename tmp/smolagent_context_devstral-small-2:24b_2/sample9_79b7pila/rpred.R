library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Select variables
wave_one <- wave_one %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave_two <- wave_two %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave_four <- wave_four %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Consolidate maternal education
merged_data <- merged_data %>%
  mutate(educdtlma = coalesce(W1hiqualmum, W2hiqualmum, w4hiqualmum))

# Consolidate paternal education
merged_data <- merged_data %>%
  mutate(educdtlpa = coalesce(W1hiqualdad, W2hiqualdad, w4hiqualdad))

# Derive collapsed variables
collapsed_educ <- function(detailed_var) {
  case_when(
    detailed_var %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4-5
    detailed_var %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,  # NVQ 1-3
    detailed_var %in% c(18) ~ 2,  # None/entry
    detailed_var %in% c(19) ~ 3,  # Other
    detailed_var %in% c(20) ~ 4,  # No qualifications mentioned
    detailed_var < 0 ~ detailed_var,  # Preserve missing codes
    TRUE ~ NA_integer_  # Default to NA
  )
}

merged_data <- merged_data %>%
  mutate(educma = collapsed_educ(educdtlma),
         educpa = collapsed_educ(educdtlpa))

# Harmonize missing value codes
harmonize_missing <- function(var) {
  case_when(
    var == -999.0 ~ -9,
    var == -99.0 ~ -8,
    var == -98.0 ~ -3,
    var == -94.0 ~ -8,
    var == -92.0 ~ -9,
    var == -91.0 ~ -1,
    var == -1.0 ~ -8,
    TRUE ~ var
  )
}

merged_data <- merged_data %>%
  mutate(educdtlma = harmonize_missing(educdtlma),
         educdtlpa = harmonize_missing(educdtlpa),
         educma = harmonize_missing(educma),
         educpa = harmonize_missing(educpa))

# Convert to labelled factors
merged_data <- merged_data %>%
  mutate(educdtlma = labelled::to_labelled(as.factor(educdtlma), labels = c(
    "-9" = "Refusal",
    "-8" = "Don't know/insufficient information",
    "-3" = "Not asked at the fieldwork stage/participated/interviewed",
    "-2" = "Schedule not applicable/Script error/information lost",
    "-1" = "Item not applicable",
    "1" = "Higher Degree",
    "2" = "First Degree",
    "3" = "HE Diploma",
    "4" = "HNC/HND/NVQ4",
    "5" = "Teaching qualification, non-degree",
    "6" = "Nursing qualification, non-degree",
    "7" = "A Levels",
    "8" = "OND/ONC",
    "9" = "City and guilds part III, NVQ3",
    "10" = "CSYS",
    "11" = "Scottish Higher Grade",
    "12" = "AS Level",
    "13" = "Trade apprenticeship",
    "14" = "City and guilds part II, NVQ2",
    "15" = "GCSE grade A-C and equivalent",
    "16" = "GCSE grade D-E and equivalent",
    "17" = "City and guilds part I, NVQ1",
    "18" = "Youth training, skill seekers",
    "19" = "Qualification, level unspecified",
    "20" = "No qualification mentioned"
  )),
  educdtlpa = labelled::to_labelled(as.factor(educdtlpa), labels = c(
    "-9" = "Refusal",
    "-8" = "Don't know/insufficient information",
    "-3" = "Not asked at the fieldwork stage/participated/interviewed",
    "-2" = "Schedule not applicable/Script error/information lost",
    "-1" = "Item not applicable",
    "1" = "Higher Degree",
    "2" = "First Degree",
    "3" = "HE Diploma",
    "4" = "HNC/HND/NVQ4",
    "5" = "Teaching qualification, non-degree",
    "6" = "Nursing qualification, non-degree",
    "7" = "A Levels",
    "8" = "OND/ONC",
    "9" = "City and guilds part III, NVQ3",
    "10" = "CSYS",
    "11" = "Scottish Higher Grade",
    "12" = "AS Level",
    "13" = "Trade apprenticeship",
    "14" = "City and guilds part II, NVQ2",
    "15" = "GCSE grade A-C and equivalent",
    "16" = "GCSE grade D-E and equivalent",
    "17" = "City and guilds part I, NVQ1",
    "18" = "Youth training, skill seekers",
    "19" = "Qualification, level unspecified",
    "20" = "No qualification mentioned"
  )),
  educma = labelled::to_labelled(as.factor(educma), labels = c(
    "-9" = "Refusal",
    "-8" = "Don't know/insufficient information",
    "-3" = "Not asked at the fieldwork stage/participated/interviewed",
    "-2" = "Schedule not applicable/Script error/information lost",
    "-1" = "Item not applicable",
    "0" = "NVQ 4-5",
    "1" = "NVQ 1-3",
    "2" = "None/entry",
    "3" = "Other",
    "4" = "No qualifications mentioned"
  )),
  educpa = labelled::to_labelled(as.factor(educpa), labels = c(
    "-9" = "Refusal",
    "-8" = "Don't know/insufficient information",
    "-3" = "Not asked at the fieldwork stage/participated/interviewed",
    "-2" = "Schedule not applicable/Script error/information lost",
    "-1" = "Item not applicable",
    "0" = "NVQ 4-5",
    "1" = "NVQ 1-3",
    "2" = "None/entry",
    "3" = "Other",
    "4" = "No qualifications mentioned"
  )))

# Select final variables
final_data <- merged_data %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)