library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to harmonize missing values based on metadata
harmonize_missing <- function(var, wave) {
  case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refused
    var %in% c(-94, -94.0) ~ -8,  # Insufficient information
    var %in% c(-97, -97.0) ~ -7,  # Prefer not to say
    var %in% c(-995, -995.0, -999, -999.0) ~ -2,  # Schedule not applicable / script error / information lost
    var %in% c(-91, -91.0) ~ -1,  # Item not applicable
    var %in% c(-99, -99.0, -100, -100.0) ~ -3,  # Not asked at the fieldwork stage / not interviewed
    TRUE ~ var
  )
}

# Function to map source variables to the 6-category scheme
map_to_collapsed <- function(var, wave) {
  case_when(
    # Wave 4 (Age 17): W4empsYP
    wave == "wave4" & var %in% c(1, 2) ~ 1,  # In paid work
    wave == "wave4" & var == 4 ~ 2,  # Apprenticeship / government training scheme / training
    wave == "wave4" & var == 5 ~ 3,  # Education
    wave == "wave4" & var == 3 ~ 4,  # Unemployed
    wave == "wave4" & var == 6 ~ 5,  # Looking after home / family
    wave == "wave4" & var %in% c(7, 8, 9) ~ 6,  # Other

    # Wave 5 (Age 18): W5mainactYP
    wave == "wave5" & var == 3 ~ 1,  # In paid work
    wave == "wave5" & var %in% c(1, 2, 5, 6) ~ 2,  # Apprenticeship / government training scheme / training
    wave == "wave5" & var %in% c(4, 10, 11) ~ 3,  # Education
    wave == "wave5" & var == 7 ~ 4,  # Unemployed
    wave == "wave5" & var == 8 ~ 5,  # Looking after home / family
    wave == "wave5" & var %in% c(9, 10, 11) ~ 6,  # Other

    # Wave 6 (Age 19): W6TCurrentAct
    wave == "wave6" & var == 3 ~ 1,  # In paid work
    wave == "wave6" & var %in% c(4, 5) ~ 2,  # Apprenticeship / government training scheme / training
    wave == "wave6" & var %in% c(1, 2) ~ 3,  # Education
    wave == "wave6" & var == 8 ~ 4,  # Unemployed
    wave == "wave6" & var == 7 ~ 5,  # Looking after home / family
    wave == "wave6" & var %in% c(6, 9, 10, 11) ~ 6,  # Other

    # Wave 7 (Age 20): W7TCurrentAct
    wave == "wave7" & var == 3 ~ 1,  # In paid work
    wave == "wave7" & var %in% c(4, 5, 11) ~ 2,  # Apprenticeship / government training scheme / training
    wave == "wave7" & var %in% c(1, 2) ~ 3,  # Education
    wave == "wave7" & var == 8 ~ 4,  # Unemployed
    wave == "wave7" & var == 7 ~ 5,  # Looking after home / family
    wave == "wave7" & var %in% c(6, 9, 10, 12, 13, 14, 15) ~ 6,  # Other

    # Wave 8 (Age 25): W8DACTIVITYC
    wave == "wave8" & var %in% c(1, 2) ~ 1,  # In paid work
    wave == "wave8" & var %in% c(6, 7) ~ 2,  # Apprenticeship / government training scheme / training
    wave == "wave8" & var == 5 ~ 3,  # Education
    wave == "wave8" & var == 4 ~ 4,  # Unemployed
    wave == "wave8" & var == 9 ~ 5,  # Looking after home / family
    wave == "wave8" & var %in% c(3, 8, 10) ~ 6,  # Other

    # Wave 9 (Age 32): W9DACTIVITYC
    wave == "wave9" & var %in% c(1, 2) ~ 1,  # In paid work
    wave == "wave9" & var %in% c(6, 7) ~ 2,  # Apprenticeship / government training scheme / training
    wave == "wave9" & var == 5 ~ 3,  # Education
    wave == "wave9" & var == 4 ~ 4,  # Unemployed
    wave == "wave9" & var == 9 ~ 5,  # Looking after home / family
    wave == "wave9" & var %in% c(3, 8, 10) ~ 6,  # Other

    TRUE ~ var
  )
}

# Derive collapsed variables
merged_data <- merged_data %>%
  mutate(
    ecoact17 = map_to_collapsed(W4empsYP, "wave4"),
    ecoact18 = map_to_collapsed(W5mainactYP, "wave5"),
    ecoact19 = map_to_collapsed(W6TCurrentAct, "wave6"),
    ecoact20 = map_to_collapsed(W7TCurrentAct, "wave7"),
    ecoact25 = map_to_collapsed(W8DACTIVITYC, "wave8"),
    ecoact32 = map_to_collapsed(W9DACTIVITYC, "wave9")
  )

# Derive detailed variables for ages 25 and 32
merged_data <- merged_data %>%
  mutate(
    ecoactadu25 = W8DACTIVITYC,
    ecoactadu32 = W9DACTIVITYC
  )

# Harmonize missing values for all derived variables
merged_data <- merged_data %>%
  mutate(
    ecoact17 = harmonize_missing(ecoact17, "wave4"),
    ecoact18 = harmonize_missing(ecoact18, "wave5"),
    ecoact19 = harmonize_missing(ecoact19, "wave6"),
    ecoact20 = harmonize_missing(ecoact20, "wave7"),
    ecoact25 = harmonize_missing(ecoact25, "wave8"),
    ecoact32 = harmonize_missing(ecoact32, "wave9"),
    ecoactadu25 = harmonize_missing(ecoactadu25, "wave8"),
    ecoactadu32 = harmonize_missing(ecoactadu32, "wave9")
  )

# Select only NSID and derived variables
output_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
