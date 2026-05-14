library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- readr::read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Standardize missing values for numeric columns only
standardize_missing <- function(x) {
  x %>%
    mutate(across(where(is.numeric), ~ case_when(
      . == -999 | . == -998 | . == -997 | . == -995 ~ -2,
      . == -94 ~ -8,
      . == -92 ~ -9,
      . == -91 ~ -1,
      . == -99 ~ -3,
      . == -9 ~ -9,
      . == -8 ~ -8,
      . == -1 ~ -1,
      TRUE ~ .
    )))
}

merged_data <- standardize_missing(merged_data)

# Harmonize employment status variables
# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(ecoact17 = case_when(
    W4empsYP == 1 ~ 1,  # Doing paid work
    W4empsYP == 2 ~ 1,  # Doing paid work
    W4empsYP == 3 ~ 4,  # Unemployed
    W4empsYP == 4 ~ 5,  # Training
    W4empsYP == 5 ~ 6,  # Education
    W4empsYP == 6 ~ 9,  # Looking after family
    W4empsYP == 7 ~ 8,  # Retired
    W4empsYP == 8 ~ 7,  # Sick/Disabled
    W4empsYP == 9 ~ 10, # Other
    TRUE ~ as.numeric(W4empsYP)  # Pass through missing values as numeric
  ))

# Convert to factor with matching labels
levels_ecoact17 <- c(-9, -8, -3, -2, -1, 1, 4, 5, 6, 7, 8, 9, 10)
labels_ecoact17 <- c("Refusal", "Insufficient Info", "Not Asked", "Schedule Error", "Not Applicable",
                     "In Paid Work", "Unemployed", "Training", "Education", "Sick/Disabled",
                     "Looking After Home", "Other", "Retired")
merged_data$ecoact17 <- factor(merged_data$ecoact17, levels = levels_ecoact17, labels = labels_ecoact17)

# Wave 5 (Age 18)
merged_data <- merged_data %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 1 ~ 6,  # Apprenticeship
    W5mainactYP == 2 ~ 1,  # Paid work/college
    W5mainactYP == 3 ~ 1,  # Paid work
    W5mainactYP == 4 ~ 6,  # Education
    W5mainactYP == 5 ~ 5,  # Training
    W5mainactYP == 6 ~ 5,  # Training
    W5mainactYP == 7 ~ 4,  # Unemployed
    W5mainactYP == 8 ~ 9,  # Looking after family
    W5mainactYP == 9 ~ 10, # Waiting for course/job
    W5mainactYP == 10 ~ 10, # Waiting for exam results
    W5mainactYP == 11 ~ 10, # Waiting for job application
    TRUE ~ as.numeric(W5mainactYP)  # Pass through missing values as numeric
  ))

levels_ecoact18 <- c(-9, -8, -3, -2, -1, 1, 4, 5, 6, 9, 10)
labels_ecoact18 <- c("Refusal", "Insufficient Info", "Not Asked", "Schedule Error", "Not Applicable",
                     "In Paid Work", "Unemployed", "Training", "Education", "Looking After Home", "Waiting")
merged_data$ecoact18 <- factor(merged_data$ecoact18, levels = levels_ecoact18, labels = labels_ecoact18)

# Wave 6 (Age 19)
merged_data <- merged_data %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 1 ~ 6,  # University
    W6TCurrentAct == 2 ~ 6,  # Education
    W6TCurrentAct == 3 ~ 1,  # Paid work
    W6TCurrentAct == 4 ~ 5,  # Training
    W6TCurrentAct == 5 ~ 6,  # Apprenticeship
    W6TCurrentAct == 6 ~ 10, # Waiting
    W6TCurrentAct == 7 ~ 9,  # Looking after family
    W6TCurrentAct == 8 ~ 4,  # Unemployed
    W6TCurrentAct == 9 ~ 10, # Waiting
    W6TCurrentAct == 10 ~ 1, # Paid work/college
    W6TCurrentAct == 11 ~ 3, # Voluntary work
    TRUE ~ as.numeric(W6TCurrentAct)  # Pass through missing values as numeric
  ))

levels_ecoact19 <- c(-9, -8, -3, -2, -1, 1, 3, 4, 5, 6, 9, 10)
labels_ecoact19 <- c("Refusal", "Insufficient Info", "Not Asked", "Schedule Error", "Not Applicable",
                     "In Paid Work", "Voluntary Work", "Unemployed", "Training", "Education",
                     "Looking After Home", "Waiting")
merged_data$ecoact19 <- factor(merged_data$ecoact19, levels = levels_ecoact19, labels = labels_ecoact19)

# Wave 7 (Age 20)
merged_data <- merged_data %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 1 ~ 6,  # University
    W7TCurrentAct == 2 ~ 6,  # Education
    W7TCurrentAct == 3 ~ 1,  # Paid work
    W7TCurrentAct == 4 ~ 5,  # Training
    W7TCurrentAct == 5 ~ 6,  # Apprenticeship
    W7TCurrentAct == 6 ~ 10, # Waiting
    W7TCurrentAct == 7 ~ 9,  # Looking after family
    W7TCurrentAct == 8 ~ 4,  # Unemployed
    W7TCurrentAct == 9 ~ 1,  # Paid work/college
    W7TCurrentAct == 10 ~ 3, # Voluntary work
    W7TCurrentAct == 11 ~ 5, # Training
    W7TCurrentAct == 12 ~ 10, # Travelling
    W7TCurrentAct == 13 ~ 10, # Break
    W7TCurrentAct == 14 ~ 7,  # Sick/Disabled
    W7TCurrentAct == 15 ~ 10, # Not defined
    TRUE ~ as.numeric(W7TCurrentAct)  # Pass through missing values as numeric
  ))

levels_ecoact20 <- c(-9, -8, -3, -2, -1, 1, 3, 4, 5, 6, 7, 9, 10)
labels_ecoact20 <- c("Refusal", "Insufficient Info", "Not Asked", "Schedule Error", "Not Applicable",
                     "In Paid Work", "Voluntary Work", "Unemployed", "Training", "Education",
                     "Sick/Disabled", "Looking After Home", "Other")
merged_data$ecoact20 <- factor(merged_data$ecoact20, levels = levels_ecoact20, labels = labels_ecoact20)

# Wave 8 (Age 25)
merged_data <- merged_data %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1 ~ 1,  # Paid work
    W8DACTIVITYC == 2 ~ 2,  # Self-employed
    W8DACTIVITYC == 3 ~ 3,  # Voluntary work
    W8DACTIVITYC == 4 ~ 4,  # Unemployed
    W8DACTIVITYC == 5 ~ 5,  # Education
    W8DACTIVITYC == 6 ~ 6,  # Apprenticeship
    W8DACTIVITYC == 7 ~ 7,  # Training
    W8DACTIVITYC == 8 ~ 8,  # Sick/Disabled
    W8DACTIVITYC == 9 ~ 9,  # Looking after home
    W8DACTIVITYC == 10 ~ 10, # Other
    TRUE ~ as.numeric(W8DACTIVITYC)  # Pass through missing values as numeric
  ))

levels_ecoact25 <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
labels_ecoact25 <- c("Refusal", "Insufficient Info", "Not Asked", "Schedule Error", "Not Applicable",
                     "In Paid Work", "Self Employed", "Voluntary Work", "Unemployed", "Education",
                     "Apprenticeship", "Training", "Sick/Disabled", "Looking After Home", "Other")
merged_data$ecoact25 <- factor(merged_data$ecoact25, levels = levels_ecoact25, labels = labels_ecoact25)

# Wave 9 (Age 32)
merged_data <- merged_data %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1 ~ 1,  # Paid work
    W9DACTIVITYC == 2 ~ 2,  # Self-employed
    W9DACTIVITYC == 3 ~ 3,  # Voluntary work
    W9DACTIVITYC == 4 ~ 4,  # Unemployed
    W9DACTIVITYC == 5 ~ 5,  # Education
    W9DACTIVITYC == 6 ~ 6,  # Apprenticeship
    W9DACTIVITYC == 7 ~ 7,  # Training
    W9DACTIVITYC == 8 ~ 8,  # Sick/Disabled
    W9DACTIVITYC == 9 ~ 9,  # Looking after home
    W9DACTIVITYC == 10 ~ 10, # Other
    TRUE ~ as.numeric(W9DACTIVITYC)  # Pass through missing values as numeric
  ))

levels_ecoact32 <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
labels_ecoact32 <- c("Refusal", "Insufficient Info", "Not Asked", "Schedule Error", "Not Applicable",
                     "In Paid Work", "Self Employed", "Voluntary Work", "Unemployed", "Education",
                     "Apprenticeship", "Training", "Sick/Disabled", "Looking After Home", "Other")
merged_data$ecoact32 <- factor(merged_data$ecoact32, levels = levels_ecoact32, labels = labels_ecoact32)

# Select final variables
final_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)