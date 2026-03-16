# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all waves by NSID using full_join
data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Recode sexual orientation variables to standard missing value codes
# Standard codes:
# -9 = Refusal
# -8 = Don't know/insufficient information
# -7 = Prefer not to say
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# -2 = Schedule not applicable

# Recode W6SexualityYP (Age 19) -> sori19
# Original codes: -97 (Respondent declined self completion), -92 (Refused), -91 (Not applicable), -1 (Don't know), 1-4 (valid)
data <- data %>%
  mutate(sori19 = case_when(
    W6SexualityYP %in% c(-97) ~ -3,  # Respondent declined self completion -> Not asked
    W6SexualityYP %in% c(-92) ~ -9,  # Refused -> Refusal
    W6SexualityYP %in% c(-91) ~ -1,  # Not applicable -> Not applicable
    W6SexualityYP %in% c(-1) ~ -8,   # Don't know -> Don't know
    W6SexualityYP %in% c(1:4) ~ as.numeric(W6SexualityYP),  # Valid responses
    is.na(W6SexualityYP) ~ -3,  # NULL -> Not asked
    TRUE ~ as.numeric(W6SexualityYP)
  ))

# Recode W7SexualityYP (Age 20) -> sori20
# Original codes: -100 (Respondent declined sexual experience questions), -97 (Refused self completion), -92 (Refused), -91 (Not applicable), -1 (Don't know), 1-4 (valid)
data <- data %>%
  mutate(sori20 = case_when(
    W7SexualityYP %in% c(-100, -97) ~ -3,  # Declined/Refused self completion -> Not asked
    W7SexualityYP %in% c(-92) ~ -9,  # Refused -> Refusal
    W7SexualityYP %in% c(-91) ~ -1,  # Not applicable -> Not applicable
    W7SexualityYP %in% c(-1) ~ -8,   # Don't know -> Don't know
    W7SexualityYP %in% c(1:4) ~ as.numeric(W7SexualityYP),  # Valid responses
    is.na(W7SexualityYP) ~ -3,  # NULL -> Not asked
    TRUE ~ as.numeric(W7SexualityYP)
  ))

# Recode W8SEXUALITY (Wave 8) -> sori25
# Original codes: -9 (Refused), -8 (Don't know), -1 (Not applicable), 1-4 (valid)
data <- data %>%
  mutate(sori25 = case_when(
    W8SEXUALITY %in% c(-9) ~ -9,   # Refused -> Refusal
    W8SEXUALITY %in% c(-8) ~ -8,   # Don't know -> Don't know
    W8SEXUALITY %in% c(-1) ~ -1,   # Not applicable -> Not applicable
    W8SEXUALITY %in% c(1:4) ~ as.numeric(W8SEXUALITY),  # Valid responses
    is.na(W8SEXUALITY) ~ -3,  # NULL -> Not asked
    TRUE ~ as.numeric(W8SEXUALITY)
  ))

# Recode W9SORI (Age 32) -> sori32
# Original codes: -9 (Refused), -8 (Don't know), -3 (Not asked at fieldwork stage), -1 (Not applicable), 1-5 (valid, where 5 = Prefer not to say)
data <- data %>%
  mutate(sori32 = case_when(
    W9SORI %in% c(-9) ~ -9,   # Refused -> Refusal
    W9SORI %in% c(-8) ~ -8,   # Don't know -> Don't know
    W9SORI %in% c(-3) ~ -3,   # Not asked -> Not asked
    W9SORI %in% c(-1) ~ -1,   # Not applicable -> Not applicable
    W9SORI %in% c(5) ~ -7,    # Prefer not to say -> Prefer not to say
    W9SORI %in% c(1:4) ~ as.numeric(W9SORI),  # Valid responses
    is.na(W9SORI) ~ -3,  # NULL -> Not asked
    TRUE ~ as.numeric(W9SORI)
  ))

# Select final output variables (ID and derived variables only)
output_data <- data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Ensure output directory exists
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")