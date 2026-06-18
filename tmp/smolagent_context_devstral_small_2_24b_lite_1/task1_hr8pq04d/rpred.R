library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define a function to harmonize missing values for a given variable
harmonize_missing <- function(var, wave) {
  case_when(
    var %in% c(-92, -9) ~ -9,  # Refusal
    var %in% c(-91, -1) ~ -1,  # Not applicable
    var %in% c(-99, -999, -998, -997, -995, -94) ~ -3,  # Not interviewed / script error / information lost
    var %in% c(-8, -92) ~ -8,  # Don't know
    var %in% c(-7) ~ -7,  # Prefer not to say
    TRUE ~ var
  )
}

# Harmonize each wave's sex variable
merged_data <- merged_data %>%
  mutate(
    W1sexYP = harmonize_missing(W1sexYP, "wave1"),
    W2SexYP = harmonize_missing(W2SexYP, "wave2"),
    W3sexYP = harmonize_missing(W3sexYP, "wave3"),
    W4SexYP = harmonize_missing(W4SexYP, "wave4"),
    W5SexYP = harmonize_missing(W5SexYP, "wave5"),
    W6Sex = harmonize_missing(W6Sex, "wave6"),
    W7Sex = harmonize_missing(W7Sex, "wave7"),
    W8CMSEX = harmonize_missing(W8CMSEX, "wave8"),
    W9DSEX = harmonize_missing(W9DSEX, "wave9")
  )

# Derive the consolidated sex variable using most-recent-valid-first
merged_data <- merged_data %>%
  mutate(
    sex = coalesce(W9DSEX, W8CMSEX, W7Sex, W6Sex, W5SexYP, W4SexYP, W3sexYP, W2SexYP, W1sexYP)
  )

# Convert missing values to -3 for the consolidated variable
merged_data$sex <- ifelse(is.na(merged_data$sex), -3, merged_data$sex)

# Create a labelled factor for the consolidated sex variable
merged_data$sex <- factor(merged_data$sex, levels = c(1, 2, -9, -8, -7, -3, -2, -1), labels = c("Male", "Female", "Refusal", "Don't know", "Prefer not to say", "Not interviewed / script error / information lost", "Schedule not applicable / script error / information lost", "Not applicable"))

# Select only the NSID and the consolidated sex variable
output_data <- merged_data %>%
  select(NSID, sex)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")