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

# Function to map missing values to standard codes
map_missing <- function(var) {
  case_when(
    var %in% c(-92, -9) ~ -9,  # Refusal
    var %in% c(-8, -94) ~ -8,  # Don't know / insufficient information
    var %in% c(-7) ~ -7,      # Prefer not to say
    var %in% c(-999, -998, -997, -995, -2) ~ -2,  # Not asked / not interviewed / script error
    var %in% c(-99, -91, -1) ~ -1,  # Item not applicable
    TRUE ~ var
  )
}

# Process each wave's sex variable
wave1$W1sexYP <- map_missing(wave1$W1sexYP)
wave2$W2SexYP <- map_missing(wave2$W2SexYP)
wave3$W3sexYP <- map_missing(wave3$W3sexYP)
wave4$W4SexYP <- map_missing(wave4$W4SexYP)
wave5$W5SexYP <- map_missing(wave5$W5SexYP)
wave6$W6Sex <- map_missing(wave6$W6Sex)
wave7$W7Sex <- map_missing(wave7$W7Sex)
wave8$W8CMSEX <- map_missing(wave8$W8CMSEX)
wave9$W9DSEX <- map_missing(wave9$W9DSEX)

# Create a consolidated sex variable using most-recent-valid-first
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      !is.na(W9DSEX) & W9DSEX %in% c(1, 2) ~ W9DSEX,
      !is.na(W8CMSEX) & W8CMSEX %in% c(1, 2) ~ W8CMSEX,
      !is.na(W7Sex) & W7Sex %in% c(1, 2) ~ W7Sex,
      !is.na(W6Sex) & W6Sex %in% c(1, 2) ~ W6Sex,
      !is.na(W5SexYP) & W5SexYP %in% c(1, 2) ~ W5SexYP,
      !is.na(W4SexYP) & W4SexYP %in% c(1, 2) ~ W4SexYP,
      !is.na(W3sexYP) & W3sexYP %in% c(1, 2) ~ W3sexYP,
      !is.na(W2SexYP) & W2SexYP %in% c(1, 2) ~ W2SexYP,
      !is.na(W1sexYP) & W1sexYP %in% c(1, 2) ~ W1sexYP,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    sex = ifelse(is.na(sex), -3, sex)
  )

# Convert sex to a labelled factor
merged_data$sex <- factor(merged_data$sex, levels = c(1, 2, -9, -8, -7, -3, -2, -1), labels = c("Male", "Female", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost", "Item not applicable"))

# Select only NSID and the consolidated sex variable
output_data <- merged_data %>%
  select(NSID, sex)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print a summary to verify
cat("Output file created successfully.\n")
cat("Summary of the output data:\n")
summary(output_data)