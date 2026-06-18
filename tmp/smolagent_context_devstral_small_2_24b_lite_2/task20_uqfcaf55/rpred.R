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
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-92, -9) ~ -9,  # Refusal
    var %in% c(-1, -8) ~ -8,   # Don't know / insufficient information
    var %in% c(-97, -7) ~ -7,  # Prefer not to say
    var %in% c(-99, -96, -91, -3) ~ -3,  # Not interviewed / not applicable
    var %in% c(-999, -998, -997, -995, -94, -2) ~ -2,  # Schedule not applicable / script error
    TRUE ~ var
  )
}

# Process each wave's alcohol consumption variables
# Wave 1 (Age 14)
wave1_alcohol <- wave1 %>%
  mutate(W1alceverYP_clean = map_missing(W1alceverYP, "wave1"))

# Wave 2 (Age 15)
wave2_alcohol <- wave2 %>%
  mutate(W2alceverYP_clean = map_missing(W2alceverYP, "wave2"))

# Wave 3 (Age 16)
wave3_alcohol <- wave3 %>%
  mutate(W3alceverYP_clean = map_missing(W3alceverYP, "wave3"))

# Wave 4 (Age 17)
wave4_alcohol <- wave4 %>%
  mutate(W4AlcEverYP_clean = map_missing(W4AlcEverYP, "wave4"))

# Wave 6 (Age 19)
wave6_alcohol <- wave6 %>%
  mutate(W6AlcEverYP_clean = map_missing(W6AlcEverYP, "wave6"))

# Wave 7 (Age 20)
wave7_alcohol <- wave7 %>%
  mutate(W7AlcEverYP_clean = map_missing(W7AlcEverYP, "wave7"))

# Wave 8 (Age 25)
wave8_alcohol <- wave8 %>%
  mutate(W8AUDIT1_clean = map_missing(W8AUDIT1, "wave8"))

# Wave 9 (Age 32)
wave9_alcohol <- wave9 %>%
  mutate(W9AUDIT1_clean = map_missing(W9AUDIT1, "wave9"))

# Merge all processed alcohol variables
merged_alcohol <- merged_data %>%
  left_join(select(wave1_alcohol, NSID, W1alceverYP_clean), by = "NSID") %>%
  left_join(select(wave2_alcohol, NSID, W2alceverYP_clean), by = "NSID") %>%
  left_join(select(wave3_alcohol, NSID, W3alceverYP_clean), by = "NSID") %>%
  left_join(select(wave4_alcohol, NSID, W4AlcEverYP_clean), by = "NSID") %>%
  left_join(select(wave6_alcohol, NSID, W6AlcEverYP_clean), by = "NSID") %>%
  left_join(select(wave7_alcohol, NSID, W7AlcEverYP_clean), by = "NSID") %>%
  left_join(select(wave8_alcohol, NSID, W8AUDIT1_clean), by = "NSID") %>%
  left_join(select(wave9_alcohol, NSID, W9AUDIT1_clean), by = "NSID")

# Derive alcfst variable
# For each person, find the earliest age at which they consumed alcohol
# Use the earliest valid response (1 = Yes) across all waves
# If never consumed alcohol (all responses are 2 = No), set alcfst = 99

merged_alcohol <- merged_alcohol %>%
  mutate(
    alcfst = case_when(
      W1alceverYP_clean == 1 ~ 14,
      W2alceverYP_clean == 1 ~ 15,
      W3alceverYP_clean == 1 ~ 16,
      W4AlcEverYP_clean == 1 ~ 17,
      W6AlcEverYP_clean == 1 ~ 19,
      W7AlcEverYP_clean == 1 ~ 20,
      W8AUDIT1_clean %in% c(2, 3, 4, 5) ~ 25,  # AUDIT1 codes 2-5 indicate alcohol consumption
      W9AUDIT1_clean %in% c(2, 3, 4, 5) ~ 32,
      (W1alceverYP_clean == 2 | is.na(W1alceverYP_clean)) & 
      (W2alceverYP_clean == 2 | is.na(W2alceverYP_clean)) & 
      (W3alceverYP_clean == 2 | is.na(W3alceverYP_clean)) & 
      (W4AlcEverYP_clean == 2 | is.na(W4AlcEverYP_clean)) & 
      (W6AlcEverYP_clean == 2 | is.na(W6AlcEverYP_clean)) & 
      (W7AlcEverYP_clean == 2 | is.na(W7AlcEverYP_clean)) & 
      (W8AUDIT1_clean == 1 | is.na(W8AUDIT1_clean)) & 
      (W9AUDIT1_clean == 1 | is.na(W9AUDIT1_clean)) ~ 99,
      TRUE ~ NA_real_
    )
  )

# Replace NA with -3 (Not applicable / not interviewed)
merged_alcohol$alcfst[is.na(merged_alcohol$alcfst)] <- -3

# Select only NSID and alcfst for output
output_data <- merged_alcohol %>% select(NSID, alcfst)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"