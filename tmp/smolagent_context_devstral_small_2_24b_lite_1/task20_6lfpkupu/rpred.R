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
    var %in% c(-97, -7) ~ -7, # Prefer not to say
    var %in% c(-997, -996, -995, -99, -96, -91, -3) ~ -3, # Not interviewed / script error / not applicable
    var %in% c(-998, -999) ~ -2, # Schedule not applicable / script error / information lost
    TRUE ~ var
  )
}

# Process each wave's alcohol consumption variables
# Wave 1 (Age 14)
wave1_alcohol <- merged_data %>%
  mutate(
    W1alceverYP_clean = map_missing(W1alceverYP, 1)
  ) %>%
  select(NSID, W1alceverYP_clean)

# Wave 2 (Age 15)
wave2_alcohol <- merged_data %>%
  mutate(
    W2alceverYP_clean = map_missing(W2alceverYP, 2)
  ) %>%
  select(NSID, W2alceverYP_clean)

# Wave 3 (Age 16)
wave3_alcohol <- merged_data %>%
  mutate(
    W3alceverYP_clean = map_missing(W3alceverYP, 3)
  ) %>%
  select(NSID, W3alceverYP_clean)

# Wave 4 (Age 17)
wave4_alcohol <- merged_data %>%
  mutate(
    W4AlcEverYP_clean = map_missing(W4AlcEverYP, 4)
  ) %>%
  select(NSID, W4AlcEverYP_clean)

# Wave 6 (Age 19)
wave6_alcohol <- merged_data %>%
  mutate(
    W6AlcEverYP_clean = map_missing(W6AlcEverYP, 6)
  ) %>%
  select(NSID, W6AlcEverYP_clean)

# Wave 7 (Age 20)
wave7_alcohol <- merged_data %>%
  mutate(
    W7AlcEverYP_clean = map_missing(W7AlcEverYP, 7)
  ) %>%
  select(NSID, W7AlcEverYP_clean)

# Wave 8 (Age 25)
wave8_alcohol <- merged_data %>%
  mutate(
    W8AUDIT1_clean = map_missing(W8AUDIT1, 8)
  ) %>%
  select(NSID, W8AUDIT1_clean)

# Wave 9 (Age 32)
wave9_alcohol <- merged_data %>%
  mutate(
    W9AUDIT1_clean = map_missing(W9AUDIT1, 9)
  ) %>%
  select(NSID, W9AUDIT1_clean)

# Merge all cleaned alcohol variables
alcohol_data <- wave1_alcohol %>%
  full_join(wave2_alcohol, by = "NSID") %>%
  full_join(wave3_alcohol, by = "NSID") %>%
  full_join(wave4_alcohol, by = "NSID") %>%
  full_join(wave6_alcohol, by = "NSID") %>%
  full_join(wave7_alcohol, by = "NSID") %>%
  full_join(wave8_alcohol, by = "NSID") %>%
  full_join(wave9_alcohol, by = "NSID")

# Derive alcfst variable
alcohol_data <- alcohol_data %>%
  mutate(
    alcfst = case_when(
      coalesce(W1alceverYP_clean, W2alceverYP_clean, W3alceverYP_clean, W4AlcEverYP_clean, W6AlcEverYP_clean, W7AlcEverYP_clean, W8AUDIT1_clean, W9AUDIT1_clean) == 1 ~ 14,
      W1alceverYP_clean == 2 & W2alceverYP_clean == 2 & W3alceverYP_clean == 2 & W4AlcEverYP_clean == 2 & W6AlcEverYP_clean == 2 & W7AlcEverYP_clean == 2 & W8AUDIT1_clean == 1 & W9AUDIT1_clean == 1 ~ 25,
      W1alceverYP_clean == 2 & W2alceverYP_clean == 2 & W3alceverYP_clean == 2 & W4AlcEverYP_clean == 2 & W6AlcEverYP_clean == 2 & W7AlcEverYP_clean == 2 & W8AUDIT1_clean == 2 & W9AUDIT1_clean == 1 ~ 32,
      W1alceverYP_clean == 2 & W2alceverYP_clean == 2 & W3alceverYP_clean == 2 & W4AlcEverYP_clean == 2 & W6AlcEverYP_clean == 2 & W7AlcEverYP_clean == 2 & W8AUDIT1_clean == 2 & W9AUDIT1_clean == 2 ~ 99,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    alcfst = ifelse(is.na(alcfst), -3, alcfst)
  )

# Select only NSID and alcfst for final output
final_output <- alcohol_data %>%
  select(NSID, alcfst)

# Write the output
write_csv(final_output, "data/output/cleaned_data.csv")
