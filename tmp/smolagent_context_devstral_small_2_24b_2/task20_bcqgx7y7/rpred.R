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
  if (wave == 1) {
    var <- case_when(
      var %in% c(-99, -97, -96, -92, -91, -1) ~ -3,  # Not interviewed, refused, not applicable, don't know
      TRUE ~ var
    )
  } else if (wave == 2) {
    var <- case_when(
      var %in% c(-998, -997, -995, -99, -97, -96, -92, -91, -1) ~ -2,  # Script error, not interviewed, etc.
      TRUE ~ var
    )
  } else if (wave == 3) {
    var <- case_when(
      var %in% c(-99, -97, -96, -92, -91, -1) ~ -3,  # Not interviewed, refused, not applicable, don't know
      TRUE ~ var
    )
  } else if (wave == 4) {
    var <- case_when(
      var %in% c(-99, -97, -96, -92, -91, -1) ~ -3,  # Not interviewed, refused, not applicable, don't know
      TRUE ~ var
    )
  } else if (wave == 6) {
    var <- case_when(
      var %in% c(-997, -97, -92, -91, -1) ~ -2,  # Script error, refused, not applicable, don't know
      TRUE ~ var
    )
  } else if (wave == 7) {
    var <- case_when(
      var %in% c(-996, -97, -92, -91, -1) ~ -2,  # Problem with feed forward, refused, not applicable, don't know
      TRUE ~ var
    )
  } else if (wave == 8) {
    var <- case_when(
      var %in% c(-9, -8, -3, -1) ~ -3,  # Refused, don't know, not asked, not applicable
      TRUE ~ var
    )
  } else if (wave == 9) {
    var <- case_when(
      var %in% c(-9, -8, -3, -1) ~ -3,  # Refused, don't know, not asked, not applicable
      TRUE ~ var
    )
  }
  return(var)
}

# Process each wave's alcohol consumption indicator
# Wave 1 (Age 14): Requires both W1alceverYP = 1 and W1alcmonYP = 1
wave1_alcohol <- merged_data %>%
  mutate(
    W1_alcohol = ifelse(
      (W1alceverYP == 1 & W1alcmonYP == 1), 1, 0
    )
  ) %>%
  mutate(W1_alcohol = map_missing(W1_alcohol, 1))

# Wave 2 (Age 15): W2alceverYP == 1
wave2_alcohol <- merged_data %>%
  mutate(W2_alcohol = ifelse(W2alceverYP == 1, 1, 0)) %>%
  mutate(W2_alcohol = map_missing(W2_alcohol, 2))

# Wave 3 (Age 16): W3alceverYP == 1
wave3_alcohol <- merged_data %>%
  mutate(W3_alcohol = ifelse(W3alceverYP == 1, 1, 0)) %>%
  mutate(W3_alcohol = map_missing(W3_alcohol, 3))

# Wave 4 (Age 17): W4AlcEverYP == 1
wave4_alcohol <- merged_data %>%
  mutate(W4_alcohol = ifelse(W4AlcEverYP == 1, 1, 0)) %>%
  mutate(W4_alcohol = map_missing(W4_alcohol, 4))

# Wave 6 (Age 19): W6AlcEverYP == 1
wave6_alcohol <- merged_data %>%
  mutate(W6_alcohol = ifelse(W6AlcEverYP == 1, 1, 0)) %>%
  mutate(W6_alcohol = map_missing(W6_alcohol, 6))

# Wave 7 (Age 20): W7AlcEverYP == 1
wave7_alcohol <- merged_data %>%
  mutate(W7_alcohol = ifelse(W7AlcEverYP == 1, 1, 0)) %>%
  mutate(W7_alcohol = map_missing(W7_alcohol, 7))

# Wave 8 (Age 25): W8AUDIT1 > 1 (AUDIT frequency above "Never")
wave8_alcohol <- merged_data %>%
  mutate(W8_alcohol = ifelse(W8AUDIT1 > 1, 1, 0)) %>%
  mutate(W8_alcohol = map_missing(W8_alcohol, 8))

# Wave 9 (Age 32): W9AUDIT1 > 1 (AUDIT frequency above "Never")
wave9_alcohol <- merged_data %>%
  mutate(W9_alcohol = ifelse(W9AUDIT1 > 1, 1, 0)) %>%
  mutate(W9_alcohol = map_missing(W9_alcohol, 9))

# Combine all waves into a single dataset
combined_alcohol <- merged_data %>%
  left_join(wave1_alcohol %>% select(NSID, W1_alcohol), by = "NSID") %>%
  left_join(wave2_alcohol %>% select(NSID, W2_alcohol), by = "NSID") %>%
  left_join(wave3_alcohol %>% select(NSID, W3_alcohol), by = "NSID") %>%
  left_join(wave4_alcohol %>% select(NSID, W4_alcohol), by = "NSID") %>%
  left_join(wave6_alcohol %>% select(NSID, W6_alcohol), by = "NSID") %>%
  left_join(wave7_alcohol %>% select(NSID, W7_alcohol), by = "NSID") %>%
  left_join(wave8_alcohol %>% select(NSID, W8_alcohol), by = "NSID") %>%
  left_join(wave9_alcohol %>% select(NSID, W9_alcohol), by = "NSID")

# Derive alcfst: earliest age at which alcohol consumption is recorded
# Initialize alcfst with -8 (don't know / insufficient information)
combined_alcohol <- combined_alcohol %>%
  mutate(alcfst = -8)

# Check each wave in order of increasing age
for (age in c(14, 15, 16, 17, 19, 20, 25, 32)) {
  wave_var <- paste0("W", gsub("14", "1", gsub("15", "2", gsub("16", "3", gsub("17", "4", gsub("19", "6", gsub("20", "7", gsub("25", "8", gsub("32", "9", as.character(age))))))))), "_alcohol")
  
  combined_alcohol <- combined_alcohol %>%
    mutate(
      alcfst = case_when(
        alcfst == -8 & get(wave_var) == 1 ~ age,  # Drinking observed, assign age
        alcfst == -8 & get(wave_var) == 0 ~ 99,   # Not drinking, assign 99 (never had alcohol)
        TRUE ~ alcfst
      )
    )
}

# Finalize alcfst: if all observed indicators show not-drinking and none are missing, assign 99
combined_alcohol <- combined_alcohol %>%
  mutate(
    alcfst = case_when(
      (W1_alcohol == 0 | is.na(W1_alcohol)) & 
      (W2_alcohol == 0 | is.na(W2_alcohol)) & 
      (W3_alcohol == 0 | is.na(W3_alcohol)) & 
      (W4_alcohol == 0 | is.na(W4_alcohol)) & 
      (W6_alcohol == 0 | is.na(W6_alcohol)) & 
      (W7_alcohol == 0 | is.na(W7_alcohol)) & 
      (W8_alcohol == 0 | is.na(W8_alcohol)) & 
      (W9_alcohol == 0 | is.na(W9_alcohol)) ~ 99,
      TRUE ~ alcfst
    )
  )

# Convert alcfst to a factor with appropriate levels and labels
combined_alcohol$alcfst <- factor(
  combined_alcohol$alcfst,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")
)

# Select only NSID and alcfst for output
output_data <- combined_alcohol %>%
  select(NSID, alcfst)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")
