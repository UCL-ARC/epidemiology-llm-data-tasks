library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all datasets from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Start with wave 1 NSIDs to ensure all NSIDs are present
data <- wave1 %>% select(NSID)

# Add wave 4 (age 17)
data <- data %>% full_join(wave4 %>% select(NSID, W4nsseccatYP), by = "NSID")

# Add wave 5 (age 18)
data <- data %>% full_join(wave5 %>% select(NSID, W5nsseccatYP), by = "NSID")

# Add wave 6 (age 19)
data <- data %>% full_join(wave6 %>% select(NSID, w6nsseccatYP), by = "NSID")

# Add wave 7 (age 20)
data <- data %>% full_join(wave7 %>% select(NSID, W7NSSECCat), by = "NSID")

# Add wave 8 (age 25) - includes NS-SEC and activity for student derivation
data <- data %>% full_join(wave8 %>% select(NSID, W8DNSSEC17, W8DACTIVITYC), by = "NSID")

# Add wave 9 (age 32)
data <- data %>% full_join(wave9 %>% select(NSID, W9NSSEC), by = "NSID")

# Rename columns to standard convention
data <- data %>% rename(
  nssec17 = W4nsseccatYP,
  nssec18 = W5nsseccatYP,
  nssec19 = w6nsseccatYP,
  nssec20 = W7NSSECCat,
  nssec25_raw = W8DNSSEC17,
  activity25 = W8DACTIVITYC,
  nssec32 = W9NSSEC
)

# Process NS-SEC variables
# Wave 4 (age 17): -99 = Not interviewed -> -3, -91 = Not applicable -> -1
data <- data %>% mutate(nssec17 = case_when(
  is.na(nssec17) ~ -3,
  nssec17 == -99 ~ -3,
  nssec17 == -91 ~ -1,
  TRUE ~ nssec17
))
# Collapse fractional codes for age 17
data <- data %>% mutate(nssec17 = ifelse(nssec17 > 0, floor(nssec17), nssec17))

# Wave 5 (age 18): -91 = Not applicable -> -1
data <- data %>% mutate(nssec18 = case_when(
  is.na(nssec18) ~ -3,
  nssec18 == -91 ~ -1,
  TRUE ~ nssec18
))
# Collapse fractional codes for age 18
data <- data %>% mutate(nssec18 = ifelse(nssec18 > 0, floor(nssec18), nssec18))

# Wave 6 (age 19): -91 = Not applicable -> -1
data <- data %>% mutate(nssec19 = case_when(
  is.na(nssec19) ~ -3,
  nssec19 == -91 ~ -1,
  TRUE ~ nssec19
))
# Collapse fractional codes for age 19
data <- data %>% mutate(nssec19 = ifelse(nssec19 > 0, floor(nssec19), nssec19))

# Wave 7 (age 20): -91 = Not applicable -> -1
data <- data %>% mutate(nssec20 = case_when(
  is.na(nssec20) ~ -3,
  nssec20 == -91 ~ -1,
  TRUE ~ nssec20
))
# Collapse fractional codes for age 20
data <- data %>% mutate(nssec20 = ifelse(nssec20 > 0, floor(nssec20), nssec20))

# Wave 8 (age 25): -9 = Refused -> -9, -8 = Insufficient info -> -8, -1 = Not applicable -> -1
data <- data %>% mutate(nssec25_raw = case_when(
  is.na(nssec25_raw) ~ -3,
  nssec25_raw == -9 ~ -9,
  nssec25_raw == -8 ~ -8,
  nssec25_raw == -1 ~ -1,
  TRUE ~ nssec25_raw
))
# Collapse fractional codes for age 25
data <- data %>% mutate(nssec25_raw = ifelse(nssec25_raw > 0, floor(nssec25_raw), nssec25_raw))

# Derive full-time student category (15) at age 25 using activity variable
data <- data %>% mutate(nssec25 = case_when(
  nssec25_raw %in% c(-9, -8, -7, -3, -2, -1) ~ nssec25_raw,
  activity25 == 5 ~ 15,
  TRUE ~ nssec25_raw
))

# Wave 9 (age 32): -1 = Not applicable -> -1
data <- data %>% mutate(nssec32 = case_when(
  is.na(nssec32) ~ -3,
  nssec32 == -1 ~ -1,
  TRUE ~ nssec32
))
# Collapse fractional codes for age 32
data <- data %>% mutate(nssec32 = ifelse(nssec32 > 0, floor(nssec32), nssec32))

# Select final columns only
output <- data %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Output file: data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")
cat("\nColumn names:\n")
print(names(output))