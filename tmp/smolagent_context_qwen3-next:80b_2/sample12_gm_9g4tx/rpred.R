library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- reduce(list(wave1, wave4, wave5, wave6, wave7, wave8, wave9), full_join, by = "NSID")

# Process nssec17 (Age 17)
merged_data <- merged_data %>%
  mutate(nssec17 = W4nsseccatYP) %>%
  mutate(nssec17 = case_when(
    nssec17 == -99.0 ~ -3,
    nssec17 == -91.0 ~ -1,
    TRUE ~ nssec17
  )) %>%
  mutate(nssec17 = ifelse(is.na(nssec17), -3, nssec17)) %>%
  mutate(nssec17 = floor(nssec17)) %>%
  mutate(nssec17 = case_when(
    nssec17 >= 1 & nssec17 <= 17 ~ nssec17,
    TRUE ~ -2
  ))

# Process nssec18 (Age 18)
merged_data <- merged_data %>%
  mutate(nssec18 = W5nsseccatYP) %>%
  mutate(nssec18 = case_when(
    nssec18 == -91.0 ~ -1,
    TRUE ~ nssec18
  )) %>%
  mutate(nssec18 = ifelse(is.na(nssec18), -3, nssec18)) %>%
  mutate(nssec18 = floor(nssec18)) %>%
  mutate(nssec18 = case_when(
    nssec18 >= 1 & nssec18 <= 17 ~ nssec18,
    TRUE ~ -2
  ))

# Process nssec19 (Age 19)
merged_data <- merged_data %>%
  mutate(nssec19 = w6nsseccatYP) %>%
  mutate(nssec19 = case_when(
    nssec19 == -91.0 ~ -1,
    TRUE ~ nssec19
  )) %>%
  mutate(nssec19 = ifelse(is.na(nssec19), -3, nssec19)) %>%
  mutate(nssec19 = floor(nssec19)) %>%
  mutate(nssec19 = case_when(
    nssec19 >= 1 & nssec19 <= 17 ~ nssec19,
    TRUE ~ -2
  ))

# Process nssec20 (Age 20)
merged_data <- merged_data %>%
  mutate(nssec20 = W7NSSECCat) %>%
  mutate(nssec20 = case_when(
    nssec20 == -91.0 ~ -1,
    TRUE ~ nssec20
  )) %>%
  mutate(nssec20 = ifelse(is.na(nssec20), -3, nssec20)) %>%
  mutate(nssec20 = floor(nssec20)) %>%
  mutate(nssec20 = case_when(
    nssec20 >= 1 & nssec20 <= 17 ~ nssec20,
    TRUE ~ -2
  ))

# Process nssec25 (Age 25)
merged_data <- merged_data %>%
  mutate(nssec25 = W8DNSSEC17) %>%
  mutate(nssec25 = case_when(
    nssec25 == -9.0 ~ -9,
    nssec25 == -8.0 ~ -8,
    nssec25 == -1.0 ~ -1,
    TRUE ~ nssec25
  )) %>%
  mutate(nssec25 = ifelse(is.na(nssec25), -3, nssec25)) %>%
  # Process W8DACTIVITYC for derivation
  mutate(W8DACTIVITYC = case_when(
    W8DACTIVITYC == -9.0 ~ -9,
    W8DACTIVITYC == -8.0 ~ -8,
    W8DACTIVITYC == -1.0 ~ -1,
    TRUE ~ W8DACTIVITYC
  )) %>%
  mutate(W8DACTIVITYC = ifelse(is.na(W8DACTIVITYC), -3, W8DACTIVITYC)) %>%
  # Derive full-time student category
  mutate(nssec25 = ifelse(W8DACTIVITYC == 5.0, 15, nssec25)) %>%
  mutate(nssec25 = floor(nssec25)) %>%
  mutate(nssec25 = case_when(
    nssec25 >= 1 & nssec25 <= 17 ~ nssec25,
    TRUE ~ -2
  ))

# Process nssec32 (Age 32)
merged_data <- merged_data %>%
  mutate(nssec32 = W9NSSEC) %>%
  mutate(nssec32 = case_when(
    nssec32 == -1.0 ~ -1,
    TRUE ~ nssec32
  )) %>%
  mutate(nssec32 = ifelse(is.na(nssec32), -3, nssec32)) %>%
  mutate(nssec32 = floor(nssec32)) %>%
  mutate(nssec32 = case_when(
    nssec32 >= 1 & nssec32 <= 17 ~ nssec32,
    TRUE ~ -2
  ))

# Define labels for all NS-SEC categories
nssec_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "1" = "Employers in large organisations",
  "2" = "Higher managerial and administrative occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional and higher technical occupations",
  "5" = "Lower managerial and administrative occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small establishments",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Never worked and Long-term unemployed",
  "15" = "Full-time students",
  "16" = "Occupations not stated or inadequately described",
  "17" = "Not classifiable for other reasons"
)

# Convert all NS-SEC variables to factors with labels
merged_data <- merged_data %>%
  mutate(across(starts_with("nssec"), ~ factor(., levels = as.numeric(names(nssec_labels)), labels = nssec_labels)))

# Select only required columns
final_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output CSV
write_csv(final_data, "data/output/cleaned_data.csv")