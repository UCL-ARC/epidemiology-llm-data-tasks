library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load all input files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Select relevant columns and rename
wave1 <- wave1 %>% select(NSID)
wave4 <- wave4 %>% select(NSID, W4nsseccatYP) %>% rename(nssec17 = W4nsseccatYP)
wave5 <- wave5 %>% select(NSID, W5nsseccatYP) %>% rename(nssec18 = W5nsseccatYP)
wave6 <- wave6 %>% select(NSID, w6nsseccatYP) %>% rename(nssec19 = w6nsseccatYP)
wave7 <- wave7 %>% select(NSID, W7NSSECCat) %>% rename(nssec20 = W7NSSECCat)
wave8 <- wave8 %>% select(NSID, W8DNSSEC17, W8DACTIVITYC) %>% rename(nssec25 = W8DNSSEC17)
wave9 <- wave9 %>% select(NSID, W9NSSEC) %>% rename(nssec32 = W9NSSEC)

# Merge all datasets
full_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Process nssec25 (age 25) with special derivation
full_data <- full_data %>%
  mutate(
    nssec25 = ifelse(!is.na(W8DACTIVITYC) & W8DACTIVITYC == 5.0, 15, nssec25),
    nssec25 = case_when(
      is.na(nssec25) ~ -3,
      nssec25 == -9.0 ~ -9,
      nssec25 == -8.0 ~ -8,
      nssec25 == -1.0 ~ -1,
      TRUE ~ floor(nssec25)
    )
  )

# Process other nssec variables
full_data <- full_data %>%
  mutate(
    nssec17 = case_when(
      is.na(nssec17) ~ -3,
      nssec17 == -99.0 ~ -3,
      nssec17 == -91.0 ~ -1,
      TRUE ~ floor(nssec17)
    ),
    nssec18 = case_when(
      is.na(nssec18) ~ -3,
      nssec18 == -91.0 ~ -1,
      TRUE ~ floor(nssec18)
    ),
    nssec19 = case_when(
      is.na(nssec19) ~ -3,
      nssec19 == -91.0 ~ -1,
      TRUE ~ floor(nssec19)
    ),
    nssec20 = case_when(
      is.na(nssec20) ~ -3,
      nssec20 == -91.0 ~ -1,
      TRUE ~ floor(nssec20)
    ),
    nssec32 = case_when(
      is.na(nssec32) ~ -3,
      nssec32 == -1.0 ~ -1,
      nssec32 == -9.0 ~ -9,
      nssec32 == -8.0 ~ -8,
      TRUE ~ floor(nssec32)
    )
  )

# Define standard labels for NS-SEC categories (names are labels, values are codes)
standard_labels <- c(
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Prefer not to say" = -7,
  "Not asked at the fieldwork stage/participated/interviewed" = -3,
  "Schedule not applicable/Script error/information lost" = -2,
  "Item not applicable" = -1,
  "Employers in large organisations" = 1,
  "Higher managerial and administrative occupations" = 2,
  "Higher professional occupations" = 3,
  "Lower professional and higher technical occupations" = 4,
  "Lower managerial and administrative occupations" = 5,
  "Higher supervisory occupations" = 6,
  "Intermediate occupations" = 7,
  "Employers in small establishments" = 8,
  "Own account workers" = 9,
  "Lower supervisory occupations" = 10,
  "Lower technical occupations" = 11,
  "Semi-routine occupations" = 12,
  "Routine occupations" = 13,
  "Never worked and Long-term unemployed" = 14,
  "Full-time students" = 15,
  "Occupations not stated or inadequately described" = 16,
  "Not classifiable for other reasons" = 17
)

# Apply labels to all NS-SEC variables using haven::labelled
full_data <- full_data %>%
  mutate(across(starts_with("nssec"), ~ haven::labelled(.x, labels = standard_labels)))

# Select only required variables
final_data <- full_data %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output CSV
write_csv(final_data, "data/output/cleaned_data.csv")