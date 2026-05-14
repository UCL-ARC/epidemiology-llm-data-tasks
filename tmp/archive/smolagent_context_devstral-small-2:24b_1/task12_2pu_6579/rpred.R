library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_derived.tab",
  wave_nine = "data/input/ns9_2022_main_interview.tab"
)

# Load each dataset
wave_one <- readr::read_delim(files$wave_one, delim = "\t")
wave_four <- readr::read_delim(files$wave_four, delim = "\t")
wave_five <- readr::read_delim(files$wave_five, delim = "\t")
wave_six <- readr::read_delim(files$wave_six, delim = "\t")
wave_seven <- readr::read_delim(files$wave_seven, delim = "\t")
wave_eight <- readr::read_delim(files$wave_eight, delim = "\t")
wave_nine <- readr::read_delim(files$wave_nine, delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Define missing value codes mapping
missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable"
)

# Function to harmonize missing codes
harmonize_missing <- function(var) {
  var <- ifelse(is.na(var), -3, var)
  var <- case_when(
    var == -999 ~ -9,
    var == -99 ~ -9,
    var == -91 ~ -1,
    var == -9 ~ -9,
    var == -8 ~ -8,
    var == -7 ~ -7,
    var == -3 ~ -3,
    var == -2 ~ -2,
    var == -1 ~ -1,
    TRUE ~ var
  )
  return(var)
}

# Process NS-SEC variables
merged_data <- merged_data %>%
  mutate(
    nssec17 = harmonize_missing(W5nsseccatYP),
    nssec18 = harmonize_missing(w6nsseccatYP),
    nssec19 = harmonize_missing(W7NSSECCat),
    nssec20 = harmonize_missing(W8DNSSEC17),
    nssec25 = harmonize_missing(W9NSSEC),
    nssec32 = harmonize_missing(W9NSSEC)
  )

# Collapse fractional codes to major categories
collapse_nssec <- function(var) {
  var <- ifelse(is.na(var), -3, var)
  var <- floor(var)
  var <- ifelse(var < 1 | var > 17, -3, var)
  return(var)
}

merged_data <- merged_data %>%
  mutate(
    nssec17 = collapse_nssec(nssec17),
    nssec18 = collapse_nssec(nssec18),
    nssec19 = collapse_nssec(nssec19),
    nssec20 = collapse_nssec(nssec20),
    nssec25 = collapse_nssec(nssec25),
    nssec32 = collapse_nssec(nssec32)
  )

# Special derivation for age 25 (full-time student category 15)
merged_data <- merged_data %>%
  mutate(
    nssec25 = ifelse(W8DACTIVITYC == 5 & nssec25 == -3, 15, nssec25)
  )

# Define labels for NS-SEC categories
nssec_labels <- c(
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
  "17" = "Not classifiable for other reasons",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable"
)

# Convert NS-SEC variables to labelled factors
merged_data <- merged_data %>%
  mutate(
    nssec17 = factor(nssec17, levels = names(nssec_labels), labels = nssec_labels),
    nssec18 = factor(nssec18, levels = names(nssec_labels), labels = nssec_labels),
    nssec19 = factor(nssec19, levels = names(nssec_labels), labels = nssec_labels),
    nssec20 = factor(nssec20, levels = names(nssec_labels), labels = nssec_labels),
    nssec25 = factor(nssec25, levels = names(nssec_labels), labels = nssec_labels),
    nssec32 = factor(nssec32, levels = names(nssec_labels), labels = nssec_labels)
  )

# Select only required variables
cleaned_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)