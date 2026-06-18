library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Define file paths
files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave5 = "data/input/wave_five_lsype_young_person_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_derived.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave1 <- read_delim(files$wave1, delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files$wave4, delim = "\t", show_col_types = FALSE)
wave5 <- read_delim(files$wave5, delim = "\t", show_col_types = FALSE)
wave6 <- read_delim(files$wave6, delim = "\t", show_col_types = FALSE)
wave7 <- read_delim(files$wave7, delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(files$wave8, delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(files$wave9, delim = "\t", show_col_types = FALSE)

# Function to collapse fractional NS-SEC codes to major categories
collapse_nssec <- function(x) {
  ifelse(is.na(x), NA_real_, floor(x))
}

# Function to convert negative codes to standard missing values
code_to_missing <- function(x, na_codes) {
  # Map specific negative codes to standard missing values
  # Based on label meanings from metadata
  x[is.element(x, na_codes$refusal)] <- -9
  x[is.element(x, na_codes$dont_know)] <- -8
  x[is.element(x, na_codes$prefer_not_say)] <- -7
  x[is.element(x, na_codes$not_asked)] <- -3
  x[is.element(x, na_codes$not_applicable)] <- -1
  x[is.element(x, na_codes$not_interviewed)] <- -3
  return(x)
}

# Wave 4 (Age 17): -99 = YP Not interviewed, -91 = Not applicable
wave4 <- wave4 %>%
  mutate(raw_nssec = W4nsseccatYP) %>%
  mutate(nssec_temp = raw_nssec) %>%
  mutate(nssec_temp = ifelse(nssec_temp == -99, -3, nssec_temp)) %>%  # YP Not interviewed
  mutate(nssec_temp = ifelse(nssec_temp == -91, -1, nssec_temp)) %>%  # Not applicable
  mutate(nssec17 = collapse_nssec(nssec_temp))

# Wave 5 (Age 18): -91 = Not applicable
wave5 <- wave5 %>%
  mutate(raw_nssec = W5nsseccatYP) %>%
  mutate(nssec_temp = raw_nssec) %>%
  mutate(nssec_temp = ifelse(nssec_temp == -91, -1, nssec_temp)) %>%  # Not applicable
  mutate(nssec18 = collapse_nssec(nssec_temp))

# Wave 6 (Age 19): -91 = Not applicable
wave6 <- wave6 %>%
  mutate(raw_nssec = w6nsseccatYP) %>%
  mutate(nssec_temp = raw_nssec) %>%
  mutate(nssec_temp = ifelse(nssec_temp == -91, -1, nssec_temp)) %>%  # Not applicable
  mutate(nssec19 = collapse_nssec(nssec_temp))

# Wave 7 (Age 20): -91 = Not applicable
wave7 <- wave7 %>%
  mutate(raw_nssec = W7NSSECCat) %>%
  mutate(nssec_temp = raw_nssec) %>%
  mutate(nssec_temp = ifelse(nssec_temp == -91, -1, nssec_temp)) %>%  # Not applicable
  mutate(nssec20 = collapse_nssec(nssec_temp))

# Wave 8 (Age 25): -9 = Refused, -8 = Insufficient information, -1 = Not applicable
wave8 <- wave8 %>%
  mutate(raw_nssec = W8DNSSEC17) %>%
  mutate(nssec_temp = raw_nssec) %>%
  mutate(nssec_temp = ifelse(nssec_temp == -9, -9, nssec_temp)) %>%  # Refused
  mutate(nssec_temp = ifelse(nssec_temp == -8, -8, nssec_temp)) %>%  # Insufficient information
  mutate(nssec_temp = ifelse(nssec_temp == -1, -1, nssec_temp)) %>%  # Not applicable
  mutate(nssec25 = collapse_nssec(nssec_temp)) %>%
  # Special rule: if W8DACTIVITYC == 5 (Education: School/college/university), set nssec25 = 15
  mutate(nssec25 = ifelse(W8DACTIVITYC == 5, 15, nssec25))

# Wave 9 (Age 32): -1 = Not Applicable
wave9 <- wave9 %>%
  mutate(raw_nssec = W9NSSEC) %>%
  mutate(nssec_temp = raw_nssec) %>%
  mutate(nssec_temp = ifelse(nssec_temp == -1, -1, nssec_temp)) %>%  # Not applicable
  mutate(nssec32 = collapse_nssec(nssec_temp))

# Merge all waves by NSID
merged <- wave1 %>%
  full_join(wave4 %>% select(NSID, nssec17), by = "NSID") %>%
  full_join(wave5 %>% select(NSID, nssec18), by = "NSID") %>%
  full_join(wave6 %>% select(NSID, nssec19), by = "NSID") %>%
  full_join(wave7 %>% select(NSID, nssec20), by = "NSID") %>%
  full_join(wave8 %>% select(NSID, nssec25), by = "NSID") %>%
  full_join(wave9 %>% select(NSID, nssec32), by = "NSID")

# Convert remaining NA to -3 (Not asked/Not interviewed)
merged <- merged %>%
  mutate(
    nssec17 = ifelse(is.na(nssec17), -3, nssec17),
    nssec18 = ifelse(is.na(nssec18), -3, nssec18),
    nssec19 = ifelse(is.na(nssec19), -3, nssec19),
    nssec20 = ifelse(is.na(nssec20), -3, nssec20),
    nssec25 = ifelse(is.na(nssec25), -3, nssec25),
    nssec32 = ifelse(is.na(nssec32), -3, nssec32)
  )

# Define labels for the 17 major NS-SEC categories
label_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "-3")
label_values <- c(
  "Employers in large organisations",
  "Higher managerial and administrative occupations",
  "Higher professional occupations",
  "Lower professional and higher technical occupations",
  "Lower managerial and administrative occupations",
  "Higher supervisory occupations",
  "Intermediate occupations",
  "Employers in small establishments",
  "Own account workers",
  "Lower supervisory occupations",
  "Lower technical occupations",
  "Semi-routine occupations",
  "Routine occupations",
  "Never worked and Long-term unemployed",
  "Full-time students",
  "Occupations not stated or inadequately described",
  "Not classifiable for other reasons",
  "Not asked at the fieldwork stage / not interviewed"
)

# Create a named numeric vector for labels
nssec_labels <- setNames(as.numeric(label_values), label_names)

# Apply labels to each variable
merged$nssec17 <- haven::labelled(merged$nssec17, labels = nssec_labels)
merged$nssec18 <- haven::labelled(merged$nssec18, labels = nssec_labels)
merged$nssec19 <- haven::labelled(merged$nssec19, labels = nssec_labels)
merged$nssec20 <- haven::labelled(merged$nssec20, labels = nssec_labels)
merged$nssec25 <- haven::labelled(merged$nssec25, labels = nssec_labels)
merged$nssec32 <- haven::labelled(merged$nssec32, labels = nssec_labels)

# Select only ID and final derived variables
output <- merged %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Check dimensions
print(paste("Output dimensions:", nrow(output), "rows,", ncol(output), "cols"))

# Write output
write_csv(output, "data/output/cleaned_data.csv")
print("Output written to data/output/cleaned_data.csv")

# Show some summary
print(summary(output))