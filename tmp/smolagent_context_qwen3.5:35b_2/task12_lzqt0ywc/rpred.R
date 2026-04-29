library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/wave_five_lsype_young_person_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave_one <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
wave_four <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
wave_five <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
wave_six <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
wave_seven <- read_delim(files[5], delim = "\t", show_col_types = FALSE)
ns8 <- read_delim(files[6], delim = "\t", show_col_types = FALSE)
ns9 <- read_delim(files[7], delim = "\t", show_col_types = FALSE)

# Merge all datasets
merged <- full_join(wave_one, wave_four, by = "NSID")
merged <- full_join(merged, wave_five, by = "NSID")
merged <- full_join(merged, wave_six, by = "NSID")
merged <- full_join(merged, wave_seven, by = "NSID")
merged <- full_join(merged, ns8, by = "NSID")
merged <- full_join(merged, ns9, by = "NSID")

# Function to map wave-specific missing codes to standard scheme
map_missing_codes <- function(raw_var, wave) {
  if (wave == 4) {
    raw_var <- ifelse(raw_var == -99, -3, raw_var)
    raw_var <- ifelse(raw_var == -91, -2, raw_var)
    raw_var <- ifelse(raw_var == -9, -9, raw_var)
    raw_var <- ifelse(raw_var == -8, -8, raw_var)
    raw_var <- ifelse(raw_var == -1, -1, raw_var)
  } else if (wave %in% c(5, 6, 7)) {
    raw_var <- ifelse(raw_var == -99, -3, raw_var)
    raw_var <- ifelse(raw_var == -91, -2, raw_var)
    raw_var <- ifelse(raw_var == -9, -9, raw_var)
    raw_var <- ifelse(raw_var == -8, -8, raw_var)
    raw_var <- ifelse(raw_var == -1, -1, raw_var)
  } else if (wave %in% c(8, 9)) {
    raw_var <- ifelse(raw_var == -9, -9, raw_var)
    raw_var <- ifelse(raw_var == -8, -8, raw_var)
    raw_var <- ifelse(raw_var == -1, -1, raw_var)
  }
  return(raw_var)
}

# Function to process NS-SEC variables
process_nssec <- function(raw_var, age) {
  # Map missing codes
  mapped <- map_missing_codes(raw_var, age)
  
  # Collapse fractional codes to major categories using floor()
  collapsed <- floor(mapped)
  
  # Convert NA to -3 (not asked at fieldwork stage)
  collapsed[is.na(collapsed)] <- -3
  
  var_label <- paste0("NS-SEC at age ", age)
  
  # Create labels as named character vector
  labels <- c(
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
    "-1" = "Item not applicable",
    "-2" = "Schedule not applicable/Script error/information lost",
    "-3" = "Not asked at the fieldwork stage/participated/interviewed",
    "-7" = "Prefer not to say",
    "-8" = "Don\'t know/insufficient information",
    "-9" = "Refusal"
  )
  
  # Use labelled::labelled with character data
  collapsed_char <- as.character(collapsed)
  labelled_var <- labelled::labelled(collapsed_char, labels)
  labelled_var <- labelled::set_variable_labels(labelled_var, var_label)
  
  return(labelled_var)
}

# Process each age-specific NS-SEC variable
merged$nssec17 <- process_nssec(merged$W4nsseccatYP, 17)
merged$nssec18 <- process_nssec(merged$W5nsseccatYP, 18)
merged$nssec19 <- process_nssec(merged$w6nsseccatYP, 19)
merged$nssec20 <- process_nssec(merged$W7NSSECCat, 20)

# Special derivation for age 25: derive full-time student category (15)
# using concurrent economic activity information
labels <- c(
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
  "-1" = "Item not applicable",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-7" = "Prefer not to say",
  "-8" = "Don\'t know/insufficient information",
  "-9" = "Refusal"
)

# Process W8DNSSEC17 first
nssec25_raw <- floor(map_missing_codes(merged$W8DNSSEC17, 8))
nssec25_raw[is.na(nssec25_raw)] <- -3
nssec25_raw <- as.character(nssec25_raw)
nssec25_raw <- labelled::labelled(nssec25_raw, labels)
nssec25_raw <- labelled::set_variable_labels(nssec25_raw, "NS-SEC at age 25")

# Override with full-time student category (15) where W8DACTIVITYC == 5
nssec25_raw <- ifelse(merged$W8DACTIVITYC == 5, "15", nssec25_raw)
nssec25_raw <- labelled::labelled(nssec25_raw, labels)
nssec25_raw <- labelled::set_variable_labels(nssec25_raw, "NS-SEC at age 25")
merged$nssec25 <- nssec25_raw

merged$nssec32 <- process_nssec(merged$W9NSSEC, 32)

# Select only required columns
output <- merged %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Create output directory if it doesn\'t exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write output CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Output file created successfully.\n")
cat("Number of observations:", nrow(output), "\n")
cat("Variables:", names(output), "\n")