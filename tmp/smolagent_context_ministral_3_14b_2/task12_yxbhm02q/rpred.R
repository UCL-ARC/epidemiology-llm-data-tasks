
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Load available files
wave4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave5 <- readr::read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave6 <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- readr::read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')

# Load ns9 only if it exists and has data
ns9_exists <- file.exists('data/input/ns9_2022_main_interview.tab')
if (ns9_exists) {
  ns9 <- readr::read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')
  # Check if ns9 has data and contains NSID
  if (nrow(ns9) > 0 && 'NSID' %in% colnames(ns9)) {
    message('ns9 loaded successfully.')
  } else {
    message('ns9 is empty or does not contain NSID. Skipping ns9.')
    ns9 <- NULL
  }
} else {
  message('ns9_2022_main_interview.tab not found. Skipping ns9.')
  ns9 <- NULL
}

# Define a function to collapse fractional NS-SEC codes to major categories
collapse_nssec <- function(x) {
  if (!is.numeric(x)) return(x)
  as.integer(floor(x))
}

# Define a function to handle missing values based on metadata
handle_missing <- function(x, wave) {
  if (!is.numeric(x)) return(x)
  if (wave == 'ns8') {
    x[x == -9] <- -9  # Refusal
    x[x == -8] <- -8  # Insufficient information
    x[x == -1] <- -1  # Not applicable
  } else {
    x[x <= -1] <- -3  # Convert all negative codes to -3 unless specified otherwise
  }
  return(x)
}

# Define a function to handle the special case for nssec25 (wave 8)
handle_nssec25 <- function(nssec, activity) {
  if (!is.numeric(nssec) | !is.numeric(activity)) return(nssec)

  # If activity is full-time education (5.0), assign 15 (Full-time students)
  full_time_student <- (activity == 5.0)
  nssec[full_time_student & (is.na(nssec) | nssec == -3)] <- 15

  return(nssec)
}

# Merge datasets by NSID, preserving all rows
merged_data <- full_join(wave4, wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(ns8, by = 'NSID')

# Only include ns9 if it exists and has valid data
if (!is.null(ns9)) {
  merged_data <- full_join(merged_data, ns9, by = 'NSID')
}

# Derive nssec17 from wave4
merged_data <- merged_data %>%
  mutate(nssec17 = collapse_nssec(W4nsseccatYP)) %>%
  mutate(nssec17 = handle_missing(nssec17, 'wave4'))

# Derive nssec18 from wave5
merged_data <- merged_data %>%
  mutate(nssec18 = collapse_nssec(W5nsseccatYP)) %>%
  mutate(nssec18 = handle_missing(nssec18, 'wave5'))

# Derive nssec19 from wave6
merged_data <- merged_data %>%
  mutate(nssec19 = collapse_nssec(w6nsseccatYP)) %>%
  mutate(nssec19 = handle_missing(nssec19, 'wave6'))

# Derive nssec20 from wave7
merged_data <- merged_data %>%
  mutate(nssec20 = collapse_nssec(W7NSSECCat)) %>%
  mutate(nssec20 = handle_missing(nssec20, 'wave7'))

# Derive nssec25 from ns8, applying special logic for full-time education
merged_data <- merged_data %>%
  mutate(nssec25 = collapse_nssec(W8DNSSEC17)) %>%
  mutate(nssec25 = handle_missing(nssec25, 'ns8')) %>%
  mutate(nssec25 = handle_nssec25(nssec25, W8DACTIVITYC))

# Derive nssec32 from ns9 only if ns9 exists and has valid data
if (!is.null(ns9)) {
  merged_data <- merged_data %>%
    mutate(nssec32 = collapse_nssec(W9NSSEC)) %>%
    mutate(nssec32 = handle_missing(nssec32, 'ns9'))
}

# Define labels for the NS-SEC categories
nssec_labels <- c(
  '-9' = 'Refusal',
  '-8' = 'Insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not interviewed',
  '-2' = 'Schedule not applicable',
  '-1' = 'Not applicable',
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial and administrative occupations',
  '3' = 'Higher professional occupations',
  '4' = 'Lower professional and higher technical occupations',
  '5' = 'Lower managerial and administrative occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate occupations',
  '8' = 'Employers in small establishments',
  '9' = 'Own account workers',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical occupations',
  '12' = 'Semi-routine occupations',
  '13' = 'Routine occupations',
  '14' = 'Never worked and Long-term unemployed',
  '15' = 'Full-time students',
  '16' = 'Occupations not stated or inadequately described',
  '17' = 'Not classifiable for other reasons'
)

# Convert nssec variables to labelled factors
nssec_vars <- c('nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25')
if (!is.null(ns9)) {
  nssec_vars <- c(nssec_vars, 'nssec32')
}

for (var in nssec_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]],
                               levels = names(nssec_labels),
                               labels = nssec_labels)
  }
}

# Keep only NSID and the derived nssec variables
final_data <- merged_data %>%
  select(NSID, all_of(nssec_vars))

# Write the final output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')
