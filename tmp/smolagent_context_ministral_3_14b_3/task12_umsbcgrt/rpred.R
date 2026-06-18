
library(readr)
library(dplyr)
library(tidyr)

# Load datasets for waves with available data
wave_one <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_five <- readr::read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave_six <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- readr::read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave_eight <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')

# Define NS-SEC labels
nssec_labels <- c(
  '-3' = 'Missing',
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

# Function to collapse fractional NS-SEC codes
collapse_nssec <- function(x) {
  as.numeric(floor(as.numeric(as.character(x))))
}

# Function to standardize missing values
standardize_missing <- function(x) {
  x <- as.numeric(as.character(x))
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x
}

# Process each wave and create NS-SEC variables

# Wave 4 (Age 17)
wave_four_nssec <- wave_four %>%
  mutate(nssec17 = collapse_nssec(W4nsseccatYP)) %>%
  mutate(nssec17 = standardize_missing(nssec17)) %>%
  mutate(nssec17 = factor(nssec17, levels = names(nssec_labels), labels = nssec_labels)) %>%
  select(NSID, nssec17)

# Wave 5 (Age 18)
wave_five_nssec <- wave_five %>%
  mutate(nssec18 = collapse_nssec(W5nsseccatYP)) %>%
  mutate(nssec18 = standardize_missing(nssec18)) %>%
  mutate(nssec18 = factor(nssec18, levels = names(nssec_labels), labels = nssec_labels)) %>%
  select(NSID, nssec18)

# Wave 6 (Age 19)
wave_six_nssec <- wave_six %>%
  mutate(nssec19 = collapse_nssec(w6nsseccatYP)) %>%
  mutate(nssec19 = standardize_missing(nssec19)) %>%
  mutate(nssec19 = factor(nssec19, levels = names(nssec_labels), labels = nssec_labels)) %>%
  select(NSID, nssec19)

# Wave 7 (Age 20)
wave_seven_nssec <- wave_seven %>%
  mutate(nssec20 = collapse_nssec(W7NSSECCat)) %>%
  mutate(nssec20 = standardize_missing(nssec20)) %>%
  mutate(nssec20 = factor(nssec20, levels = names(nssec_labels), labels = nssec_labels)) %>%
  select(NSID, nssec20)

# Wave 8 (Age 25) with special handling for full-time students
wave_eight_nssec <- wave_eight %>%
  mutate(nssec25 = collapse_nssec(W8DNSSEC17)) %>%
  mutate(nssec25 = ifelse(W8DACTIVITYC == 5, 15, nssec25)) %>%
  mutate(nssec25 = standardize_missing(nssec25)) %>%
  mutate(nssec25 = factor(nssec25, levels = names(nssec_labels), labels = nssec_labels)) %>%
  select(NSID, nssec25)

# Create an empty dataset for nssec32 since wave_nine is unavailable
wave_nine_nssec <- tibble(NSID = character(0), nssec32 = factor(-3, levels = names(nssec_labels), labels = nssec_labels))

# Merge all datasets by NSID
merged_data <- full_join(wave_one, wave_four_nssec, by = 'NSID') %>%
  full_join(., wave_five_nssec, by = 'NSID') %>%
  full_join(., wave_six_nssec, by = 'NSID') %>%
  full_join(., wave_seven_nssec, by = 'NSID') %>%
  full_join(., wave_eight_nssec, by = 'NSID') %>%
  full_join(., wave_nine_nssec, by = 'NSID')

# Write the output to CSV
readr::write_csv(merged_data, 'data/output/cleaned_data.csv')
