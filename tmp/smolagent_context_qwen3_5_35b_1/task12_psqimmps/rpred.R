library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
w5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
w6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
w7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
w8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
w9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all files by NSID using full_join
df <- full_join(wave1, w4, by = 'NSID') %>%
  full_join(w5, by = 'NSID') %>%
  full_join(w6, by = 'NSID') %>%
  full_join(w7, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9, by = 'NSID')

# Function to extract integer part of NS-SEC code
extract_nssec_int <- function(code) {
  ifelse(is.na(code) | code %in% c(-9, -8, -7, -3, -2, -1),
         code,
         as.integer(as.character(code)))
}

# Define NS-SEC labels (standardized across waves)
nssec_labels <- c(
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

# Create NS-SEC variables for each wave using mutate
df <- df %>%
  mutate(
    # Age 17 (Wave 4)
    nssec17 = extract_nssec_int(W4nsseccatYP),
    # Age 18 (Wave 5)
    nssec18 = extract_nssec_int(W5nsseccatYP),
    # Age 19 (Wave 6)
    nssec19 = extract_nssec_int(w6nsseccatYP),
    # Age 20 (Wave 7)
    nssec20 = extract_nssec_int(W7NSSECCat),
    # Age 25 (Wave 8) - special rule: if W8DACTIVITYC == 5 (Full-time education), assign 15
    nssec25_raw = extract_nssec_int(W8DNSSEC17),
    # Age 32 (Wave 9)
    nssec32 = extract_nssec_int(W9NSSEC)
  )

# Apply special rule for nssec25: if in full-time education, assign 15
df <- df %>%
  mutate(
    nssec25 = ifelse(W8DACTIVITYC == 5, 15, nssec25_raw)
  )

# Create labels for missing codes
missing_labels <- c('Refusal', 'Insufficient information', 'Prefer not to say', 'Not asked', 'Schedule not applicable', 'Not applicable')

# Convert to factor with labels
df$nssec17 <- factor(df$nssec17, levels = c(1:17, -9, -8, -7, -3, -2, -1),
                     labels = c(nssec_labels, missing_labels))
df$nssec18 <- factor(df$nssec18, levels = c(1:17, -9, -8, -7, -3, -2, -1),
                     labels = c(nssec_labels, missing_labels))
df$nssec19 <- factor(df$nssec19, levels = c(1:17, -9, -8, -7, -3, -2, -1),
                     labels = c(nssec_labels, missing_labels))
df$nssec20 <- factor(df$nssec20, levels = c(1:17, -9, -8, -7, -3, -2, -1),
                     labels = c(nssec_labels, missing_labels))
df$nssec25 <- factor(df$nssec25, levels = c(1:17, -9, -8, -7, -3, -2, -1),
                     labels = c(nssec_labels, missing_labels))
df$nssec32 <- factor(df$nssec32, levels = c(1:17, -9, -8, -7, -3, -2, -1),
                     labels = c(nssec_labels, missing_labels))

# Keep only ID and final NS-SEC variables
output <- df %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write to CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Data cleaning complete. Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(output), '\n')