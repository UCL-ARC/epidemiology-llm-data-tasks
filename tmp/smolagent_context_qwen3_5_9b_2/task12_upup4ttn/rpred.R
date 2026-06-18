library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Start with wave1 as base (contains only NSID)
cohort <- wave1

# Wave 4 (age 17) - W4nsseccatYP
cohort <- cohort %>%
  full_join(wave4, by = 'NSID')

# Handle missing values for nssec17
cohort$nssec17 <- cohort$W4nsseccatYP
cohort$nssec17[is.na(cohort$W4nsseccatYP) | cohort$W4nsseccatYP <= -999 | cohort$W4nsseccatYP <= -91 | cohort$W4nsseccatYP <= -99 | cohort$W4nsseccatYP <= -1] <- -3

# Wave 5 (age 18) - W5nsseccatYP
cohort <- cohort %>%
  full_join(wave5, by = 'NSID')

cohort$nssec18 <- cohort$W5nsseccatYP
cohort$nssec18[is.na(cohort$W5nsseccatYP) | cohort$W5nsseccatYP <= -999 | cohort$W5nsseccatYP <= -91 | cohort$W5nsseccatYP <= -99 | cohort$W5nsseccatYP <= -1] <- -3

# Wave 6 (age 19) - w6nsseccatYP
cohort <- cohort %>%
  full_join(wave6, by = 'NSID')

cohort$nssec19 <- cohort$w6nsseccatYP
cohort$nssec19[is.na(cohort$w6nsseccatYP) | cohort$w6nsseccatYP <= -999 | cohort$w6nsseccatYP <= -91 | cohort$w6nsseccatYP <= -99 | cohort$w6nsseccatYP <= -1] <- -3

# Wave 7 (age 20) - W7NSSECCat
cohort <- cohort %>%
  full_join(wave7, by = 'NSID')

cohort$nssec20 <- cohort$W7NSSECCat
cohort$nssec20[is.na(cohort$W7NSSECCat) | cohort$W7NSSECCat <= -999 | cohort$W7NSSECCat <= -91 | cohort$W7NSSECCat <= -99 | cohort$W7NSSECCat <= -1] <- -3

# Wave 8 (age 25) - W8DNSSEC17 with special logic for full-time students
cohort <- cohort %>%
  full_join(wave8, by = 'NSID')

# Handle missing values and apply special logic for nssec25
cohort$nssec25 <- cohort$W8DNSSEC17
cohort$nssec25[is.na(cohort$W8DNSSEC17) | cohort$W8DNSSEC17 <= -9 | cohort$W8DNSSEC17 <= -8 | cohort$W8DNSSEC17 <= -1] <- NA
cohort$nssec25[cohort$W8DACTIVITYC == 5.0] <- 15.0
cohort$nssec25[is.na(cohort$W8DNSSEC17)] <- -3

# Wave 9 (age 32) - W9NSSEC
cohort <- cohort %>%
  full_join(wave9, by = 'NSID')

cohort$nssec32 <- cohort$W9NSSEC
cohort$nssec32[is.na(cohort$W9NSSEC) | cohort$W9NSSEC <= -9 | cohort$W9NSSEC <= -1] <- -3

# Select only the final variables
cohort <- cohort %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# NS-SEC labels (17 categories)
nssec_labels <- c(
  'Employers in large organisations',
  'Higher managerial and administrative occupations',
  'Higher professional occupations',
  'Lower professional and higher technical occupations',
  'Lower managerial and administrative occupations',
  'Higher supervisory occupations',
  'Intermediate occupations',
  'Employers in small establishments',
  'Own account workers',
  'Lower supervisory occupations',
  'Lower technical occupations',
  'Semi-routine occupations',
  'Routine occupations',
  'Never worked and Long-term unemployed',
  'Full-time students',
  'Occupations not stated or inadequately described',
  'Not classifiable for other reasons'
)

# Create labelled factors with correct levels
# Levels: -9 (Refusal), -8 (Don't know), -7 (Prefer not to say), -3 (Not asked), -2 (Schedule error), -1 (Not applicable), 1:17 (NS-SEC categories)
# But based on metadata, we use standard codes: -9, -8, -7, -3, -2, -1, and 1-17
# Actually for NS-SEC we just need: -9 to -1 as missing, and 1-17 as valid
# Let's use: -9 (Refusal), -3 (Not asked), -1 (Not applicable) and 1-17

# Factor levels for each variable: -9, -8, -7, -3, -2, -1, 1, 2, ..., 17
factor_levels <- c(-9, -8, -7, -3, -2, -1, 1:17)

# Missing value labels
missing_labels <- c(
  'Refusal',           # -9
  'Don\'t know',       # -8
  'Prefer not to say', # -7
  'Not asked',         # -3
  'Schedule not applicable', # -2
  'Not applicable'     # -1
)

# Valid category labels
valid_labels <- nssec_labels

# Full labels vector
full_labels <- c(missing_labels, valid_labels)

# Create labelled factors
cohort$nssec17 <- factor(cohort$nssec17, levels = factor_levels, labels = full_labels)
cohort$nssec18 <- factor(cohort$nssec18, levels = factor_levels, labels = full_labels)
cohort$nssec19 <- factor(cohort$nssec19, levels = factor_levels, labels = full_labels)
cohort$nssec20 <- factor(cohort$nssec20, levels = factor_levels, labels = full_labels)
cohort$nssec25 <- factor(cohort$nssec25, levels = factor_levels, labels = full_labels)
cohort$nssec32 <- factor(cohort$nssec32, levels = factor_levels, labels = full_labels)

# Keep only final variables
cohort <- cohort %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write_csv(cohort, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Cleaned data written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(cohort), '\n')
cat('Number of columns:', ncol(cohort), '\n')
