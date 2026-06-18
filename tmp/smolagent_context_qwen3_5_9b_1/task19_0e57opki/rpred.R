library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Set working directory
setwd(getwd())

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets using full_join by NSID
combined <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Check if W8DBMI and W9DBMI exist
cat('W8DBMI exists:', 'W8DBMI' %in% names(combined), '\n')
cat('W9DBMI exists:', 'W9DBMI' %in% names(combined), '\n')

# Create bmi25 variable from W8DBMI (age 25)
# Standard missing value codes:
# -9 = Refusal
# -8 = Don't know / insufficient information  
# -7 = Prefer not to say
# -3 = Not asked at the fieldwork stage / not interviewed
# -2 = Schedule not applicable / script error / information lost
# -1 = Item not applicable

# W8DBMI has user_missing_values: '-9.0 thru -8.0 and -1.0'
# -9.0 = Refused -> -9
# -8.0 = Insufficient information -> -8
# -1.0 = Not applicable -> -1

bmi25 <- combined$W8DBMI

# Map missing values according to standard codes
bmi25[bmi25 == -9.0] <- -9
bmi25[bmi25 == -8.0] <- -8
bmi25[bmi25 == -1.0] <- -1

# Map NA to -3
bmi25[is.na(bmi25)] <- -3

# Create bmi32 variable from W9DBMI (age 32)
bmi32 <- combined$W9DBMI

# W9DBMI has user_missing_values: '-1.0 thru -8.0 and -9.0'
# -9.0 = Refused -> -9
# -8.0 = Insufficient information -> -8
# -1.0 = Not applicable -> -1

bmi32[bmi32 == -9.0] <- -9
bmi32[bmi32 == -8.0] <- -8
bmi32[bmi32 == -1.0] <- -1

# Map NA to -3
bmi32[is.na(bmi32)] <- -3

# Copy the original variables to the output dataset
combined$bmi25 <- bmi25
combined$bmi32 <- bmi32

# Check for negative values in the original data (shouldn't exist for BMI)
cat('Negative values in W8DBMI:', sum(combined$W8DBMI < 0), '\n')
cat('Negative values in W9DBMI:', sum(combined$W9DBMI < 0), '\n')

# Check for NA values
na_count_w8 <- sum(is.na(combined$W8DBMI))
na_count_w9 <- sum(is.na(combined$W9DBMI))
cat('NA count W8DBMI:', na_count_w8, '\n')
cat('NA count W9DBMI:', na_count_w9, '\n')

# Check the distribution of bmi25
unique_values <- unique(sort(bmi25))
cat('Unique values in bmi25 (first 10):', head(unique_values, 10), '\n')
cat('Unique values in bmi25 (last 10):', tail(unique_values, 10), '\n')

# Write output
cat('Writing cleaned_data.csv...\n')
write_csv(combined, 'data/output/cleaned_data.csv')

cat('Done!\n')