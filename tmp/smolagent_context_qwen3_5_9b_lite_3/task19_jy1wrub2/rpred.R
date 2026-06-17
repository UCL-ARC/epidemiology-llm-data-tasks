library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets (keeping full cohort)
merged <- full_join(wave1, wave4, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Create bmi25 from W8DBMI (wave8 = age 25)
merged <- merged %>% mutate(
  bmi25 = case_when(
    W8DBMI == -9 ~ -9,
    W8DBMI == -8 ~ -8,
    W8DBMI == -1 ~ -1,
    TRUE ~ as.numeric(W8DBMI)
  )
)

# Create bmi32 from W9DBMI (wave9 = age 32)
merged <- merged %>% mutate(
  bmi32 = case_when(
    W9DBMI == -9 ~ -9,
    W9DBMI == -8 ~ -8,
    W9DBMI == -1 ~ -1,
    TRUE ~ as.numeric(W9DBMI)
  )
)

# Create final output with only NSID and derived variables
output <- merged %>%
  select(NSID, all_of(c('bmi25', 'bmi32')))

# Write to CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Script completed successfully\n')