library(readr)
library(dplyr)
library(labelled)

# Load all files from data/input/
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets using full_join by NSID
cleaned <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Create inc25 from W8DINCB (age 25, Wave 8)
# -1 = Not applicable, values 1-16 are substantive income bands
cleaned <- cleaned %>%
  mutate(
    inc25 = case_when(
      W8DINCB == -1 ~ -1,
      W8DINCB >= 1 & W8DINCB <= 16 ~ as.numeric(W8DINCB),
      TRUE ~ -3
    )
  )

# Create inc32 from W9DINCB (age 32, Wave 9)
cleaned <- cleaned %>%
  mutate(
    inc32 = case_when(
      W9DINCB == -1 ~ -1,
      W9DINCB >= 1 & W9DINCB <= 16 ~ as.numeric(W9DINCB),
      TRUE ~ -3
    )
  )

# Define labels for income bands - use character names that match numeric values
income_labels <- c(
  '-1' = 'Not applicable',
  '1' = 'less than 25',
  '2' = '25 to 50',
  '3' = '50 to 90',
  '4' = '90 to 140',
  '5' = '140 to 240',
  '6' = '240 to 300',
  '7' = '300 to 350',
  '8' = '350 to 400',
  '9' = '400 to 500',
  '10' = '500 to 600',
  '11' = '600 to 700',
  '12' = '700 to 800',
  '13' = '800 to 900',
  '14' = '900 to 1200',
  '15' = '1200 to 1400',
  '16' = 'more than 1400'
)

# Create final output with only ID and derived variables
output <- cleaned %>%
  select(NSID, inc25, inc32)

# Apply labels directly using attr()
attr(output$inc25, 'labels') <- income_labels
attr(output$inc25, 'label') <- 'Weekly income band at age 25'
attr(output$inc32, 'labels') <- income_labels
attr(output$inc32, 'label') <- 'Weekly income band at age 32'

# Write to CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Data cleaning complete. Output written to data/output/cleaned_data.csv\n')

# Show sample output
head(output)