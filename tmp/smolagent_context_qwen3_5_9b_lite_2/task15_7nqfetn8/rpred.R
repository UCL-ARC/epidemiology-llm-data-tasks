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

# Merge all datasets by NSID
combined <- full_join(wave1, wave4, by = 'NSID')
combined <- full_join(combined, ns8, by = 'NSID')
combined <- full_join(combined, ns9, by = 'NSID')

# Create the output dataframe with only NSID and the two income variables
# First, extract the income variables
inc25_raw <- combined$W8DINCB
inc32_raw <- combined$W9DINCB

# Handle missing values
# -1.0 is 'Not applicable' → map to -1 (Item not applicable)
# R NA values → map to -3 (Not asked)
inc25_clean <- ifelse(inc25_raw == -1, -1, inc25_raw)
inc25_clean[is.na(inc25_clean)] <- -3

inc32_clean <- ifelse(inc32_raw == -1, -1, inc32_raw)
inc32_clean[is.na(inc32_clean)] <- -3

# Create output dataframe with only NSID and the two income variables
output <- data.frame(
  NSID = combined$NSID,
  inc25 = inc25_clean,
  inc32 = inc32_clean
)

print('Output structure:')
print(dim(output))
print('Column names:')
print(names(output))
print('inc25 unique values:')
print(unique(output$inc25))
print('inc32 unique values:')
print(unique(output$inc32))
print('Summary of inc25:')
print(table(output$inc25))
print('Summary of inc32:')
print(table(output$inc32))

# Write to CSV
write_csv(output, 'data/output/cleaned_data.csv')

print('Output file created successfully!')
print('File contains', nrow(output), 'rows and', ncol(output), 'columns.')
