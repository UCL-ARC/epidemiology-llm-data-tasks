library(haven)
library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Read all tab-delimited files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets using full_join by NSID
combined <- full_join(wave_one, wave_four, by = 'NSID')
combined <- full_join(combined, ns8, by = 'NSID')
combined <- full_join(combined, ns9, by = 'NSID')

# Rename income variables
combined <- combined %>%
  rename(inc25 = W8DINCB,
         inc32 = W9DINCB)

# Define factor levels and labels for income variables
inc_levels <- c('-9', '-8', '-1', '-3', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16')
inc_labels <- c('Refusal', 'Don\'t know/insufficient information', 'Not applicable', 'Not asked/interviewed',
                'less than 25', '25 to 50', '50 to 90', '90 to 140', '140 to 240', '240 to 300',
                '300 to 350', '350 to 400', '400 to 500', '500 to 600', '600 to 700',
                '700 to 800', '800 to 900', '900 to 1200', '1200 to 1400', 'more than 1400')

# Recode NA values to -3 and convert to factors with labels
combined <- combined %>%
  mutate(
    inc25 = ifelse(is.na(inc25), -3, inc25),
    inc32 = ifelse(is.na(inc32), -3, inc32),
    inc25 = factor(inc25, levels = inc_levels, labels = inc_labels),
    inc32 = factor(inc32, levels = inc_levels, labels = inc_labels)
  )

# Select final variables
final_data <- combined %>%
  select(NSID, inc25, inc32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print summary
cat('Dataset cleaned successfully!\n')
cat('Rows:', nrow(final_data), '\n')
cat('Columns:', ncol(final_data), '\n')
