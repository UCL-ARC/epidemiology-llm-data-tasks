library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define files from metadata
files_to_load <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load files explicitly
data_w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
data_w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
data_w8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
data_w9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge datasets using full_join by NSID
full_df <- data_w1 %>%
  full_join(data_w4, by = 'NSID') %>%
  full_join(data_w8, by = 'NSID') %>%
  full_join(data_w9, by = 'NSID')

# 2. Target Variables Identification
# Construct: Income bands
# Source variables: W8DINCB (Wave 8), W9DINCB (Wave 9)
# Output variables: age-specific (e.g., income_31, income_32 - based on metadata descriptions
# Wave 8 is 2015 (Age 31 approx), Wave 9 is 2022 (Age 32 as per metadata label)
# However, requirements ask for 'age-specific variables'. 
# Looking at metadata: W8 is Wave 8, W9 is Age 32. 
# Let's use names like income_w8 and income_w9 or derived age suffixes if possible.
# Metadata for W9 explicitly says "Age 32". W8 is Wave Eight. 
# Let's use income_w8 and income_w9 to be safe, or follow the 'age' suffix rule.
# Let's check metadata descriptions: W9 is Age 32. W8 is Wave 8. 
# If W9 is 32, W8 (previous wave) is likely 31. 
# Let's use income_31 and income_32.

# Define the value labels for the 16 substantive bands
income_labels <- c(
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

# Harmonise missing values
# Metadata: -1.0 is 'Not applicable'. Standard missing -1 = Item not applicable.
# NA/Null should be -3.

process_income <- function(var) {
  # Convert to numeric
  res <- as.numeric(var)
  
  # Handle missing values based on metadata
  # -1.0 -> -1 (Not applicable)
  # NA -> -3 (Not asked/not interviewed)
  res <- case_when(
    res == -1.0 ~ -1,
    is.na(res) ~ -3,
    TRUE ~ res
  )
  
  # Create factor for substantive values
  # We only apply labels to the 1-16 range
  res_fact <- factor(res, 
                     levels = c(-9, -8, -7, -3, -2, -1, 1:16),
                     labels = c('Refusal', 'Don\'t know', 'Prefer not to say', 
                                'Not asked', 'Schedule not applicable', 'Not applicable', 
                                income_labels))
  return(res_fact)
}

# Since the requirement says "Adopt the 16 substantive band labels directly", 
# and "produce separate age-specific variables", 
# and "keep only final derived variables".

# Applying the processing
final_df <- full_df %>%
  mutate(
    income_31 = process_income(W8DINCB),
    income_32 = process_income(W9DINCB)
  ) %>%
  select(NSID, income_31, income_32)

# Save to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')
