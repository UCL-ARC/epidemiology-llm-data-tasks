library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
file_list <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

load_data <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = readr::col_character()))
}

data1 <- load_data('wave_one_lsype_family_background_2020.tab')
data2 <- load_data('wave_two_lsype_family_background_2020.tab')
data3 <- load_data('wave_three_lsype_family_background_2020.tab')
data4 <- load_data('wave_four_lsype_family_background_2020.tab')

# Merge datasets
merged_data <- data1 %>%
  full_join(data2, by = 'NSID') %>%
  full_join(data3, by = 'NSID') %>%
  full_join(data4, by = 'NSID')

# Helper function for cleaning missing values
# Standard codes:
# -9 = Refusal, -8 = DK, -7 = Prefer not to say, -3 = Not asked, -2 = Schedule not app, -1 = Item not app
clean_missing <- function(x) {
  # Specific task requirement: -99, -98, -996 map to -3
  x <- case_when(
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -996 ~ -3,
    x == -999 ~ -2, # Missing household info - lost
    x == -94 ~ -8,  # Insufficient information
    x == -92 ~ -9,  # Refusal
    TRUE ~ x
  )
  # Convert NA to -3
  x[is.na(x)] <- -3
  return(x)
}

# Process variables for each wave
# Wave 1 (Age 14)
merged_data <- merged_data %>%
  mutate(
    mum14 = clean_missing(W1empsmum),
    dad14 = clean_missing(W1empsdad)
  )

# Wave 2 (Age 15)
merged_data <- merged_data %>%
  mutate(
    mum15 = clean_missing(W2empsmum),
    dad15 = clean_missing(W2empsdad)
  )

# Wave 3 (Age 16)
merged_data <- merged_data %>%
  mutate(
    mum16 = clean_missing(W3empsmum),
    dad16 = clean_missing(W3empsdad)
  )

# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(
    mum17 = clean_missing(w4empsmum),
    dad17 = clean_missing(w4empsdad)
  )

# Define common labels for the 9-category scheme
cat_labels <- c(
  '1' = 'Doing paid work for 30 or more hours a week',
  '2' = 'Doing paid work for fewer than 30 hours a week',
  '3' = 'Unemployed/ Looking for a job',
  '4' = 'On a training course or scheme',
  '5' = 'In full-time education/ at school',
  '6' = 'Looking after the family/ household',
  '7' = 'Retired from work altogether',
  '8' = 'Sick/ disabled',
  '9' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Apply factors to derived variables
final_vars <- c('mum14', 'dad14', 'mum15', 'dad15', 'mum16', 'dad16', 'mum17', 'dad17')

merged_data <- merged_data %>%
  mutate(across(all_of(final_vars), ~ factor(.x, levels = as.numeric(names(cat_labels)), labels = cat_labels)))

# Final selection
output_data <- merged_data %>%
  select(NSID, all_of(final_vars))

write_csv(output_data, 'data/output/cleaned_data.csv')