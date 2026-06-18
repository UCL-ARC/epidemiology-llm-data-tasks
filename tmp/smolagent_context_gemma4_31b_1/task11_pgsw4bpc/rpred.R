library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define files and paths
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

# Load datasets
load_data <- function(file) {
  read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols())
}

data1 <- load_data(files[1])
data2 <- load_data(files[2])
data3 <- load_data(files[3])
data4 <- load_data(files[4])

# Merge datasets
full_frame <- data1 %>%
  full_join(data2, by = 'NSID') %>%
  full_join(data3, by = 'NSID') %>%
  full_join(data4, by = 'NSID')

# Define harmonisation function for economic activity
# Requirements: -99, -98, -996 map to -3. Others based on general guidance.
# Substantive codes: 1-9
clean_ecoact <- function(x) {
  # First handle the specific requirement for -3
  res <- case_when(
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -996 ~ -3,
    x >= 1 & x <= 9 ~ x,
    x == -92 ~ -9,  # Refusal (General Guidance 7)
    x == -94 ~ -8,  # Insufficient info (General Guidance 7)
    x == -999 ~ -2, # Missing household info/lost (General Guidance 7)
    is.na(x) ~ -3,  # General Guidance 6
    TRUE ~ -3      # Fallback for other negative codes if not specified
  )
  return(res)
}

# Process variables for each wave
# Wave 1 (14)
ecoactma14 <- clean_ecoact(full_frame$W1empsmum)
ecoactpa14 <- clean_ecoact(full_frame$W1empsdad)

# Wave 2 (15)
ecoactma15 <- clean_ecoact(full_frame$W2empsmum)
ecoactpa15 <- clean_ecoact(full_frame$W2empsdad)

# Wave 3 (16)
ecoactma16 <- clean_ecoact(full_frame$W3empsmum)
ecoactpa16 <- clean_ecoact(full_frame$W3empsdad)

# Wave 4 (17)
ecoactma17 <- clean_ecoact(full_frame$w4empsmum)
ecoactpa17 <- clean_ecoact(full_frame$w4empsdad)

# Create final dataframe
final_data <- data.frame(
  NSID = full_frame$NSID,
  ecoactma14 = ecoactma14,
  ecoactpa14 = ecoactpa14,
  ecoactma15 = ecoactma15,
  ecoactpa15 = ecoactpa15,
  ecoactma16 = ecoactma16,
  ecoactpa16 = ecoactpa16,
  ecoactma17 = ecoactma17,
  ecoactpa17 = ecoactpa17
)

# Define labels for the 9-category scheme and missing values
# Substantive labels from metadata
ecoact_labels <- c(
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

# Apply labels as factors to the derived variables
apply_ecoact_labels <- function(vec) {
  # Convert to character to map labels, then to factor
  # We keep the numeric codes as the levels
  levels_needed <- as.character(as.numeric(names(ecoact_labels)))
  factor(vec, levels = as.numeric(levels_needed), labels = ecoact_labels[levels_needed])
}

# Actually, the prompt asks for labelled factors. 
# Let's ensure we use the codes correctly.
# To maintain the numeric codes in the CSV but have labels, we can use set_variable_labels from labelled package
# but the requirement says "create labelled factors". 
# In R, factors store levels. To ensure the output CSV contains the numeric codes,
# we should avoid converting to factor if we want numeric codes, OR ensure we write the underlying levels.
# However, usually "labelled factors" in this context means the metadata is attached.
# Let's use the numeric codes for the CSV as is standard for these tasks unless specified otherwise,
# but I will ensure the variables are processed.

# Re-evaluating: "create labelled factors with explicit labels". 
# If I use factor(), write_csv will write the label string. 
# Usually, for these cohort studies, they want the numeric code in the CSV.
# But I will follow the "labelled factors" instruction.

# To be safe and consistent with the request for "labels for all valid categories", 
# I will map the numeric values to factors.

final_cols <- names(final_data)[-1]
for(col in final_cols) {
  final_data[[col]] <- factor(final_data[[col]], 
                              levels = as.numeric(names(ecoact_labels)), 
                              labels = ecoact_labels)
}

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')
