library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define filenames from metadata
files <- c('wave_one_lsype_young_person_2020.tab', 
           'wave_four_lsype_young_person_2020.tab', 
           'ns8_2015_derived.tab', 
           'ns9_2022_derived_variables.tab')

# Read files
data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))
names(data_list) <- files

# Merge datasets
full_frame <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_frame <- full_join(full_frame, data_list[[i]], by = 'NSID')
}

# 2. Target Variables Identification
# Target: BMI at age 25 and 32
# Age 25 corresponds to Wave 8 (W8DBMI)
# Age 32 corresponds to Wave 9 (W9DBMI)

# 3. Processing and Missing Value Harmonisation
# Missing values for BMI in both waves: 
# -9.0: Refused -> -9
# -8.0: Insufficient info -> -8
# -1.0: Not applicable -> -1
# NA -> -3 (Not asked/interviewed)

process_bmi <- function(var_name) {
  vec <- full_frame[[var_name]]
  
  # Handle NAs first
  vec[is.na(vec)] <- -3
  
  # The metadata says -9, -8, -1 are already the labels for Refused, Insufficient, and Not applicable
  # Standard codes: -9=Refusal, -8=Don't know/insufficient, -7=Prefer not to say, -3=Not asked, -2=Schedule error, -1=Not applicable
  # The raw values -9, -8, -1 in the metadata align with our standard codes.
  
  return(vec)
}

# Create output variables
# Requested variables: bmi25 and bmi32
full_frame <- full_frame %>%
  mutate(
    bmi25 = process_bmi('W8DBMI'),
    bmi32 = process_bmi('W9DBMI')
  )

# 4. Final Selection
final_data <- full_frame %>%
  select(NSID, bmi25, bmi32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')
