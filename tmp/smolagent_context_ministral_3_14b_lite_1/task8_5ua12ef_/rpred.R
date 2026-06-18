
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)

# Load datasets
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8_main <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_four, by = 'NSID') %>%
  full_join(ns8_main, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Create education variables
merged_data <- merged_data %>%
  mutate(
    educ25 = ifelse(is.na(W8DHANVQH), -3, W8DHANVQH),
    educ32 = ifelse(is.na(W9DANVQH), -3, W9DANVQH)
  )

# Create detailed qualification indicators
academic_vars <- grep('^W9ACQU', names(merged_data), value = TRUE)
vocational_vars <- grep('^W9VCQU', names(merged_data), value = TRUE)

merged_data$educadtl32 <- ifelse(rowSums(merged_data[academic_vars] == 1) > 0, 1, 0)
merged_data$educvdtl32 <- ifelse(rowSums(merged_data[vocational_vars] == 1) > 0, 1, 0)

# Select and save final variables
final_data <- merged_data %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)
write_csv(final_data, 'data/output/cleaned_data.csv')
