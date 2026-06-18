library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files listed in metadata to preserve cohort frame
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab',
  'ns9_2022_main_interview.tab'
)

# Read all files as tab-delimited
data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric'))
})

# The first file might have NSID as character, so we fix that
# Actually, let's read NSID as character explicitly
data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'))
})

names(data_list) <- files

# Create the full cohort frame using full_join on NSID
cohort_frame <- data_list[[1]] %>%
  select(NSID)

for (i in 2:length(data_list)) {
  cohort_frame <- full_join(cohort_frame, data_list[[i]], by = 'NSID')
}

# Process variables from specific files to avoid column name collisions (e.g., urbind in W2 and W3)

# Wave 2 (Age 15)
w2_data <- data_list[['wave_two_lsype_family_background_2020.tab']]
w2_vars <- w2_data %>%
  select(NSID, urbind, gor) %>%
  mutate(
    regub15 = ifelse(urbind == -94, -8, urbind),
    regov15 = ifelse(gor == -94, -8, gor)
  ) %>%
  select(NSID, regub15, regov15)

# Wave 3 (Age 16)
w3_data <- data_list[['wave_three_lsype_family_background_2020.tab']]
w3_vars <- w3_data %>%
  select(NSID, urbind, gor) %>%
  mutate(
    regub16 = ifelse(urbind == -94, -8, urbind),
    regov16 = ifelse(gor == -94, -8, gor)
  ) %>%
  select(NSID, regub16, regov16)

# Wave 8 (Age 25)
w8_data <- data_list[['ns8_2015_derived.tab']]
w8_vars <- w8_data %>%
  select(NSID, W8DGOR) %>%
  mutate(regor25 = W8DGOR) %>%
  select(NSID, regor25)

# Wave 9 (Age 32) - Derived
w9d_data <- data_list[['ns9_2022_derived_variables.tab']]
w9d_vars <- w9d_data %>%
  select(NSID, W9DRGN) %>%
  mutate(regor32 = W9DRGN) %>%
  select(NSID, regor32)

# Wave 9 (Age 32) - Main
w9m_data <- data_list[['ns9_2022_main_interview.tab']]
w9m_vars <- w9m_data %>%
  select(NSID, W9NATIONRES) %>%
  mutate(regint32 = W9NATIONRES) %>%
  select(NSID, regint32)

# Merge all derived variables back to the cohort frame
final_df <- cohort_frame %>%
  select(NSID) %>%
  left_join(w2_vars, by = 'NSID') %>%
  left_join(w3_vars, by = 'NSID') %>%
  left_join(w8_vars, by = 'NSID') %>%
  left_join(w9d_vars, by = 'NSID') %>%
  left_join(w9m_vars, by = 'NSID')

# Standard missing value harmonization: convert all remaining NAs to -3
final_df <- final_df %>%
  mutate(across(-NSID, ~ifelse(is.na(.), -3, .)))

# Write to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')