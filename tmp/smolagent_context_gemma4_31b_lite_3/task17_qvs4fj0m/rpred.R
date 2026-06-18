library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns9_2022_derived_variables.tab'
)

load_tab <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
}

data_list <- map(files, load_tab)
names(data_list) <- files

# 2. Extract specific variables from specific files to avoid name collisions and missing object errors
# Age 15: wave_two_lsype_family_background_2020.tab
wave2_df <- data_list[['wave_two_lsype_family_background_2020.tab']] %>%
  select(NSID, imd15_raw = IMDRSCORE)

# Age 16: wave_three_lsype_family_background_2020.tab
wave3_df <- data_list[['wave_three_lsype_family_background_2020.tab']] %>%
  select(NSID, imd16_raw = IMDRSCORE)

# Age 32: ns9_2022_derived_variables.tab
wave9_df <- data_list[['ns9_2022_derived_variables.tab']] %>%
  select(NSID, imd32_raw = W9DIMDD)

# Cohort frame from wave 1
cohort_frame <- data_list[['wave_one_lsype_young_person_2020.tab']] %>% select(NSID)

# Merge
final_df <- cohort_frame %>%
  full_join(wave2_df, by = 'NSID') %>%
  full_join(wave3_df, by = 'NSID') %>%
  full_join(wave9_df, by = 'NSID')

# 3. Variable Harmonisation

# imd15
final_df <- final_df %>%
  mutate(imd15 = case_when(
    imd15_raw == -94.0 ~ -8,
    imd15_raw >= -999.0 & imd15_raw <= -1.0 ~ -2,
    is.na(imd15_raw) ~ -3,
    TRUE ~ imd15_raw
  ))

# imd16
final_df <- final_df %>%
  mutate(imd16 = case_when(
    imd16_raw == -94.0 ~ -8,
    imd16_raw >= -999.0 & imd16_raw <= -1.0 ~ -2,
    is.na(imd16_raw) ~ -3,
    TRUE ~ imd16_raw
  ))

# imd32
final_df <- final_df %>%
  mutate(imd32_val = case_when(
    imd32_raw == -8.0 ~ -8,
    is.na(imd32_raw) ~ -3,
    TRUE ~ imd32_raw
  ))

# Convert imd32 to labelled factor
final_df$imd32 <- factor(final_df$imd32_val, 
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -8, -3),
                        labels = c('Most deprived decile', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 'Least deprived decile', 'Insufficient information', 'Not asked'))

# 4. Output
output_df <- final_df %>%
  select(NSID, imd15, imd16, imd32)

write_csv(output_df, 'data/output/cleaned_data.csv')
