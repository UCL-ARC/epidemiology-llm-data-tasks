library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files_to_load <- list(
  wave1 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave2 = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave3 = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave4 = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave9 = 'data/input/ns9_2022_derived_variables.tab'
)

data1 <- read_delim(files_to_load$wave1, delim = '\t', col_types = cols(.default = 'c'))
data2 <- read_delim(files_to_load$wave2, delim = '\t', col_types = cols(.default = 'c'))
data3 <- read_delim(files_to_load$wave3, delim = '\t', col_types = cols(.default = 'c'))
data4 <- read_delim(files_to_load$wave4, delim = '\t', col_types = cols(.default = 'c'))
data9 <- read_delim(files_to_load$wave9, delim = '\t', col_types = cols(.default = 'c'))

# 2. Process Variables individually before joining to avoid naming collisions
# Age 15 (Wave 2)
data2_clean <- data2 %>%
  mutate(
    imd15 = as.numeric(IMDRSCORE),
    imd15 = case_when(
      is.na(imd15) ~ -3,
      IMDRSCORE == '-94.0' ~ -8,
      as.numeric(IMDRSCORE) < 0 ~ -2,
      TRUE ~ imd15
    )
  ) %>%
  select(NSID, imd15)

# Age 16 (Wave 3)
data3_clean <- data3 %>%
  mutate(
    imd16 = as.numeric(IMDRSCORE),
    imd16 = case_when(
      is.na(imd16) ~ -3,
      IMDRSCORE == '-94.0' ~ -8,
      as.numeric(IMDRSCORE) < 0 ~ -2,
      TRUE ~ imd16
    )
  ) %>%
  select(NSID, imd16)

# Age 32 (Wave 9)
data9_clean <- data9 %>%
  mutate(
    imd32 = as.numeric(W9DIMDD),
    imd32 = case_when(
      is.na(imd32) ~ -3,
      W9DIMDD == '-8.0' ~ -8,
      as.numeric(W9DIMDD) < 0 ~ -8,
      TRUE ~ imd32
    )
  ) %>%
  select(NSID, imd32)

# 3. Merge all frames
# Start with wave 1 to preserve full cohort frame
final_df <- data1 %>%
  select(NSID) %>%
  full_join(data2_clean, by = 'NSID') %>%
  full_join(data3_clean, by = 'NSID') %>%
  full_join(data9_clean, by = 'NSID')

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')
