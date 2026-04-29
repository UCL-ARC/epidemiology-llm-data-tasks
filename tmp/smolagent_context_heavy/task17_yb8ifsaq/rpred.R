library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_nine = 'data/input/ns9_2022_derived_variables.tab'
)

# Read all files as tab-delimited
data_one <- readr::read_delim(files$wave_one, delim = '\t', col_types = readr::cols(.default = 'c'))
data_four <- readr::read_delim(files$wave_four, delim = '\t', col_types = readr::cols(.default = 'c'))
data_two <- readr::read_delim(files$wave_two, delim = '\t', col_types = readr::cols(.default = 'c'))
data_three <- readr::read_delim(files$wave_three, delim = '\t', col_types = readr::cols(.default = 'c'))
data_nine <- readr::read_delim(files$wave_nine, delim = '\t', col_types = readr::cols(.default = 'c'))

# Create the full cohort frame (all unique NSIDs across all files)
all_ids <- bind_rows(
  data_one %>% select(NSID),
  data_four %>% select(NSID),
  data_two %>% select(NSID),
  data_three %>% select(NSID),
  data_nine %>% select(NSID)
) %>% distinct(NSID)

# Process IMD 15 (Wave 2)
# Variable: IMDRSCORE. Missing: -94.0 -> -8, -999.0 to -1.0 -> -2, NA -> -3
df_15 <- data_two %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd15 = as.numeric(IMDRSCORE)) %>%
  mutate(imd15 = case_when(
    is.na(imd15) ~ -3,
    imd15 == -94.0 ~ -8,
    imd15 <= -1.0 ~ -2,
    TRUE ~ imd15
  )) %>%
  select(NSID, imd15)

# Process IMD 16 (Wave 3)
# Variable: IMDRSCORE. Missing: -94.0 -> -8, -999.0 to -1.0 -> -2, NA -> -3
df_16 <- data_three %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd16 = as.numeric(IMDRSCORE)) %>%
  mutate(imd16 = case_when(
    is.na(imd16) ~ -3,
    imd16 == -94.0 ~ -8,
    imd16 <= -1.0 ~ -2,
    TRUE ~ imd16
  )) %>%
  select(NSID, imd16)

# Process IMD 32 (Wave 9)
# Variable: W9DIMDD. Missing: -8.0 -> -8, NA -> -3
df_32 <- data_nine %>%
  select(NSID, W9DIMDD) %>%
  mutate(imd32 = as.numeric(W9DIMDD)) %>%
  mutate(imd32 = case_when(
    is.na(imd32) ~ -3,
    imd32 == -8.0 ~ -8,
    TRUE ~ imd32
  )) %>%
  select(NSID, imd32)

# Merge into final analysis file
final_df <- all_ids %>%
  left_join(df_15, by = 'NSID') %>%
  left_join(df_16, by = 'NSID') %>%
  left_join(df_32, by = 'NSID') %>%
  mutate(
    imd15 = coalesce(imd15, -3),
    imd16 = coalesce(imd16, -3),
    imd32 = coalesce(imd32, -3)
  )

# Write output
readr::write_csv(final_df, 'data/output/cleaned_data.csv')