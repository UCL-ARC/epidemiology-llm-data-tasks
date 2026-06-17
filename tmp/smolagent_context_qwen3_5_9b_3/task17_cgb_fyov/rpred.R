library(haven)
library(dplyr)
library(readr)

if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Clean IMDRSCORE from wave2 (Age 15)
wave2_clean <- wave2 %>%
  mutate(
    imd15 = case_when(
      IMDRSCORE >= -999 & IMDRSCORE <= -1 | is.na(IMDRSCORE) ~ -3,
      IMDRSCORE == -94 ~ -8,
      TRUE ~ IMDRSCORE
    )
  ) %>%
  select(NSID, imd15)

# Clean IMDRSCORE from wave3 (Age 16)
wave3_clean <- wave3 %>%
  mutate(
    imd16 = case_when(
      IMDRSCORE >= -999 & IMDRSCORE <= -1 | is.na(IMDRSCORE) ~ -3,
      IMDRSCORE == -94 ~ -8,
      TRUE ~ IMDRSCORE
    )
  ) %>%
  select(NSID, imd16)

# Clean W9DIMDD from wave9 (Age 32)
wave9_clean <- wave9 %>%
  mutate(
    imd32 = case_when(
      W9DIMDD >= -8 | is.na(W9DIMDD) ~ -3,
      W9DIMDD == -94 ~ -8,
      TRUE ~ W9DIMDD
    )
  ) %>%
  select(NSID, imd32)

# Merge all waves
all_data <- full_join(wave1, wave2, by = 'NSID')
all_data <- full_join(all_data, wave3, by = 'NSID')
all_data <- full_join(all_data, wave4, by = 'NSID')
all_data <- full_join(all_data, wave9, by = 'NSID')

# Merge in clean IMD variables
all_data <- full_join(all_data, wave2_clean, by = 'NSID')
all_data <- full_join(all_data, wave3_clean, by = 'NSID')
all_data <- full_join(all_data, wave9_clean, by = 'NSID')

# Check final structure
cat('Variables in output:', paste(names(all_data), collapse = ', '), '\n')
cat('Rows:', nrow(all_data), '\n')

# Write output
write_csv(all_data, 'data/output/cleaned_data.csv')
