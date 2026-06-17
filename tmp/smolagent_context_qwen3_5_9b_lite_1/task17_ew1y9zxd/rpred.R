library(haven)
library(dplyr)
library(readr)
library(labelled)

# Load all files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Process IMDRSCORE for wave 2 (age 15)
imd15 <- wave_two %>%
  mutate(
    imd15 = case_when(
      is.na(IMDRSCORE) ~ -3,
      IMDRSCORE == -94.0 ~ -8,
      IMDRSCORE >= -999.0 & IMDRSCORE <= -1.0 ~ -3,
      TRUE ~ IMDRSCORE
    )
  )

# Process IMDRSCORE for wave 3 (age 16)
imd16 <- wave_three %>%
  mutate(
    imd16 = case_when(
      is.na(IMDRSCORE) ~ -3,
      IMDRSCORE == -94.0 ~ -8,
      IMDRSCORE >= -999.0 & IMDRSCORE <= -1.0 ~ -3,
      TRUE ~ IMDRSCORE
    )
  )

# Process W9DIMDD for wave 9 (age 32)
# -8.0 = Insufficient information -> -8
# -8.0 through None = missing
imd32 <- ns9 %>%
  mutate(
    imd32 = case_when(
      is.na(W9DIMDD) ~ -3,
      W9DIMDD == -8.0 ~ -8,
      W9DIMDD == -1 ~ -3,
      TRUE ~ W9DIMDD
    )
  )

# Merge all datasets
merged <- full_join(wave_one, imd15, by = 'NSID') %>%
  full_join(imd16, by = 'NSID') %>%
  full_join(imd32, by = 'NSID') %>%
  full_join(wave_four, by = 'NSID')

# Select final variables: NSID, imd15, imd16, imd32
final <- merged %>%
  select(NSID, imd15, imd16, imd32)

# Write output
write_csv(final, 'data/output/cleaned_data.csv')
