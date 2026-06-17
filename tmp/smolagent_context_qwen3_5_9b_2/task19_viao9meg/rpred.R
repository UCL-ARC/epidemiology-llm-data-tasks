library(haven)
library(dplyr)
library(readr)

# Load all files
data_wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
data_wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
data_wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
data_wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Start with wave1 as base
cleaned <- data_wave1

# Merge wave4
cleaned <- full_join(cleaned, data_wave4, by = 'NSID')

# Merge wave8
cleaned <- full_join(cleaned, data_wave8, by = 'NSID')

# Merge wave9
cleaned <- full_join(cleaned, data_wave9, by = 'NSID')

# Process W8DBMI for bmi25 (wave8, age 25)
# Missing values: -9.0 (Refused), -8.0 (Insufficient information), -1.0 (Not applicable)
cleaned <- cleaned %>%
  mutate(
    bmi25 = W8DBMI,
    bmi25 = case_when(
      is.na(bmi25) | bmi25 == -3 | bmi25 == -2 | bmi25 == -7 ~ as.numeric(NA_real_),
      bmi25 == -9.0 ~ -9,
      bmi25 == -8.0 ~ -8,
      bmi25 == -1.0 ~ -1,
      TRUE ~ bmi25
    )
  )

# Process W9DBMI for bmi32 (wave9, age 32)
# Missing values: -9.0 (Refused), -8.0 (Insufficient information), -1.0 (Not applicable)
cleaned <- cleaned %>%
  mutate(
    bmi32 = W9DBMI,
    bmi32 = case_when(
      is.na(bmi32) | bmi32 == -3 | bmi32 == -2 | bmi32 == -7 ~ as.numeric(NA_real_),
      bmi32 == -9.0 ~ -9,
      bmi32 == -8.0 ~ -8,
      bmi32 == -1.0 ~ -1,
      TRUE ~ bmi32
    )
  )

# Select final variables: NSID, bmi25, bmi32
cleaned <- cleaned %>%
  select(NSID, bmi25, bmi32)

# Write output
write_csv(cleaned, 'data/output/cleaned_data.csv')

cat('Script completed successfully\n')
cat('Output written to data/output/cleaned_data.csv\n')
cat('Rows:', nrow(cleaned), '\n')
cat('Columns:', ncol(cleaned), '\n')
