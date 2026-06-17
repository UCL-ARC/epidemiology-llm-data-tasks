library(dplyr)
library(readr)
library(haven)
library(labelled)

# Define file paths
files <- list(
  'wave_one_lsype_young_person_2020.tab' = 'data/input/wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab' = 'data/input/wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab' = 'data/input/ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab' = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all files
df1 <- read_delim(files[['wave_one_lsype_young_person_2020.tab']], delim = '\t', show_col_types = FALSE)
df4 <- read_delim(files[['wave_four_lsype_young_person_2020.tab']], delim = '\t', show_col_types = FALSE)
df8 <- read_delim(files[['ns8_2015_derived.tab']], delim = '\t', show_col_types = FALSE)
df9 <- read_delim(files[['ns9_2022_derived_variables.tab']], delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID using full_join
cleaned <- df1 %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# Check structure
cat('Number of rows:', nrow(cleaned), '\n')
cat('Columns:', paste(names(cleaned), collapse = ', '), '\n')

# Create bmi25 from W8DBMI (Wave 8, Age 25)
# W8DBMI has user missing values: -9 (Refused), -8 (Insufficient information), -1 (Not applicable)
# Since BMI is continuous/SCALE, keep valid values numeric
# Map missing codes to standard scheme
cleaned <- cleaned %>%
  mutate(
    bmi25 = case_when(
      W8DBMI == -9 ~ -9,   # Refusal
      W8DBMI == -8 ~ -8,   # Don't know / insufficient information
      W8DBMI == -1 ~ -1,   # Not applicable
      is.na(W8DBMI) ~ -3,  # Not asked / not interviewed
      W8DBMI >= 0 ~ W8DBMI # Valid BMI value
    )
  )

# Create bmi32 from W9DBMI (Wave 9, Age 32)
# W9DBMI has user missing values: -9 (Refused), -8 (Insufficient information), -1 (Not applicable)
cleaned <- cleaned %>%
  mutate(
    bmi32 = case_when(
      W9DBMI == -9 ~ -9,   # Refusal
      W9DBMI == -8 ~ -8,   # Don't know / insufficient information
      W9DBMI == -1 ~ -1,   # Not applicable
      is.na(W9DBMI) ~ -3,  # Not asked / not interviewed
      W9DBMI >= 0 ~ W9DBMI # Valid BMI value
    )
  )

# Select only NSID and final derived variables
output <- cleaned %>%
  select(NSID, bmi25, bmi32)

# Add value labels for missing codes
bmi25_lab <- labelled(
  output$bmi25,
  labels = c(
    'Refused' = -9,
    "Don't know / insufficient information" = -8,
    'Not applicable' = -1,
    'Not asked / not interviewed' = -3
  )
)

bmi32_lab <- labelled(
  output$bmi32,
  labels = c(
    'Refused' = -9,
    "Don't know / insufficient information" = -8,
    'Not applicable' = -1,
    'Not asked / not interviewed' = -3
  )
)

output$bmi25 <- bmi25_lab
output$bmi32 <- bmi32_lab

# Write output
dir.create('data/output', showWarnings = FALSE)
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written successfully\n')
cat('Number of observations:', nrow(output), '\n')
print(head(output))
