library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load all files from data/input/
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets by NSID
# Start with the first file and progressively merge others
df <- full_join(w1, w4, by = 'NSID')
df <- full_join(df, ns8, by = 'NSID')
df <- full_join(df, ns9, by = 'NSID')

# Create bmi25 from W8DBMI (Wave 8 at age 25)
df <- df %>%
  mutate(bmi25 = case_when(
    W8DBMI == -9.0 ~ -9,
    W8DBMI == -8.0 ~ -8,
    W8DBMI == -1.0 ~ -1,
    is.na(W8DBMI) ~ -3,
    TRUE ~ W8DBMI
  ))

# Create bmi32 from W9DBMI (Wave 9 at age 32)
df <- df %>%
  mutate(bmi32 = case_when(
    W9DBMI == -9.0 ~ -9,
    W9DBMI == -8.0 ~ -8,
    W9DBMI == -1.0 ~ -1,
    is.na(W9DBMI) ~ -3,
    TRUE ~ W9DBMI
  ))

# Remove raw source variables, keep only final derived variables and ID
final_df <- df %>%
  select(NSID, bmi25, bmi32)

# Write to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
cat('Output file: data/output/cleaned_data.csv\n')
cat('Observing output dimensions:\n')
print(dim(final_df))