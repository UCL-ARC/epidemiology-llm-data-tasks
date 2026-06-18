
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the datasets
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Standardize missing values for a single variable
standardize_single_var <- function(df, var_name) {
  df %>%
    mutate(!!var_name := case_when(
      .data[[var_name]] == -999 ~ -2,
      .data[[var_name]] == -99 ~ -3,
      .data[[var_name]] == -98 ~ -1,
      .data[[var_name]] == -94 ~ -8,
      .data[[var_name]] == -92 ~ -9,
      .data[[var_name]] == -91 ~ -1,
      .data[[var_name]] == -1 ~ -8,
      TRUE ~ .data[[var_name]]
    ))
}

# Standardize mother's and father's education variables in all waves
wave1 <- wave1 %>%
  standardize_single_var('W1hiqualmum') %>%
  standardize_single_var('W1hiqualdad')

wave2 <- wave2 %>%
  standardize_single_var('W2hiqualmum') %>%
  standardize_single_var('W2hiqualdad')

wave4 <- wave4 %>%
  standardize_single_var('w4hiqualmum') %>%
  standardize_single_var('w4hiqualdad')

# Merge all datasets by NSID
merged_data <- full_join(wave1, wave2, by = 'NSID', copy = TRUE) %>%
  full_join(., wave4, by = 'NSID', copy = TRUE)

# Consolidate mother's education
merged_data <- merged_data %>%
  mutate(educdtlma = case_when(
    W1hiqualmum > 0 ~ W1hiqualmum,
    W2hiqualmum > 0 ~ W2hiqualmum,
    w4hiqualmum > 0 ~ w4hiqualmum,
    TRUE ~ -3
  )) %>%
  mutate(educma = case_when(
    educdtlma %in% c(1, 2, 3, 4) ~ 0,
    educdtlma %in% c(5:17) ~ 1,
    educdtlma == 18 ~ 2,
    educdtlma == 19 ~ 3,
    educdtlma == 20 ~ 4,
    TRUE ~ -3
  ))

# Consolidate father's education
merged_data <- merged_data %>%
  mutate(educdtlpa = case_when(
    W1hiqualdad > 0 ~ W1hiqualdad,
    W2hiqualdad > 0 ~ W2hiqualdad,
    w4hiqualdad > 0 ~ w4hiqualdad,
    TRUE ~ -3
  )) %>%
  mutate(educpa = case_when(
    educdtlpa %in% c(1, 2, 3, 4) ~ 0,
    educdtlpa %in% c(5:17) ~ 1,
    educdtlpa == 18 ~ 2,
    educdtlpa == 19 ~ 3,
    educdtlpa == 20 ~ 4,
    TRUE ~ -3
  ))

# Select only the final derived variables and NSID
final_df <- merged_data %>%
  select(NSID, educdtlma, educma, educdtlpa, educpa)

# Ensure the output directory exists
if (!dir.exists('data/output')) {
  dir.create('data/output')
}

# Write the output file
write_csv(final_df, 'data/output/cleaned_data.csv')

# Verify the file was created
file.exists('data/output/cleaned_data.csv')
