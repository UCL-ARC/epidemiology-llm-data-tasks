library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# To avoid memory issues and many-to-many joins, we will only read the required columns
# and ensure NSID is treated as a character key.

read_ethnic_data <- function(filename, ethnic_var) {
  path <- paste0('data/input/', filename)
  # Use read_delim with col_select to only load NSID and the target variable
  df <- read_delim(path, delim = '\t', col_types = cols(NSID = col_character(), .default = col_double()), show_col_types = FALSE) %>%
    select(NSID, all_of(ethnic_var))
  return(df)
}

# Load specific variables
df1 <- read_ethnic_data('wave_one_lsype_young_person_2020.tab', 'W1ethnic2YP')
df2 <- read_ethnic_data('wave_two_lsype_young_person_2020.tab', 'W2ethnicYP')
df4 <- read_ethnic_data('wave_four_lsype_young_person_2020.tab', 'w4ethnic2YP')
df8 <- read_ethnic_data('ns8_2015_derived.tab', 'W8DETHN15')
df9 <- read_ethnic_data('ns9_2022_derived_variables.tab', 'W9DETHN15')

# Join only these smaller dataframes
cohort_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# Processing functions based on metadata
process_w1 <- function(x) {
  case_when(
    x >= 1 & x <= 16 ~ x,
    x == -999 ~ -2, x == -94 ~ -8, x == -92 ~ -9, x == -91 ~ -1, x == -1 ~ -8, TRUE ~ -3
  )
}

process_w2 <- function(x) {
  case_when(
    x >= 1 & x <= 16 ~ x,
    x == -998 ~ -2, x == -997 ~ -2, x == -995 ~ -2, x == -99 ~ -3, x == -92 ~ -9, x == -91 ~ -1, x == -1 ~ -8, TRUE ~ -3
  )
}

process_w4 <- function(x) {
  case_when(
    x >= 1 & x <= 16 ~ x,
    x == -94 ~ -8, x == -1 ~ -8, TRUE ~ -3
  )
}

process_w8 <- function(x) {
  case_when(
    x >= 1 & x <= 16 ~ x,
    x == -9 ~ -9, x == -8 ~ -8, x == -1 ~ -1, TRUE ~ -3
  )
}

process_w9 <- function(x) {
  case_when(
    x >= 1 & x <= 16 ~ x,
    x == -8 ~ -8, TRUE ~ -3
  )
}

# Derive consolidated eth
cohort_df <- cohort_df %>%
  mutate(
    v1 = process_w1(W1ethnic2YP),
    v2 = process_w2(W2ethnicYP),
    v4 = process_w4(w4ethnic2YP),
    v8 = process_w8(W8DETHN15),
    v9 = process_w9(W9DETHN15)
  ) %>%
  mutate(eth = case_when(
    v1 >= 1 & v1 <= 16 ~ v1,
    v2 >= 1 & v2 <= 16 ~ v2,
    v4 >= 1 & v4 <= 16 ~ v4,
    v8 >= 1 & v8 <= 16 ~ v8,
    v9 >= 1 & v9 <= 16 ~ v9,
    TRUE ~ -3
  ))

# Labels
eth_labels <- c(
  "1" = "White - British", "2" = "White - Irish", "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean", "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian", "7" = "Any other mixed background",
  "8" = "Indian", "9" = "Pakistani", "10" = "Bangladeshi",
  "11" = "Any other Asian background", "12" = "Black Caribbean",
  "13" = "Black African", "14" = "Any other Black background",
  "15" = "Chinese", "16" = "Any other ethnic background",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable"
)

cohort_df$eth <- factor(cohort_df$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

# Final output
final_df <- cohort_df %>%
  select(NSID, eth)

write_csv(final_df, 'data/output/cleaned_data.csv')