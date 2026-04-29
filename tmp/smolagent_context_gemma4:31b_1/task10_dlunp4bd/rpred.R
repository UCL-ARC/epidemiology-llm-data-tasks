library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
load_data_with_nsid <- function(f) {
  df <- readr::read_delim(paste0('data/input/', f), delim = '\t')
  df <- df %>% mutate(NSID = as.character(NSID))
  return(df)
}

wv1 <- load_data_with_nsid('wave_one_lsype_young_person_2020.tab')
wv4 <- load_data_with_nsid('wave_four_lsype_young_person_2020.tab')
wv5 <- load_data_with_nsid('wave_five_lsype_young_person_2020.tab')
wv6 <- load_data_with_nsid('wave_six_lsype_young_person_2020.tab')
wv7 <- load_data_with_nsid('wave_seven_lsype_young_person_2020.tab')
wv8 <- load_data_with_nsid('ns8_2015_derived.tab')
wv9 <- load_data_with_nsid('ns9_2022_derived_variables.tab')

# Merge using full_join
merged_data <- wv1 %>%
  full_join(wv4, by = 'NSID') %>%
  full_join(wv5, by = 'NSID') %>%
  full_join(wv6, by = 'NSID') %>%
  full_join(wv7, by = 'NSID') %>%
  full_join(wv8, by = 'NSID') %>%
  full_join(wv9, by = 'NSID')

# Helper for missing value harmonization
harmonize_missing <- function(x) {
  case_when(
    x == -999 ~ -2,
    x == -998 ~ -2,
    x == -997 ~ -2,
    x == -995 ~ -2,
    x == -94  ~ -8,
    x == -92  ~ -9,
    x == -99  ~ -3,
    x == -91  ~ -1,
    TRUE      ~ x
  )
}

# Recoding inside mutate to ensure variables are found in the dataframe
final_df <- merged_data %>%
  mutate(
    # Age 17
    ecoact17 = case_when(
      W4empsYP == 1 ~ 1,
      W4empsYP == 2 ~ 1,
      W4empsYP == 3 ~ 3,
      W4empsYP == 4 ~ 2,
      W4empsYP == 5 ~ 2,
      W4empsYP == 6 ~ 4,
      W4empsYP == 7 ~ 6,
      W4empsYP == 8 ~ 5,
      W4empsYP == 9 ~ 6,
      TRUE ~ harmonize_missing(W4empsYP)
    ),
    # Age 18
    ecoact18 = case_when(
      W5mainactYP == 3 ~ 1,
      W5mainactYP == 1 ~ 2,
      W5mainactYP == 2 ~ 2,
      W5mainactYP == 4 ~ 2,
      W5mainactYP == 5 ~ 2,
      W5mainactYP == 6 ~ 2,
      W5mainactYP == 7 ~ 3,
      W5mainactYP == 8 ~ 4,
      W5mainactYP == 9 ~ 6,
      W5mainactYP == 10 ~ 6,
      W5mainactYP == 11 ~ 6,
      TRUE ~ harmonize_missing(W5mainactYP)
    ),
    # Age 19
    ecoact19 = case_when(
      W6TCurrentAct == 3 ~ 1,
      W6TCurrentAct == 1 ~ 2,
      W6TCurrentAct == 2 ~ 2,
      W6TCurrentAct == 4 ~ 2,
      W6TCurrentAct == 5 ~ 2,
      W6TCurrentAct == 10 ~ 2,
      W6TCurrentAct == 6 ~ 6,
      W6TCurrentAct == 7 ~ 4,
      W6TCurrentAct == 8 ~ 3,
      W6TCurrentAct == 9 ~ 6,
      W6TCurrentAct == 11 ~ 1,
      TRUE ~ harmonize_missing(W6TCurrentAct)
    ),
    # Age 20
    ecoact20 = case_when(
      W7TCurrentAct == 3 ~ 1,
      W7TCurrentAct == 1 ~ 2,
      W7TCurrentAct == 2 ~ 2,
      W7TCurrentAct == 4 ~ 2,
      W7TCurrentAct == 5 ~ 2,
      W7TCurrentAct == 9 ~ 2,
      W7TCurrentAct == 6 ~ 6,
      W7TCurrentAct == 7 ~ 4,
      W7TCurrentAct == 8 ~ 3,
      W7TCurrentAct == 10 ~ 1,
      W7TCurrentAct == 11 ~ 2,
      W7TCurrentAct == 12 ~ 6,
      W7TCurrentAct == 13 ~ 6,
      W7TCurrentAct == 14 ~ 5,
      W7TCurrentAct == 15 ~ 6,
      TRUE ~ harmonize_missing(W7TCurrentAct)
    ),
    # Age 25 Detailed
    ecoactadu25 = case_when(
      W8DACTIVITYC >= -9 & W8DACTIVITYC <= 10 ~ W8DACTIVITYC,
      TRUE ~ harmonize_missing(W8DACTIVITYC)
    ),
    # Age 25 Collapsed
    ecoact25 = case_when(
      ecoactadu25 == 1 ~ 1,
      ecoactadu25 == 2 ~ 1,
      ecoactadu25 == 3 ~ 1,
      ecoactadu25 == 4 ~ 3,
      ecoactadu25 == 5 ~ 2,
      ecoactadu25 == 6 ~ 2,
      ecoactadu25 == 7 ~ 2,
      ecoactadu25 == 8 ~ 5,
      ecoactadu25 == 9 ~ 4,
      ecoactadu25 == 10 ~ 6,
      TRUE ~ ecoactadu25
    ),
    # Age 32 Detailed
    ecoactadu32 = case_when(
      W9DACTIVITYC >= -9 & W9DACTIVITYC <= 10 ~ W9DACTIVITYC,
      TRUE ~ harmonize_missing(W9DACTIVITYC)
    ),
    # Age 32 Collapsed
    ecoact32 = case_when(
      ecoactadu32 == 1 ~ 1,
      ecoactadu32 == 2 ~ 1,
      ecoactadu32 == 3 ~ 1,
      ecoactadu32 == 4 ~ 3,
      ecoactadu32 == 5 ~ 2,
      ecoactadu32 == 6 ~ 2,
      ecoactadu32 == 7 ~ 2,
      ecoactadu32 == 8 ~ 5,
      ecoactadu32 == 9 ~ 4,
      ecoactadu32 == 10 ~ 6,
      TRUE ~ ecoactadu32
    )
  )

# Handle Nulls as -3
final_df <- final_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), -3, .)))

# Convert to factors
final_vars <- c('ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32', 'ecoactadu25', 'ecoactadu32')
final_df <- final_df %>%
  mutate(across(all_of(final_vars), as.factor))

# Final selection
final_df <- final_df %>%
  select(NSID, all_of(final_vars))

# Save output
write_csv(final_df, 'data/output/cleaned_data.csv')
