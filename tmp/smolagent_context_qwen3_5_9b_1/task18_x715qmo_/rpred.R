library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all dataset files
df_wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
df_wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
df_wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
df_ns8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
df_ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
df_ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')
df_ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets by NSID
df_merged <- full_join(df_wave1, df_wave2, by = 'NSID')
df_merged <- full_join(df_merged, df_wave4, by = 'NSID')
df_merged <- full_join(df_merged, df_ns8, by = 'NSID')
df_merged <- full_join(df_merged, df_ns8_derived, by = 'NSID')
df_merged <- full_join(df_merged, df_ns9, by = 'NSID')
df_merged <- full_join(df_merged, df_ns9_derived, by = 'NSID')

# Define GHQ item variables for each wave
ghq_items_15 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 
                  'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 
                  'W2wthlessYP', 'W2happyYP')

ghq_items_17 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 
                  'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 
                  'W4WthlessYP', 'W4HappyYP')

ghq_items_25 <- c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4', 'W8GHQ12_5', 
                  'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8', 'W8GHQ12_9', 'W8GHQ12_10', 
                  'W8GHQ12_11', 'W8GHQ12_12')

ghq_items_32 <- c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4', 'W9GHQ12_5', 
                  'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8', 'W9GHQ12_9', 'W9GHQ12_10', 
                  'W9GHQ12_11', 'W9GHQ12_12')

# Compute item-summed scores for wave 15
df_merged <- df_merged %>%
  mutate(
    all_na_15 = if_all(all_of(ghq_items_15), ~is.na(.)),
    any_neg_15 = if_any(all_of(ghq_items_15), ~. < 0),
    ghqtl15 = case_when(
      all_na_15 == TRUE ~ -3,
      any_neg_15 == TRUE ~ -8,
      TRUE ~ rowSums(across(all_of(ghq_items_15)))
    )
  ) %>%
  select(-all_na_15, -any_neg_15)

# Compute item-summed scores for wave 17
df_merged <- df_merged %>%
  mutate(
    all_na_17 = if_all(all_of(ghq_items_17), ~is.na(.)),
    any_neg_17 = if_any(all_of(ghq_items_17), ~. < 0),
    ghqtl17 = case_when(
      all_na_17 == TRUE ~ -3,
      any_neg_17 == TRUE ~ -8,
      TRUE ~ rowSums(across(all_of(ghq_items_17)))
    )
  ) %>%
  select(-all_na_17, -any_neg_17)

# Compute item-summed scores for wave 25
df_merged <- df_merged %>%
  mutate(
    all_na_25 = if_all(all_of(ghq_items_25), ~is.na(.)),
    any_neg_25 = if_any(all_of(ghq_items_25), ~. < 0),
    ghqtl25 = case_when(
      all_na_25 == TRUE ~ -3,
      any_neg_25 == TRUE ~ -8,
      TRUE ~ rowSums(across(all_of(ghq_items_25)))
    )
  ) %>%
  select(-all_na_25, -any_neg_25)

# Compute item-summed scores for wave 32
df_merged <- df_merged %>%
  mutate(
    all_na_32 = if_all(all_of(ghq_items_32), ~is.na(.)),
    any_neg_32 = if_any(all_of(ghq_items_32), ~. < 0),
    ghqtl32 = case_when(
      all_na_32 == TRUE ~ -3,
      any_neg_32 == TRUE ~ -8,
      TRUE ~ rowSums(across(all_of(ghq_items_32)))
    )
  ) %>%
  select(-all_na_32, -any_neg_32)

# Harmonize pre-derived GHQ scores (caseness)
# Wave 2 (age 15): map -97 and -92 to -9
df_merged <- df_merged %>%
  mutate(
    ghq15 = case_when(
      W2ghq12scr %in% c(-97, -92) ~ -9,
      is.na(W2ghq12scr) ~ -3,
      TRUE ~ W2ghq12scr
    ),
    ghq17 = case_when(
      W4ghq12scr %in% c(-97, -92) ~ -9,
      is.na(W4ghq12scr) ~ -3,
      TRUE ~ W4ghq12scr
    ),
    ghq25 = case_when(
      W8DGHQSC == -9 ~ -9,
      W8DGHQSC == -8 ~ -9,
      is.na(W8DGHQSC) ~ -3,
      TRUE ~ W8DGHQSC
    ),
    ghq32 = case_when(
      W9DGHQSC == -9 ~ -9,
      W9DGHQSC == -8 ~ -9,
      is.na(W9DGHQSC) ~ -3,
      TRUE ~ W9DGHQSC
    )
  )

# Select only required variables: NSID and the four ghqtl and four ghq variables
final_df <- df_merged %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

print('Script completed successfully')