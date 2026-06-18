library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = 'character'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = 'character'))
wave8_self <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = 'character'))
wave8_der <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = 'character'))
wave9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = 'character'))
wave9_der <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = 'character'))

# Merge all into a base frame
full_df <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8_self, by = 'NSID') %>%
  full_join(wave8_der, by = 'NSID') %>%
  full_join(wave9_main, by = 'NSID') %>%
  full_join(wave9_der, by = 'NSID')

# Helper for item-summed Likert scores
calc_ghq_sum <- function(vars) {
  # vars is a list of column names
  # Return a vector of summed scores or missing codes
  # Logic: 
  # - If all 12 items are NA -> -3
  # - If any item has a negative value -> -8
  # - Otherwise -> sum (0-12 range)
  
  # This will be applied row-wise
}

# Process Age 15 (Wave 2)
items15 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')
full_df <- full_df %>%
  mutate(
    ghqtl15 = apply(across(all_of(items15)), 1, function(x) {
      if (all(is.na(x))) return(-3)
      if (any(x < 0, na.rm = TRUE)) return(-8)
      sum(x, na.rm = TRUE)
    }),
    ghq15 = case_when(
      W2ghq12scr == -97 ~ -9,
      W2ghq12scr == -92 ~ -9,
      W2ghq12scr == -99 ~ -3,
      W2ghq12scr < 0 ~ -2, # Simplified default for others
      TRUE ~ W2ghq12scr
    )
  )

# Process Age 17 (Wave 4)
items17 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')
full_df <- full_df %>%
  mutate(
    ghqtl17 = apply(across(all_of(items17)), 1, function(x) {
      if (all(is.na(x))) return(-3)
      if (any(x < 0, na.rm = TRUE)) return(-8)
      sum(x, na.rm = TRUE)
    }),
    ghq17 = case_when(
      W4ghq12scr == -97 ~ -9,
      W4ghq12scr == -92 ~ -9,
      W4ghq12scr == -99 ~ -3,
      W4ghq12scr < 0 ~ -2,
      TRUE ~ W4ghq12scr
    )
  )

# Process Age 25 (Wave 8)
items25 <- paste0('W8GHQ12_', 1:12)
full_df <- full_df %>%
  mutate(
    ghqtl25 = apply(across(all_of(items25)), 1, function(x) {
      if (all(is.na(x))) return(-3)
      if (any(x < 0, na.rm = TRUE)) return(-8)
      sum(x, na.rm = TRUE)
    }),
    ghq25 = case_when(
      W8DGHQSC == -9 ~ -9,
      W8DGHQSC == -8 ~ -8,
      W8DGHQSC == -1 ~ -1,
      TRUE ~ W8DGHQSC
    )
  )

# Process Age 32 (Wave 9)
items32 <- paste0('W9GHQ12_', 1:12)
full_df <- full_df %>%
  mutate(
    ghqtl32 = apply(across(all_of(items32)), 1, function(x) {
      if (all(is.na(x))) return(-3)
      if (any(x < 0, na.rm = TRUE)) return(-8)
      sum(x, na.rm = TRUE)
    }),
    ghq32 = case_when(
      W9DGHQSC == -9 ~ -9,
      W9DGHQSC == -8 ~ -8,
      W9DGHQSC == -1 ~ -1,
      TRUE ~ W9DGHQSC
    )
  )

# Final selection
final_df <- full_df %>%
  select(NSID, ghqtl15, ghq17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32) # Wait, requested order was slightly different in names but I'll just keep them

# Correcting the column names to match exactly requested: ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32
# My current selection has ghqtl15, ghq17 (wrong), ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32
# Let's fix the logic for ghq17 in the select

final_df <- full_df %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

write_csv(final_df, 'data/output/cleaned_data.csv')
