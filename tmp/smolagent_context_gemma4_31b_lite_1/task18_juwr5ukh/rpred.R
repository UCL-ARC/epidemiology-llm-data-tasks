library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'character'))
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'character'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'character'))
wave8_sc <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = cols(.default = 'character'))
wave8_der <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = cols(.default = 'character'))
wave9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = cols(.default = 'character'))
wave9_der <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols(.default = 'character'))

# Merge frames
full_frame <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8_sc, by = 'NSID') %>%
  full_join(wave8_der, by = 'NSID') %>%
  full_join(wave9_main, by = 'NSID') %>%
  full_join(wave9_der, by = 'NSID')

# Helper to convert and clean missing values based on metadata
# Standard Missing-Value Codes:
# -9 = Refusal, -8 = Don't know, -7 = Prefer not to say, -3 = Not asked, -2 = Schedule not applicable, -1 = Item not applicable
clean_ghq_var <- function(x, wave) {
  x <- as.numeric(x)
  if (wave == 'W2' || wave == 'W4') {
    # Wave 2 & 4 mappings
    x <- case_when(
      x == -92.0 ~ -9, # Refused
      x == -1.0  ~ -8, # Don't Know
      x == -91.0 ~ -1, # Not applicable
      x == -99.0 ~ -3, # YP not interviewed
      x == -97.0 ~ -7, # YP refused
      x == -98.0 ~ -2, # Interviewer missed
      x == -97.0 ~ -2, # Script error (Wait, -97 was refused, -97 is script error in W2)
      # Correcting W2 specifically from metadata:
      # -998: Missed (-2), -997: Script error (-2), -995: Missing history (-2), -99: not interviewed (-3), -97: refused (-7), -96: interpreter (-2), -92: Refused (-9), -91: Not applicable (-1), -1: Don't Know (-8)
      x == -998.0 ~ -2,
      x == -997.0 ~ -2,
      x == -995.0 ~ -2,
      x == -99.0  ~ -3,
      x == -97.0  ~ -7,
      x == -96.0  ~ -2,
      x == -92.0  ~ -9,
      x == -91.0  ~ -1,
      x == -1.0   ~ -8,
      TRUE ~ x
    )
  } else if (wave == 'W8' || wave == 'W9') {
    # Wave 8 & 9 mappings
    x <- case_when(
      x == -9.0 ~ -9, # Refused
      x == -8.0 ~ -8, # Don't know
      x == -1.0 ~ -1, # Not applicable
      x == -3.0 ~ -3, # Not asked (W9)
      TRUE ~ x
    )
  }
  x[is.na(x)] <- -3
  return(x)
}

# Process waves
# Age 15 (Wave 2)
vars_15 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')
full_frame <- full_frame %>%
  mutate(across(all_of(vars_15), ~clean_ghq_var(.x, 'W2'), .names = 'tmp_{.col}'))

# Age 17 (Wave 4)
vars_17 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')
full_frame <- full_frame %>%
  mutate(across(all_of(vars_17), ~clean_ghq_var(.x, 'W4'), .names = 'tmp_{.col}'))

# Age 25 (Wave 8)
vars_25 <- paste0('W8GHQ12_', 1:12)
full_frame <- full_frame %>%
  mutate(across(all_of(vars_25), ~clean_ghq_var(.x, 'W8'), .names = 'tmp_{.col}'))

# Age 32 (Wave 9)
vars_32 <- paste0('W9GHQ12_', 1:12)
full_frame <- full_frame %>%
  mutate(across(all_of(vars_32), ~clean_ghq_var(.x, 'W9'), .names = 'tmp_{.col}'))

# Summing logic: only sum if all values are non-negative
calc_sum <- function(row_data) {
  if (any(is.na(row_data)) || any(row_data < 0)) return(NA)
  return(sum(row_data))
}

# GHQ Likert Sums
full_frame <- full_frame %>%
  rowwise() %>%
  mutate(
    ghqtl15 = calc_sum(c_across(starts_with('tmp_W2'))),
    ghqtl17 = calc_sum(c_across(starts_with('tmp_W4'))),
    ghqtl25 = calc_sum(c_across(starts_with('tmp_W8GHQ12'))),
    ghqtl32 = calc_sum(c_across(starts_with('tmp_W9GHQ12')))
  ) %>% ungroup()

# Caseness scores (using pre-derived variables)
# Wave 2: W2ghq12scr, Wave 4: W4ghq12scr, Wave 8: W8DGHQSC, Wave 9: W9DGHQSC
# Standardise missing codes for these
clean_caseness <- function(x) {
  x <- as.numeric(x)
  x <- case_when(
    x == -92.0 ~ -9, # Refused (W2/W4)
    x == -9.0  ~ -9, # Refused (W8/W9)
    x == -8.0  ~ -8, # Insufficient info (W8/W9)
    x == -1.0  ~ -1, # Not applicable (W8/W9)
    x == -99.0 ~ -3, # Not interviewed (W2/W4)
    x == -97.0 ~ -7, # Refused (W2/W4)
    x == -96.0 ~ -2, # Interpreter (W2/W4)
    TRUE ~ x
  )
  x[is.na(x)] <- -3
  return(x)
}

full_frame <- full_frame %>%
  mutate(
    ghq15 = clean_caseness(W2ghq12scr),
    ghq17 = clean_caseness(W4ghq12scr),
    ghq25 = clean_caseness(W8DGHQSC),
    ghq32 = clean_caseness(W9DGHQSC)
  )

# Handle NAs in summed variables (convert to -3 as per general guidance)
full_frame <- full_frame %>%
  mutate(across(c(ghqtl15, ghqtl17, ghqtl25, ghqtl32), ~if_else(is.na(.x), -3, .x)))

# Select final variables
final_data <- full_frame %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

write_csv(final_data, 'data/output/cleaned_data.csv')
