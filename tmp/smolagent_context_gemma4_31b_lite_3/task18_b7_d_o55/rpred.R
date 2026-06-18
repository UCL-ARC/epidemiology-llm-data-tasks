library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

load_file <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = 'numeric'))
}

# Correcting col_types for NSID which is string
load_file_with_nsid <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'numeric'))
}

# Load files
all_data <- list(
  w1 = load_file_with_nsid('wave_one_lsype_young_person_2020.tab'),
  w2 = load_file_with_nsid('wave_two_lsype_young_person_2020.tab'),
  w4 = load_file_with_nsid('wave_four_lsype_young_person_2020.tab'),
  w8_sc = load_file_with_nsid('ns8_2015_self_completion.tab'),
  w8_dr = load_file_with_nsid('ns8_2015_derived.tab'),
  w9_mi = load_file_with_nsid('ns9_2022_main_interview.tab'),
  w9_dr = load_file_with_nsid('ns9_2022_derived_variables.tab')
)

# Merge datasets
full_frame <- all_data$w1 %>%
  full_join(all_data$w2, by = 'NSID') %>%
  full_join(all_data$w4, by = 'NSID') %>%
  full_join(all_data$w8_sc, by = 'NSID') %>%
  full_join(all_data$w8_dr, by = 'NSID') %>%
  full_join(all_data$w9_mi, by = 'NSID') %>%
  full_join(all_data$w9_dr, by = 'NSID')

# Function to map missing values based on general guidance
map_missing <- function(x, labels_map = NULL) {
  # Default mapping based on label meanings from metadata
  # -99.0 (YP not interviewed) -> -3
  # -97.0 (YP refused self completion) -> -7
  # -96.0 (YP using interpreter) -> -2
  # -92.0 (Refused) -> -9
  # -91.0 (Not applicable) -> -1
  # -1.0 (Don't Know) -> -8
  # -998, -997, -995 (Missed/Script Error) -> -2
  
  res <- x
  res[x == -99.0] <- -3
  res[x == -97.0] <- -7
  res[x == -96.0] <- -2
  res[x == -92.0] <- -9
  res[x == -91.0] <- -1
  res[x == -1.0] <- -8
  res[x == -998.0 | x == -997.0 | x == -995.0] <- -2
  res[is.na(x)] <- -3
  return(res)
}

# GHQ-12 Summed Scores
# Wave 2 (Age 15)
w2_vars <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')
full_frame <- full_frame %>%
  mutate(ghqtl15 = rowSums(across(all_of(w2_vars), ~ ifelse(. >= 0, ., NA)), na.rm = TRUE) %>%
    ifelse(rowSums(!is.na(across(all_of(w2_vars), ~ ifelse(. >= 0, ., NA)))), ., -3))

# Wave 4 (Age 17)
w4_vars <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')
full_frame <- full_frame %>%
  mutate(ghqtl17 = rowSums(across(all_of(w4_vars), ~ ifelse(. >= 0, ., NA)), na.rm = TRUE) %>%
    ifelse(rowSums(!is.na(across(all_of(w4_vars), ~ ifelse(. >= 0, ., NA)))), ., -3))

# Wave 8 (Age 25)
w8_vars <- paste0('W8GHQ12_', 1:12)
full_frame <- full_frame %>%
  mutate(ghqtl25 = rowSums(across(all_of(w8_vars), ~ ifelse(. >= 0, ., NA)), na.rm = TRUE) %>%
    ifelse(rowSums(!is.na(across(all_of(w8_vars), ~ ifelse(. >= 0, ., NA)))), ., -3))

# Wave 9 (Age 32)
w9_vars <- paste0('W9GHQ12_', 1:12)
full_frame <- full_frame %>%
  mutate(ghqtl32 = rowSums(across(all_of(w9_vars), ~ ifelse(. >= 0, ., NA)), na.rm = TRUE) %>%
    ifelse(rowSums(!is.na(across(all_of(w9_vars), ~ ifelse(. >= 0, ., NA)))), ., -3))

# GHQ-12 Caseness scores (pre-derived)
full_frame <- full_frame %>%
  mutate(
    ghq15 = map_missing(W2ghq12scr),
    ghq17 = map_missing(W4ghq12scr),
    ghq25 = map_missing(W8DGHQSC),
    ghq32 = map_missing(W9DGHQSC)
  )

# Final selection
final_data <- full_frame %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

readr::write_csv(final_data, 'data/output/cleaned_data.csv')
