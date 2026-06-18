library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols())
wave8_self <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = readr::cols())
wave8_der <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols())
wave9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = readr::cols())
wave9_der <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols())

# Merge datasets
data <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8_self, by = 'NSID') %>%
  full_join(wave8_der, by = 'NSID') %>%
  full_join(wave9_main, by = 'NSID') %>%
  full_join(wave9_der, by = 'NSID')

# Helper for item-summed variables
calculate_ghq_sum <- function(vars) {
  # vars is a character vector of column names
  # Use rowwise for calculation
  res <- apply(data[, vars], 1, function(row) {
    vals <- as.numeric(row)
    if (all(is.na(vals))) return(-3)
    if (any(vals < 0, na.rm = TRUE)) return(-8)
    if (any(is.na(vals))) return(-8) # Treating any NA in the set as insufficient info if not all are NA
    return(sum(vals))
  })
  return(res)
}

# --- Age 15 (Wave 2) ---
vars15 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')
data$ghqtl15 <- calculate_ghq_sum(vars15)

data <- data %>%
  mutate(ghq15 = case_when(
    is.na(W2ghq12scr) ~ -3,
    W2ghq12scr == -97 | W2ghq12scr == -92 ~ -9,
    W2ghq12scr == -99 ~ -3,
    W2ghq12scr == -91 ~ -1,
    W2ghq12scr < 0 ~ -2, # Default for other negative codes like -998, -997
    TRUE ~ W2ghq12scr
  ))

# --- Age 17 (Wave 4) ---
vars17 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')
data$ghqtl17 <- calculate_ghq_sum(vars17)

data <- data %>%
  mutate(ghq17 = case_when(
    is.na(W4ghq12scr) ~ -3,
    W4ghq12scr == -97 | W4ghq12scr == -92 ~ -9,
    W4ghq12scr == -99 ~ -3,
    W4ghq12scr == -91 ~ -1,
    W4ghq12scr < 0 ~ -2,
    TRUE ~ W4ghq12scr
  ))

# --- Age 25 (Wave 8) ---
vars25 <- paste0('W8GHQ12_', 1:12)
data$ghqtl25 <- calculate_ghq_sum(vars25)

data <- data %>%
  mutate(ghq25 = case_when(
    is.na(W8DGHQSC) ~ -3,
    W8DGHQSC == -9 ~ -9,
    W8DGHQSC == -8 ~ -8,
    W8DGHQSC == -1 ~ -1,
    TRUE ~ W8DGHQSC
  ))

# --- Age 32 (Wave 9) ---
vars32 <- paste0('W9GHQ12_', 1:12)
data$ghqtl32 <- calculate_ghq_sum(vars32)

data <- data %>%
  mutate(ghq32 = case_when(
    is.na(W9DGHQSC) ~ -3,
    W9DGHQSC == -9 ~ -9,
    W9DGHQSC == -8 ~ -8,
    W9DGHQSC == -1 ~ -1,
    TRUE ~ W9DGHQSC
  ))

# Select final variables
final_data <- data %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

write_csv(final_data, 'data/output/cleaned_data.csv')