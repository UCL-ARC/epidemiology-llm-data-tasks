library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define helper functions for missing value mapping
map_missing_standard <- function(x) {
  x <- as.numeric(x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(is.na(x), -3, x)
  return(x)
}

map_missing_region <- function(x) {
  x <- as.numeric(x)
  x <- ifelse(x == 13, -2, x)
  x <- ifelse(is.na(x), -3, x)
  return(x)
}

# Load and process Wave 2 (Age 15)
w2_raw <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w2 <- w2_raw %>% 
  mutate(
    regub15 = map_missing_standard(urbind),
    regov15 = map_missing_standard(gor)
  ) %>% 
  select(NSID, regub15, regov15)

# Load and process Wave 3 (Age 16)
w3_raw <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w3 <- w3_raw %>% 
  mutate(
    regub16 = map_missing_standard(urbind),
    regov16 = map_missing_standard(gor)
  ) %>% 
  select(NSID, regub16, regov16)

# Load and process Wave 8 (Age 25)
w8_raw <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w8 <- w8_raw %>% 
  mutate(regor25 = map_missing_region(W8DGOR)) %>% 
  select(NSID, regor25)

# Load and process Wave 9 Derived (Age 32)
w9d_raw <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w9d <- w9d_raw %>% 
  mutate(regor32 = map_missing_region(W9DRGN)) %>% 
  select(NSID, regor32)

# Load and process Wave 9 Main (Age 32)
w9m_raw <- readr::read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w9m <- w9m_raw %>% 
  mutate(regint32 = {
    val <- as.numeric(W9NATIONRES)
    res <- rep(-3, length(val))
    res[val >= 1 & val <= 4] <- 1
    res[val == 5] <- 2
    res[val == -9] <- -9
    res[val == -8] <- -8
    res[val == -3] <- -3
    res[val == -1] <- -1
    res
  }) %>% 
  select(NSID, regint32)

# Load cohort frame from Wave 1
w1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c')) %>% 
  select(NSID)

# Full join all processed datasets
final_output <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9d, by = 'NSID') %>%
  full_join(w9m, by = 'NSID')

# Ensure final NAs are converted to -3
final_output[is.na(final_output)] <- -3

# Write to CSV
readr::write_csv(final_output, 'data/output/cleaned_data.csv')
