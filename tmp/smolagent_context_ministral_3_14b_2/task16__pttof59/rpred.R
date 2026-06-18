
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load data with explicit column types
wave1 <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t',
                           col_types = cols(NSID = col_character(), W1GrsswkHH = col_double()))
wave2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t',
                           col_types = cols(NSID = col_character(), W2GrsswkHH = col_double()))
wave3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t',
                           col_types = cols(NSID = col_character(), W3incestw = col_double()))
wave4 <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t',
                           col_types = cols(NSID = col_character(), w4IncEstW = col_double()))

# Define missing value mapping
map_missing_values <- function(x, wave) {
  x <- as.numeric(x)
  if (wave %in% c(1, 2)) {
    x[is.na(x)] <- -3
    x[x == -999] <- -3
    x[x == -99] <- -3
    x[x == -94] <- -3
    x[x == -91] <- -3
    x[x == -92] <- -9
    x[x == -2] <- -3
    x[x == -3] <- -1
    x[x == -1] <- -8
    x[x == -992] <- -9
  } else if (wave == 4) {
    x[is.na(x)] <- -3
    x[x == -999] <- -3
    x[x == -99] <- -3
    x[x == -92] <- -3
    x[x == -91] <- -3
    x[x == -996] <- -3
  } else if (wave == 3) {
    x[is.na(x)] <- -3
    x[x == -999] <- -3
    x[x == -99] <- -3
    x[x == -92] <- -3
    x[x == -91] <- -3
  }
  return(x)
}

# Define income banding function
band_income <- function(x) {
  x <- as.numeric(x)
  cut(x,
      breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
      labels = FALSE,
      right = TRUE,
      include.lowest = TRUE
  )
}

# Process wave 1 (age 14)
wave1 <- wave1 %>%
  mutate(incwhhcnt14 = map_missing_values(W1GrsswkHH, 1)) %>%
  mutate(incwhh14 = case_when(
    incwhhcnt14 >= 1 & incwhhcnt14 <= 49 ~ 1,
    incwhhcnt14 >= 50 & incwhhcnt14 <= 99 ~ 2,
    incwhhcnt14 >= 100 & incwhhcnt14 <= 199 ~ 3,
    incwhhcnt14 >= 200 & incwhhcnt14 <= 299 ~ 4,
    incwhhcnt14 >= 300 & incwhhcnt14 <= 399 ~ 5,
    incwhhcnt14 >= 400 & incwhhcnt14 <= 499 ~ 6,
    incwhhcnt14 >= 500 & incwhhcnt14 <= 599 ~ 7,
    incwhhcnt14 >= 600 & incwhhcnt14 <= 699 ~ 8,
    incwhhcnt14 >= 700 & incwhhcnt14 <= 799 ~ 9,
    incwhhcnt14 >= 800 & incwhhcnt14 <= 899 ~ 10,
    incwhhcnt14 >= 900 & incwhhcnt14 <= 999 ~ 11,
    incwhhcnt14 >= 1000 ~ 12,
    TRUE ~ NA_integer_
  )) %>%
  mutate(across(c(incwhhcnt14, incwhh14), ~ifelse(is.na(.), -3, .)))

# Process wave 2 (age 15)
wave2 <- wave2 %>%
  mutate(incwhhcnt15 = map_missing_values(W2GrsswkHH, 2)) %>%
  mutate(incwhh15 = case_when(
    incwhhcnt15 >= 1 & incwhhcnt15 <= 49 ~ 1,
    incwhhcnt15 >= 50 & incwhhcnt15 <= 99 ~ 2,
    incwhhcnt15 >= 100 & incwhhcnt15 <= 199 ~ 3,
    incwhhcnt15 >= 200 & incwhhcnt15 <= 299 ~ 4,
    incwhhcnt15 >= 300 & incwhhcnt15 <= 399 ~ 5,
    incwhhcnt15 >= 400 & incwhhcnt15 <= 499 ~ 6,
    incwhhcnt15 >= 500 & incwhhcnt15 <= 599 ~ 7,
    incwhhcnt15 >= 600 & incwhhcnt15 <= 699 ~ 8,
    incwhhcnt15 >= 700 & incwhhcnt15 <= 799 ~ 9,
    incwhhcnt15 >= 800 & incwhhcnt15 <= 899 ~ 10,
    incwhhcnt15 >= 900 & incwhhcnt15 <= 999 ~ 11,
    incwhhcnt15 >= 1000 ~ 12,
    TRUE ~ NA_integer_
  )) %>%
  mutate(across(c(incwhhcnt15, incwhh15), ~ifelse(is.na(.), -3, .)))

# Process wave 3 (age 16)
wave3 <- wave3 %>%
  mutate(incwhh16 = map_missing_values(W3incestw, 3)) %>%
  mutate(across(incwhh16, ~ifelse(is.na(.), -3, .)))

# Process wave 4 (age 17)
wave4 <- wave4 %>%
  mutate(incwhh17 = map_missing_values(w4IncEstW, 4)) %>%
  mutate(across(incwhh17, ~ifelse(is.na(.), -3, .)))

# Create value labels
value_labels <- c(
  '1' = 'Up to £49',
  '2' = '£50 up to £99',
  '3' = '£100 up to £199',
  '4' = '£200 up to £299',
  '5' = '£300 up to £399',
  '6' = '£400 up to £499',
  '7' = '£500 up to £599',
  '8' = '£600 up to £699',
  '9' = '£700 up to £799',
  '10' = '£800 up to £899',
  '11' = '£900 up to £999',
  '12' = '£1,000 or more'
)

# Convert banded variables to factors with labels
wave1 <- wave1 %>%
  mutate(incwhh14 = factor(incwhh14, levels = 0:12, labels = c(NA_character_, value_labels)))

wave2 <- wave2 %>%
  mutate(incwhh15 = factor(incwhh15, levels = 0:12, labels = c(NA_character_, value_labels)))

wave3 <- wave3 %>%
  mutate(incwhh16 = factor(incwhh16, levels = 0:12, labels = c(NA_character_, value_labels)))

wave4 <- wave4 %>%
  mutate(incwhh17 = factor(incwhh17, levels = 0:12, labels = c(NA_character_, value_labels)))

# Merge all waves
cleaned_data <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  select(NSID, incwhhcnt14, incwhhcnt15, incwhh14, incwhh15, incwhh16, incwhh17)

# Write output
readr::write_csv(cleaned_data, 'data/output/cleaned_data.csv')
