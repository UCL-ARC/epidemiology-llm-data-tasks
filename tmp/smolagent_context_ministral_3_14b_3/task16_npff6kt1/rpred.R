
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
file_paths <- list(
  wave1 = "data/input/wave_one_lsype_family_background_2020.tab",
  wave2 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave3 = "data/input/wave_three_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load datasets
wave1 <- read_delim(file_paths$wave1, delim = "\t")
wave2 <- read_delim(file_paths$wave2, delim = "\t")
wave3 <- read_delim(file_paths$wave3, delim = "\t")
wave4 <- read_delim(file_paths$wave4, delim = "\t")

# Vectorized function to band continuous income values
band_income <- function(inc) {
  cuts <- c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf)
  labels <- c(1:12)
  cut(inc, cuts, labels = labels, right = FALSE)
}

# Process wave 1 (age 14)
wave1 <- wave1 %>%
  mutate(
    incwhhcnt14 = W1GrsswkHH,
    incwhh14 = ifelse(!is.na(W1GrsswkHH), band_income(W1GrsswkHH), NA_integer_)
  ) %>%
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH %in% c(-3, -999) ~ -3,
      W1GrsswkHH == -992 ~ -9,
      W1GrsswkHH == -99 ~ -3,
      W1GrsswkHH == -94 ~ -8,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -1 ~ -8,
      TRUE ~ incwhhcnt14
    ),
    incwhh14 = case_when(
      W1GrsswkHH %in% c(-3, -999) ~ -3,
      W1GrsswkHH == -992 ~ -9,
      W1GrsswkHH == -99 ~ -3,
      W1GrsswkHH == -94 ~ -8,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -1 ~ -8,
      TRUE ~ incwhh14
    )
  )

# Process wave 2 (age 15)
wave2 <- wave2 %>%
  mutate(
    incwhhcnt15 = W2GrsswkHH,
    incwhh15 = ifelse(!is.na(W2GrsswkHH), band_income(W2GrsswkHH), NA_integer_)
  ) %>%
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH %in% c(-3, -999) ~ -3,
      W2GrsswkHH == -992 ~ -9,
      W2GrsswkHH == -99 ~ -3,
      W2GrsswkHH == -94 ~ -8,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -1 ~ -8,
      TRUE ~ incwhhcnt15
    ),
    incwhh15 = case_when(
      W2GrsswkHH %in% c(-3, -999) ~ -3,
      W2GrsswkHH == -992 ~ -9,
      W2GrsswkHH == -99 ~ -3,
      W2GrsswkHH == -94 ~ -8,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -1 ~ -8,
      TRUE ~ incwhh15
    )
  )

# Process wave 3 (age 16)
wave3 <- wave3 %>%
  mutate(
    incwhh16 = W3incestw
  ) %>%
  mutate(
    incwhh16 = case_when(
      W3incestw %in% c(-999, -996) ~ -3,
      W3incestw == -992 ~ -9,
      W3incestw == -99 ~ -3,
      W3incestw == -94 ~ -8,
      W3incestw == -92 ~ -9,
      W3incestw == -91 ~ -1,
      W3incestw == -1 ~ -8,
      TRUE ~ incwhh16
    )
  )

# Process wave 4 (age 17)
wave4 <- wave4 %>%
  mutate(
    incwhh17 = w4IncEstW
  ) %>%
  mutate(
    incwhh17 = case_when(
      w4IncEstW %in% c(-999, -996) ~ -3,
      w4IncEstW == -992 ~ -9,
      w4IncEstW == -99 ~ -3,
      w4IncEstW == -94 ~ -8,
      w4IncEstW == -92 ~ -9,
      w4IncEstW == -91 ~ -1,
      w4IncEstW == -1 ~ -8,
      TRUE ~ incwhh17
    )
  )

# Define income band labels
inc_band_labels <- c(
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
  '12' = '£1,000 or more',
  '-9' = 'Refused',
  '-8' = 'Insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not interviewed',
  '-2' = 'Schedule not applicable',
  '-1' = "Don't know"
)

# Convert banded variables to factors with labels
wave1 <- wave1 %>%
  mutate(incwhh14 = factor(incwhh14, levels = c(-9, -8, -7, -3, -2, -1, 1:12), labels = inc_band_labels))
wave2 <- wave2 %>%
  mutate(incwhh15 = factor(incwhh15, levels = c(-9, -8, -7, -3, -2, -1, 1:12), labels = inc_band_labels))
wave3 <- wave3 %>%
  mutate(incwhh16 = factor(incwhh16, levels = c(-9, -8, -7, -3, -2, -1, 1:12), labels = inc_band_labels))
wave4 <- wave4 %>%
  mutate(incwhh17 = factor(incwhh17, levels = c(-9, -8, -7, -3, -2, -1, 1:12), labels = inc_band_labels))

# Merge datasets
cleaned_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Ensure all required variables are present
required_vars <- c("NSID", "incwhhcnt14", "incwhhcnt15", "incwhh14", "incwhh15", "incwhh16", "incwhh17")

# Remove intermediate variables
cleaned_data <- cleaned_data %>%
  select(all_of(required_vars))

# Write cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
