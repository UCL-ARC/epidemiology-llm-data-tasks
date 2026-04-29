library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge datasets
data <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Wave 1 (Age 14) - W1GrsswkHH
data <- data %>%
  mutate(
    W1GrsswkHH = case_when(
      W1GrsswkHH == -999 ~ -2,
      W1GrsswkHH == -992 ~ -9,
      W1GrsswkHH == -99 ~ -3,
      W1GrsswkHH == -94 ~ -8,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -3 ~ -1,
      W1GrsswkHH == -1 ~ -8,
      TRUE ~ W1GrsswkHH
    ),
    W1GrsswkHH = ifelse(is.na(W1GrsswkHH), -3, W1GrsswkHH),
    incwhhcnt14 = case_when(
      W1GrsswkHH == 1 ~ 24.5,
      W1GrsswkHH == 2 ~ 74.5,
      W1GrsswkHH == 3 ~ 149.5,
      W1GrsswkHH == 4 ~ 249.5,
      W1GrsswkHH == 5 ~ 349.5,
      W1GrsswkHH == 6 ~ 449.5,
      W1GrsswkHH == 7 ~ 549.5,
      W1GrsswkHH == 8 ~ 649.5,
      W1GrsswkHH == 9 ~ 749.5,
      W1GrsswkHH == 10 ~ 849.5,
      W1GrsswkHH == 11 ~ 949.5,
      W1GrsswkHH == 12 ~ 1050,
      W1GrsswkHH %in% c(-9, -8, -1, -3, -2) ~ W1GrsswkHH,
      TRUE ~ NA_real_
    ),
    incwhh14 = factor(
      W1GrsswkHH,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -1, -3, -2),
      labels = c(
        'Up to £49', '£50 up to £99', '£100 up to £199', '£200 up to £299',
        '£300 up to £399', '£400 up to £499', '£500 up to £599', '£600 up to £699',
        '£700 up to £799', '£800 up to £899', '£900 up to £999', '£1,000 or more',
        'Refusal', "Don't know", 'Item not applicable', 'Not asked', 'Script error'
      )
    )
  )

# Wave 2 (Age 15) - W2GrsswkHH
data <- data %>%
  mutate(
    W2GrsswkHH = case_when(
      W2GrsswkHH == -999 ~ -2,
      W2GrsswkHH == -992 ~ -9,
      W2GrsswkHH == -99 ~ -3,
      W2GrsswkHH == -94 ~ -8,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -3 ~ -1,
      W2GrsswkHH == -1 ~ -8,
      TRUE ~ W2GrsswkHH
    ),
    W2GrsswkHH = ifelse(is.na(W2GrsswkHH), -3, W2GrsswkHH),
    incwhhcnt15 = case_when(
      W2GrsswkHH == 1 ~ 24.5,
      W2GrsswkHH == 2 ~ 74.5,
      W2GrsswkHH == 3 ~ 149.5,
      W2GrsswkHH == 4 ~ 249.5,
      W2GrsswkHH == 5 ~ 349.5,
      W2GrsswkHH == 6 ~ 449.5,
      W2GrsswkHH == 7 ~ 549.5,
      W2GrsswkHH == 8 ~ 649.5,
      W2GrsswkHH == 9 ~ 749.5,
      W2GrsswkHH == 10 ~ 849.5,
      W2GrsswkHH == 11 ~ 949.5,
      W2GrsswkHH == 12 ~ 1050,
      W2GrsswkHH %in% c(-9, -8, -1, -3, -2) ~ W2GrsswkHH,
      TRUE ~ NA_real_
    ),
    incwhh15 = factor(
      W2GrsswkHH,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -1, -3, -2),
      labels = c(
        'Up to £49', '£50 up to £99', '£100 up to £199', '£200 up to £299',
        '£300 up to £399', '£400 up to £499', '£500 up to £599', '£600 up to £699',
        '£700 up to £799', '£800 up to £899', '£900 up to £999', '£1,000 or more',
        'Refusal', "Don't know", 'Item not applicable', 'Not asked', 'Script error'
      )
    )
  )

# Wave 3 (Age 16) - W3incestw
data <- data %>%
  mutate(
    W3incestw = case_when(
      W3incestw == -99 ~ -3,
      W3incestw == -92 ~ -9,
      W3incestw == -1 ~ -8,
      TRUE ~ W3incestw
    ),
    W3incestw = ifelse(is.na(W3incestw), -3, W3incestw),
    incwhh16 = factor(
      W3incestw,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -3),
      labels = c(
        'Up to £49', '£50 up to £99', '£100 up to £199', '£200 up to £299',
        '£300 up to £399', '£400 up to £499', '£500 up to £599', '£600 up to £699',
        '£700 up to £799', '£800 up to £899', '£900 up to £999', '£1,000 or more',
        'Refusal', "Don't know", 'Not asked'
      )
    )
  )

# Wave 4 (Age 17) - w4IncEstW
data <- data %>%
  mutate(
    w4IncEstW = case_when(
      w4IncEstW == -996 ~ -3,
      w4IncEstW == -99 ~ -3,
      w4IncEstW == -92 ~ -9,
      w4IncEstW == -1 ~ -8,
      TRUE ~ w4IncEstW
    ),
    w4IncEstW = ifelse(is.na(w4IncEstW), -3, w4IncEstW),
    incwhh17 = factor(
      w4IncEstW,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -3),
      labels = c(
        'Up to £49', '£50 up to £99', '£100 up to £199', '£200 up to £299',
        '£300 up to £399', '£400 up to £499', '£500 up to £599', '£600 up to £699',
        '£700 up to £799', '£800 up to £899', '£900 up to £999', '£1,000 or more',
        'Refusal', "Don't know", 'Not asked'
      )
    )
  )

# Select required variables
final_data <- data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')