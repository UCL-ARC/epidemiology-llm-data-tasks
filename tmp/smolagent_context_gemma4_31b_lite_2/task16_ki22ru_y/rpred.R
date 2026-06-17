library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Convert target variables to numeric
wave1$W1GrsswkHH <- as.numeric(wave1$W1GrsswkHH)
wave2$W2GrsswkHH <- as.numeric(wave2$W2GrsswkHH)
wave3$W3incestw <- as.numeric(wave3$W3incestw)
wave4$w4IncEstW <- as.numeric(wave4$w4IncEstW)

# Merge datasets
df <- wave1 %>% 
  full_join(wave2, by = 'NSID') %>% 
  full_join(wave3, by = 'NSID') %>% 
  full_join(wave4, by = 'NSID')

# Harmonisation function for missing values
harmonise_missing <- function(val) {
  res <- case_when(
    is.na(val) ~ -3,
    val == -92.0 ~ -9,
    val == -1.0 ~ -8,
    val == -94.0 ~ -8,
    val == -99.0 ~ -3,
    val == -91.0 ~ -1,
    val == -999.0 ~ -2,
    val == -992.0 ~ -2,
    val == -3.0 ~ -1, 
    val == -996.0 ~ -1, 
    TRUE ~ val
  )
  return(res)
}

# Process continuous variables for Age 14 and 15
df <- df %>%
  mutate(
    hhinc14 = harmonise_missing(W1GrsswkHH),
    hhinc15 = harmonise_missing(W2GrsswkHH)
  )

# Process banded variables
df <- df %>%
  mutate(
    hhincband16 = case_when(
      is.na(W3incestw) ~ -3,
      W3incestw == -92.0 ~ -9,
      W3incestw == -1.0 ~ -8,
      W3incestw == -99.0 ~ -3,
      W3incestw >= 1 & W3incestw <= 12 ~ W3incestw,
      TRUE ~ -3
    ),
    hhincband17 = case_when(
      is.na(w4IncEstW) ~ -3,
      w4IncEstW == -92.0 ~ -9,
      w4IncEstW == -1.0 ~ -8,
      w4IncEstW == -99.0 ~ -3,
      w4IncEstW == -996.0 ~ -1,
      w4IncEstW >= 1 & w4IncEstW <= 12 ~ w4IncEstW,
      TRUE ~ -3
    )
  )

# For age 14 and 15, we use the continuous values since no band mapping is provided in metadata
df <- df %>%
  mutate(
    hhincband14 = hhinc14,
    hhincband15 = hhinc15
  )

# Final variable selection
final_df <- df %>%
  select(NSID, hhinc14, hhinc15, hhincband14, hhincband15, hhincband16, hhincband17)

write_csv(final_df, 'data/output/cleaned_data.csv')