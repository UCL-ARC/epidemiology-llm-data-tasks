
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Step 1: Load datasets
wave_one <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Convert NSID to character to ensure proper joining
wave_one$NSID <- as.character(wave_one$NSID)
wave_two$NSID <- as.character(wave_two$NSID)
wave_three$NSID <- as.character(wave_three$NSID)
wave_four$NSID <- as.character(wave_four$NSID)

# Step 2: Merge datasets
merged_data <- full_join(wave_one, wave_two, by = 'NSID') %>%
  full_join(wave_three, by = 'NSID') %>%
  full_join(wave_four, by = 'NSID')

# Step 3: Define missing value mapping
map_missing_codes <- function(x) {
  x %>%
    mutate(
      W1GrsswkHH = case_when(
        W1GrsswkHH %in% c(-999, -992) ~ -3,
        W1GrsswkHH == -99 ~ -3,
        W1GrsswkHH == -94 ~ -8,
        W1GrsswkHH == -92 ~ -9,
        W1GrsswkHH == -91 ~ -1,
        W1GrsswkHH == -3 ~ -1,
        W1GrsswkHH == -1 ~ -1,
        is.na(W1GrsswkHH) ~ -3,
        TRUE ~ as.numeric(W1GrsswkHH)
      ),
      W2GrsswkHH = case_when(
        W2GrsswkHH %in% c(-999, -992) ~ -3,
        W2GrsswkHH == -99 ~ -3,
        W2GrsswkHH == -94 ~ -8,
        W2GrsswkHH == -92 ~ -9,
        W2GrsswkHH == -91 ~ -1,
        W2GrsswkHH == -3 ~ -1,
        W2GrsswkHH == -1 ~ -1,
        is.na(W2GrsswkHH) ~ -3,
        TRUE ~ as.numeric(W2GrsswkHH)
      ),
      W3incestw = case_when(
        W3incestw == -99 ~ -3,
        W3incestw == -92 ~ -9,
        W3incestw == -1 ~ -1,
        is.na(W3incestw) ~ -3,
        TRUE ~ as.numeric(W3incestw)
      ),
      w4IncEstW = case_when(
        w4IncEstW == -996 ~ -3,
        w4IncEstW == -99 ~ -3,
        w4IncEstW == -92 ~ -9,
        w4IncEstW == -1 ~ -1,
        is.na(w4IncEstW) ~ -3,
        TRUE ~ as.numeric(w4IncEstW)
      )
    )
}

# Step 4: Map missing values
merged_data <- map_missing_codes(merged_data)

# Step 5: Rename variables and create banded variables
merged_data <- merged_data %>%
  rename(
    incwhhcnt14 = W1GrsswkHH,
    incwhhcnt15 = W2GrsswkHH,
    incwhh16 = W3incestw,
    incwhh17 = w4IncEstW
  ) %>%
  mutate(
    incwhh14 = case_when(
      incwhhcnt14 %in% c(-9, -8, -3, -2, -1) ~ NA_integer_,
      incwhhcnt14 > 0 & incwhhcnt14 <= 49 ~ 1,
      incwhhcnt14 > 49 & incwhhcnt14 <= 99 ~ 2,
      incwhhcnt14 > 99 & incwhhcnt14 <= 199 ~ 3,
      incwhhcnt14 > 199 & incwhhcnt14 <= 299 ~ 4,
      incwhhcnt14 > 299 & incwhhcnt14 <= 399 ~ 5,
      incwhhcnt14 > 399 & incwhhcnt14 <= 499 ~ 6,
      incwhhcnt14 > 499 & incwhhcnt14 <= 599 ~ 7,
      incwhhcnt14 > 599 & incwhhcnt14 <= 699 ~ 8,
      incwhhcnt14 > 699 & incwhhcnt14 <= 799 ~ 9,
      incwhhcnt14 > 799 & incwhhcnt14 <= 899 ~ 10,
      incwhhcnt14 > 899 & incwhhcnt14 <= 999 ~ 11,
      incwhhcnt14 >= 1000 ~ 12,
      TRUE ~ NA_integer_
    ),
    incwhh15 = case_when(
      incwhhcnt15 %in% c(-9, -8, -3, -2, -1) ~ NA_integer_,
      incwhhcnt15 > 0 & incwhhcnt15 <= 49 ~ 1,
      incwhhcnt15 > 49 & incwhhcnt15 <= 99 ~ 2,
      incwhhcnt15 > 99 & incwhhcnt15 <= 199 ~ 3,
      incwhhcnt15 > 199 & incwhhcnt15 <= 299 ~ 4,
      incwhhcnt15 > 299 & incwhhcnt15 <= 399 ~ 5,
      incwhhcnt15 > 399 & incwhhcnt15 <= 499 ~ 6,
      incwhhcnt15 > 499 & incwhhcnt15 <= 599 ~ 7,
      incwhhcnt15 > 599 & incwhhcnt15 <= 699 ~ 8,
      incwhhcnt15 > 699 & incwhhcnt15 <= 799 ~ 9,
      incwhhcnt15 > 799 & incwhhcnt15 <= 899 ~ 10,
      incwhhcnt15 > 899 & incwhhcnt15 <= 999 ~ 11,
      incwhhcnt15 >= 1000 ~ 12,
      TRUE ~ NA_integer_
    )
  )

# Step 6: Convert banded variables to factors
merged_data <- merged_data %>%
  mutate(
    incwhh14 = factor(incwhh14, levels = c(-9, -8, -3, -2, -1, 1:12)),
    incwhh15 = factor(incwhh15, levels = c(-9, -8, -3, -2, -1, 1:12)),
    incwhh16 = factor(incwhh16, levels = c(-9, -8, -3, -2, -1, 1:12)),
    incwhh17 = factor(incwhh17, levels = c(-9, -8, -3, -2, -1, 1:12))
  )

# Step 7: Select required variables
cleaned_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Step 8: Write output
write_csv(cleaned_data, 'data/output/cleaned_data.csv')
