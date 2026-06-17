library(readr)
library(dplyr)
library(haven)

# Load each dataset explicitly
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define the banding function for continuous income values
band_income <- function(income) {
  case_when(
    income < 50 ~ 1,
    income >= 50 & income < 100 ~ 2,
    income >= 100 & income < 200 ~ 3,
    income >= 200 & income < 300 ~ 4,
    income >= 300 & income < 400 ~ 5,
    income >= 400 & income < 500 ~ 6,
    income >= 500 & income < 600 ~ 7,
    income >= 600 & income < 700 ~ 8,
    income >= 700 & income < 800 ~ 9,
    income >= 800 & income < 900 ~ 10,
    income >= 900 & income < 1000 ~ 11,
    income >= 1000 ~ 12,
    TRUE ~ NA_real_
  )
}

# Apply missing value mappings and banding for each wave
# Wave 1 (Age 14)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH == -992 ~ -9,
      W1GrsswkHH == -99 ~ -3,
      W1GrsswkHH == -94 ~ -2,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -3 ~ -1,
      W1GrsswkHH == -1 ~ -8,
      W1GrsswkHH > 0 ~ W1GrsswkHH,
      TRUE ~ -3
    ),
    incwhh14 = case_when(
      incwhhcnt14 == -9 ~ -9,
      incwhhcnt14 == -8 ~ -8,
      incwhhcnt14 == -7 ~ -7,
      incwhhcnt14 == -3 ~ -3,
      incwhhcnt14 == -2 ~ -2,
      incwhhcnt14 == -1 ~ -1,
      incwhhcnt14 > 0 ~ band_income(incwhhcnt14),
      TRUE ~ -3
    )
  )

# Wave 2 (Age 15)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH == -992 ~ -9,
      W2GrsswkHH == -99 ~ -3,
      W2GrsswkHH == -94 ~ -2,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -3 ~ -1,
      W2GrsswkHH == -1 ~ -8,
      W2GrsswkHH > 0 ~ W2GrsswkHH,
      TRUE ~ -3
    ),
    incwhh15 = case_when(
      incwhhcnt15 == -9 ~ -9,
      incwhhcnt15 == -8 ~ -8,
      incwhhcnt15 == -7 ~ -7,
      incwhhcnt15 == -3 ~ -3,
      incwhhcnt15 == -2 ~ -2,
      incwhhcnt15 == -1 ~ -1,
      incwhhcnt15 > 0 ~ band_income(incwhhcnt15),
      TRUE ~ -3
    )
  )

# Wave 3 (Age 16)
merged_data <- merged_data %>%
  mutate(
    incwhh16 = case_when(
      W3incestw == -99 ~ -3,
      W3incestw == -92 ~ -9,
      W3incestw == -1 ~ -8,
      W3incestw >= 1 & W3incestw <= 12 ~ W3incestw,
      TRUE ~ -3
    )
  )

# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(
    incwhh17 = case_when(
      w4IncEstW == -996 ~ -3,
      w4IncEstW == -99 ~ -3,
      w4IncEstW == -92 ~ -9,
      w4IncEstW == -1 ~ -8,
      w4IncEstW >= 1 & w4IncEstW <= 12 ~ w4IncEstW,
      TRUE ~ -3
    )
  )

# Select only the required variables
cleaned_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

# Write the output CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"