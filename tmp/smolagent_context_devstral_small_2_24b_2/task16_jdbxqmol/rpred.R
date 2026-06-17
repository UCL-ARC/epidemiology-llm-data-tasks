library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>% 
  full_join(wave2, by = "NSID") %>% 
  full_join(wave3, by = "NSID") %>% 
  full_join(wave4, by = "NSID")

# Process wave1 (age 14)
merged_data <- merged_data %>% 
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH == -3 ~ -1,  # "Not yet paid" → -1
      W1GrsswkHH == -1 ~ -8,  # "Don't know" → -8
      W1GrsswkHH == -992 ~ -9,  # "No information - refused" → -9
      W1GrsswkHH == -99 ~ -3,  # "HH not interviewed" → -3
      W1GrsswkHH == -94 ~ -3,  # "Insufficent information" → -3
      W1GrsswkHH == -92 ~ -9,  # "Refused" → -9
      W1GrsswkHH == -91 ~ -1,  # "Not applicable" → -1
      W1GrsswkHH == -999 ~ -2,  # "Missing in error" → -2
      TRUE ~ W1GrsswkHH
    )
  ) %>% 
  mutate(
    incwhhcnt14 = ifelse(is.na(incwhhcnt14), -3, incwhhcnt14),
    incwhh14 = case_when(
      incwhhcnt14 < 0 ~ incwhhcnt14,
      incwhhcnt14 < 50 ~ 1,
      incwhhcnt14 < 100 ~ 2,
      incwhhcnt14 < 200 ~ 3,
      incwhhcnt14 < 300 ~ 4,
      incwhhcnt14 < 400 ~ 5,
      incwhhcnt14 < 500 ~ 6,
      incwhhcnt14 < 600 ~ 7,
      incwhhcnt14 < 700 ~ 8,
      incwhhcnt14 < 800 ~ 9,
      incwhhcnt14 < 900 ~ 10,
      incwhhcnt14 < 1000 ~ 11,
      TRUE ~ 12
    )
  )

# Process wave2 (age 15)
merged_data <- merged_data %>% 
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH == -3 ~ -1,  # "Not yet paid" → -1
      W2GrsswkHH == -1 ~ -8,  # "Don't know" → -8
      W2GrsswkHH == -992 ~ -9,  # "No information - refused" → -9
      W2GrsswkHH == -99 ~ -3,  # "HH not interviewed" → -3
      W2GrsswkHH == -94 ~ -3,  # "insufficient information" → -3
      W2GrsswkHH == -92 ~ -9,  # "Refused" → -9
      W2GrsswkHH == -91 ~ -1,  # "Not applicable" → -1
      W2GrsswkHH == -999 ~ -2,  # "Missing in error" → -2
      TRUE ~ W2GrsswkHH
    )
  ) %>% 
  mutate(
    incwhhcnt15 = ifelse(is.na(incwhhcnt15), -3, incwhhcnt15),
    incwhh15 = case_when(
      incwhhcnt15 < 0 ~ incwhhcnt15,
      incwhhcnt15 < 50 ~ 1,
      incwhhcnt15 < 100 ~ 2,
      incwhhcnt15 < 200 ~ 3,
      incwhhcnt15 < 300 ~ 4,
      incwhhcnt15 < 400 ~ 5,
      incwhhcnt15 < 500 ~ 6,
      incwhhcnt15 < 600 ~ 7,
      incwhhcnt15 < 700 ~ 8,
      incwhhcnt15 < 800 ~ 9,
      incwhhcnt15 < 900 ~ 10,
      incwhhcnt15 < 1000 ~ 11,
      TRUE ~ 12
    )
  )

# Process wave3 (age 16)
merged_data <- merged_data %>% 
  mutate(
    incwhh16 = case_when(
      W3incestw == -99 ~ -3,  # "MP not interviewed" → -3
      W3incestw == -92 ~ -9,  # "Refused" → -9
      W3incestw == -1 ~ -8,  # "Don't know" → -8
      TRUE ~ W3incestw
    )
  ) %>% 
  mutate(
    incwhh16 = ifelse(is.na(incwhh16), -3, incwhh16)
  )

# Process wave4 (age 17)
merged_data <- merged_data %>% 
  mutate(
    incwhh17 = case_when(
      w4IncEstW == -996 ~ -3,  # "No parent in household" → -3
      w4IncEstW == -99 ~ -3,  # "MP not interviewed" → -3
      w4IncEstW == -92 ~ -9,  # "Refused" → -9
      w4IncEstW == -1 ~ -8,  # "Don't know" → -8
      TRUE ~ w4IncEstW
    )
  ) %>% 
  mutate(
    incwhh17 = ifelse(is.na(incwhh17), -3, incwhh17)
  )

# Select only the required variables
output_data <- merged_data %>% 
  select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"