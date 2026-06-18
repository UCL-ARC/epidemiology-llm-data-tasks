library(dplyr)
library(readr)
library(haven)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Function to harmonize missing values
harmonize_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable / script error / information lost
    x == -94 ~ -8,                         # Insufficient information
    x == -92 ~ -9,                         # Refusal
    x == -91 ~ -1,                         # Item not applicable
    x == -99 ~ -3,                         # Not asked at fieldwork stage / not interviewed
    x == -98 ~ -3,                         # Not present (treated as not interviewed)
    x == -100 | x == -97 ~ -8,             # Insufficient information (default mapping)
    TRUE ~ x
  )
}

# Function to recode employment status variables
recode_ecoact <- function(x) {
  x <- harmonize_missing(x)
  case_when(
    x == 1 ~ 1,  # Doing paid work for 30 or more hours a week
    x == 2 ~ 2,  # Doing paid work for fewer than 30 hours a week
    x == 3 ~ 3,  # Unemployed/ Looking for a job
    x == 4 ~ 4,  # On a training course or scheme
    x == 5 ~ 5,  # In full-time education/ at school
    x == 6 ~ 6,  # Looking after the family/ household
    x == 7 ~ 7,  # Retired from work altogether
    x == 8 ~ 8,  # Sick/ disabled
    x == 9 ~ 9,  # Other
    TRUE ~ x
  )
}

# Recode variables for each wave
wave1 <- wave1 %>%
  mutate(
    ecoactma14 = recode_ecoact(W1empsmum),
    ecoactpa14 = recode_ecoact(W1empsdad)
  )

wave2 <- wave2 %>%
  mutate(
    ecoactma15 = recode_ecoact(W2empsmum),
    ecoactpa15 = recode_ecoact(W2empsdad)
  )

wave3 <- wave3 %>%
  mutate(
    ecoactma16 = recode_ecoact(W3empsmum),
    ecoactpa16 = recode_ecoact(W3empsdad)
  )

wave4 <- wave4 %>%
  mutate(
    ecoactma17 = recode_ecoact(w4empsmum),
    ecoactpa17 = recode_ecoact(w4empsdad)
  )

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Return path to output file
"data/output/cleaned_data.csv"