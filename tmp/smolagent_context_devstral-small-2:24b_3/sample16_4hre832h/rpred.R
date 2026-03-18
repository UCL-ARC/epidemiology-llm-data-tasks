library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Load datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave1, wave2, by = "NSID")
merged_data <- full_join(merged_data, wave3, by = "NSID")
merged_data <- full_join(merged_data, wave4, by = "NSID")

# Standard missing value codes
standard_missing <- c(-9, -8, -1, -3, -2)
names(standard_missing) <- c("Refusal", "Don't know/insufficient information", "Item not applicable", "Not asked/interviewed", "Script error/lost")

# Recode missing values for wave1 (age 14)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH == -999.0 ~ -3,  # Missing in error -> Not asked/interviewed
      W1GrsswkHH == -992.0 ~ -8,  # No information -> Don't know/insufficient
      W1GrsswkHH == -99.0 ~ -3,   # HH not interviewed -> Not asked/interviewed
      W1GrsswkHH == -94.0 ~ -8,   # Insufficient information -> Don't know/insufficient
      W1GrsswkHH == -92.0 ~ -9,   # Refused -> Refusal
      W1GrsswkHH == -91.0 ~ -1,   # Not applicable -> Item not applicable
      W1GrsswkHH == -3.0 ~ -1,    # Not yet paid -> Item not applicable
      W1GrsswkHH == -1.0 ~ -8,    # Don't know -> Don't know/insufficient
      TRUE ~ W1GrsswkHH
    ),
    incwhh14 = case_when(
      incwhhcnt14 == -9 ~ -9,
      incwhhcnt14 == -8 ~ -8,
      incwhhcnt14 == -1 ~ -1,
      incwhhcnt14 == -3 ~ -3,
      incwhhcnt14 == -2 ~ -2,
      incwhhcnt14 < 0 ~ -3,
      incwhhcnt14 >= 0 & incwhhcnt14 < 200 ~ 1,
      incwhhcnt14 >= 200 & incwhhcnt14 < 400 ~ 2,
      incwhhcnt14 >= 400 & incwhhcnt14 < 600 ~ 3,
      incwhhcnt14 >= 600 & incwhhcnt14 < 800 ~ 4,
      incwhhcnt14 >= 800 ~ 5
    )
  )

# Recode missing values for wave2 (age 15)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH == -999.0 ~ -3,  # Missing in error -> Not asked/interviewed
      W2GrsswkHH == -992.0 ~ -8,  # No information -> Don't know/insufficient
      W2GrsswkHH == -99.0 ~ -3,   # HH not interviewed -> Not asked/interviewed
      W2GrsswkHH == -94.0 ~ -8,   # Insufficient information -> Don't know/insufficient
      W2GrsswkHH == -92.0 ~ -9,   # Refused -> Refusal
      W2GrsswkHH == -91.0 ~ -1,   # Not applicable -> Item not applicable
      W2GrsswkHH == -3.0 ~ -1,    # Not yet paid -> Item not applicable
      W2GrsswkHH == -1.0 ~ -8,    # Don't know -> Don't know/insufficient
      TRUE ~ W2GrsswkHH
    ),
    incwhh15 = case_when(
      incwhhcnt15 == -9 ~ -9,
      incwhhcnt15 == -8 ~ -8,
      incwhhcnt15 == -1 ~ -1,
      incwhhcnt15 == -3 ~ -3,
      incwhhcnt15 == -2 ~ -2,
      incwhhcnt15 < 0 ~ -3,
      incwhhcnt15 >= 0 & incwhhcnt15 < 200 ~ 1,
      incwhhcnt15 >= 200 & incwhhcnt15 < 400 ~ 2,
      incwhhcnt15 >= 400 & incwhhcnt15 < 600 ~ 3,
      incwhhcnt15 >= 600 & incwhhcnt15 < 800 ~ 4,
      incwhhcnt15 >= 800 ~ 5
    )
  )

# Recode missing values for wave3 (age 16)
merged_data <- merged_data %>%
  mutate(
    incwhh16 = case_when(
      W3incestw == -99.0 ~ -3,    # MP not interviewed -> Not asked/interviewed
      W3incestw == -92.0 ~ -9,    # Refused -> Refusal
      W3incestw == -1.0 ~ -8,     # Don't know -> Don't know/insufficient
      TRUE ~ W3incestw
    )
  )

# Recode missing values for wave4 (age 17)
merged_data <- merged_data %>%
  mutate(
    incwhh17 = case_when(
      w4IncEstW == -996.0 ~ -3,   # No parent in household -> Not asked/interviewed
      w4IncEstW == -99.0 ~ -3,    # MP not interviewed -> Not asked/interviewed
      w4IncEstW == -92.0 ~ -9,    # Refused -> Refusal
      w4IncEstW == -1.0 ~ -8,     # Don't know -> Don't know/insufficient
      TRUE ~ w4IncEstW
    )
  )

# Select and output required variables
output_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write output
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)