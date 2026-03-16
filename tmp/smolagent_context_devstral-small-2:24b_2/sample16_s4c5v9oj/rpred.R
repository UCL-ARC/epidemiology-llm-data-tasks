library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Standard missing value codes
standard_missing <- c(
  `-9` = "Refusal",
  `-8` = "Don't know/insufficient information",
  `-1` = "Item not applicable",
  `-3` = "Not asked/interviewed",
  `-2` = "Script error/lost"
)

# Harmonize missing values for wave1 (age 14)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH == -999.0 ~ -3,
      W1GrsswkHH == -992.0 ~ -9,
      W1GrsswkHH == -99.0 ~ -3,
      W1GrsswkHH == -94.0 ~ -8,
      W1GrsswkHH == -92.0 ~ -9,
      W1GrsswkHH == -91.0 ~ -1,
      W1GrsswkHH == -3.0 ~ -3,
      W1GrsswkHH == -1.0 ~ -8,
      TRUE ~ W1GrsswkHH
    )
  )

# Harmonize missing values for wave2 (age 15)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH == -999.0 ~ -3,
      W2GrsswkHH == -992.0 ~ -9,
      W2GrsswkHH == -99.0 ~ -3,
      W2GrsswkHH == -94.0 ~ -8,
      W2GrsswkHH == -92.0 ~ -9,
      W2GrsswkHH == -91.0 ~ -1,
      W2GrsswkHH == -3.0 ~ -3,
      W2GrsswkHH == -1.0 ~ -8,
      TRUE ~ W2GrsswkHH
    )
  )

# Derive banded variables for ages 14 and 15
merged_data <- merged_data %>%
  mutate(
    incwhh14 = case_when(
      incwhhcnt14 >= 0 & incwhhcnt14 < 50 ~ 1,
      incwhhcnt14 >= 50 & incwhhcnt14 < 100 ~ 2,
      incwhhcnt14 >= 100 & incwhhcnt14 < 200 ~ 3,
      incwhhcnt14 >= 200 & incwhhcnt14 < 300 ~ 4,
      incwhhcnt14 >= 300 & incwhhcnt14 < 400 ~ 5,
      incwhhcnt14 >= 400 & incwhhcnt14 < 500 ~ 6,
      incwhhcnt14 >= 500 & incwhhcnt14 < 600 ~ 7,
      incwhhcnt14 >= 600 & incwhhcnt14 < 700 ~ 8,
      incwhhcnt14 >= 700 & incwhhcnt14 < 800 ~ 9,
      incwhhcnt14 >= 800 & incwhhcnt14 < 900 ~ 10,
      incwhhcnt14 >= 900 & incwhhcnt14 < 1000 ~ 11,
      incwhhcnt14 >= 1000 ~ 12,
      TRUE ~ incwhhcnt14
    )
  )

merged_data <- merged_data %>%
  mutate(
    incwhh15 = case_when(
      incwhhcnt15 >= 0 & incwhhcnt15 < 50 ~ 1,
      incwhhcnt15 >= 50 & incwhhcnt15 < 100 ~ 2,
      incwhhcnt15 >= 100 & incwhhcnt15 < 200 ~ 3,
      incwhhcnt15 >= 200 & incwhhcnt15 < 300 ~ 4,
      incwhhcnt15 >= 300 & incwhhcnt15 < 400 ~ 5,
      incwhhcnt15 >= 400 & incwhhcnt15 < 500 ~ 6,
      incwhhcnt15 >= 500 & incwhhcnt15 < 600 ~ 7,
      incwhhcnt15 >= 600 & incwhhcnt15 < 700 ~ 8,
      incwhhcnt15 >= 700 & incwhhcnt15 < 800 ~ 9,
      incwhhcnt15 >= 800 & incwhhcnt15 < 900 ~ 10,
      incwhhcnt15 >= 900 & incwhhcnt15 < 1000 ~ 11,
      incwhhcnt15 >= 1000 ~ 12,
      TRUE ~ incwhhcnt15
    )
  )

# Harmonize missing values for wave3 (age 16)
merged_data <- merged_data %>%
  mutate(
    incwhh16 = case_when(
      W3incestw == -99.0 ~ -3,
      W3incestw == -92.0 ~ -9,
      W3incestw == -1.0 ~ -8,
      TRUE ~ W3incestw
    )
  )

# Harmonize missing values for wave4 (age 17)
merged_data <- merged_data %>%
  mutate(
    incwhh17 = case_when(
      w4IncEstW == -999.0 ~ -3,
      w4IncEstW == -99.0 ~ -3,
      w4IncEstW == -92.0 ~ -9,
      w4IncEstW == -1.0 ~ -8,
      TRUE ~ w4IncEstW
    )
  )

# Convert banded variables to factors with labels
merged_data <- merged_data %>%
  mutate(
    incwhh14 = factor(incwhh14, levels = c(-9, -8, -3, -2, -1, 1:12),
                        labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", 
                                  "Script error/lost", "Item not applicable",
                                  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299",
                                  "£300 up to £399", "£400 up to £499", "£500 up to £599",
                                  "£600 up to £699", "£700 up to £799", "£800 up to £899",
                                  "£900 up to £999", "£1,000 or more")),
    incwhh15 = factor(incwhh15, levels = c(-9, -8, -3, -2, -1, 1:12),
                        labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", 
                                  "Script error/lost", "Item not applicable",
                                  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299",
                                  "£300 up to £399", "£400 up to £499", "£500 up to £599",
                                  "£600 up to £699", "£700 up to £799", "£800 up to £899",
                                  "£900 up to £999", "£1,000 or more")),
    incwhh16 = factor(incwhh16, levels = c(-9, -8, -3, -2, -1, 1:12),
                        labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", 
                                  "Script error/lost", "Item not applicable",
                                  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299",
                                  "£300 up to £399", "£400 up to £499", "£500 up to £599",
                                  "£600 up to £699", "£700 up to £799", "£800 up to £899",
                                  "£900 up to £990", "£1,000 or more")),
    incwhh17 = factor(incwhh17, levels = c(-9, -8, -3, -2, -1, 1:12),
                        labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", 
                                  "Script error/lost", "Item not applicable",
                                  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299",
                                  "£300 up to £399", "£400 up to £499", "£500 up to £599",
                                  "£600 up to £699", "£700 up to £799", "£800 up to £899",
                                  "£900 up to £999", "£1,000 or more"))
  )

# Select and output required variables
output_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write output
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)