library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_three, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")

# Standard missing value codes
standard_missing <- c("-9" = "Refusal", "-8" = "Don't know/insufficient information", "-1" = "Item not applicable", "-3" = "Not asked/interviewed", "-2" = "Script error/lost")

# Harmonize missing values for continuous income variables
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
    ),
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

# Create banded variables for ages 14 and 15
merged_data <- merged_data %>%
  mutate(
    incwhh14 = case_when(
      incwhhcnt14 < 0 ~ as.character(incwhhcnt14),
      incwhhcnt14 <= 49 ~ "1",
      incwhhcnt14 <= 99 ~ "2",
      incwhhcnt14 <= 199 ~ "3",
      incwhhcnt14 <= 299 ~ "4",
      incwhhcnt14 <= 399 ~ "5",
      incwhhcnt14 <= 499 ~ "6",
      incwhhcnt14 <= 599 ~ "7",
      incwhhcnt14 <= 699 ~ "8",
      incwhhcnt14 <= 799 ~ "9",
      incwhhcnt14 <= 899 ~ "10",
      incwhhcnt14 <= 999 ~ "11",
      incwhhcnt14 >= 1000 ~ "12",
      TRUE ~ "-3"
    ),
    incwhh15 = case_when(
      incwhhcnt15 < 0 ~ as.character(incwhhcnt15),
      incwhhcnt15 <= 49 ~ "1",
      incwhhcnt15 <= 99 ~ "2",
      incwhhcnt15 <= 199 ~ "3",
      incwhhcnt15 <= 299 ~ "4",
      incwhhcnt15 <= 399 ~ "5",
      incwhhcnt15 <= 499 ~ "6",
      incwhhcnt15 <= 599 ~ "7",
      incwhhcnt15 <= 699 ~ "8",
      incwhhcnt15 <= 799 ~ "9",
      incwhhcnt15 <= 899 ~ "10",
      incwhhcnt15 <= 999 ~ "11",
      incwhhcnt15 >= 1000 ~ "12",
      TRUE ~ "-3"
    )
  )

# Harmonize missing values for categorical variables (ages 16 and 17)
merged_data <- merged_data %>%
  mutate(
    incwhh16 = case_when(
      W3incestw == -99.0 ~ "-3",
      W3incestw == -92.0 ~ "-9",
      W3incestw == -1.0 ~ "-8",
      TRUE ~ as.character(W3incestw)
    ),
    incwhh17 = case_when(
      w4IncEstW == -996.0 ~ "-1",
      w4IncEstW == -99.0 ~ "-3",
      w4IncEstW == -92.0 ~ "-9",
      w4IncEstW == -1.0 ~ "-8",
      TRUE ~ as.character(w4IncEstW)
    )
  )

# Convert banded variables to factors with labels
merged_data <- merged_data %>%
  mutate(
    incwhh14 = factor(incwhh14, levels = c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                   labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable", "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more")),
    incwhh15 = factor(incwhh15, levels = c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                   labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable", "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more")),
    incwhh16 = factor(incwhh16, levels = c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                   labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable", "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £990", "£1,000 or more")),
    incwhh17 = factor(incwhh17, levels = c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                   labels = c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable", "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more"))
  )

# Select and output required variables
output_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write output
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)