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
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Recode missing values for wave_one (age 14)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH == -999.0 ~ -3,
      W1GrsswkHH == -992.0 ~ -9,
      W1GrsswkHH == -99.0 ~ -3,
      W1GrsswkHH == -94.0 ~ -8,
      W1GrsswkHH == -92.0 ~ -9,
      W1GrsswkHH == -91.0 ~ -1,
      W1GrsswkHH == -3.0 ~ -1,
      W1GrsswkHH == -1.0 ~ -8,
      TRUE ~ W1GrsswkHH
    ),
    incwhh14 = case_when(
      incwhhcnt14 >= 0 & incwhhcnt14 < 200 ~ 1,
      incwhhcnt14 >= 200 & incwhhcnt14 < 400 ~ 2,
      incwhhcnt14 >= 400 & incwhhcnt14 < 600 ~ 3,
      incwhhcnt14 >= 600 & incwhhcnt14 < 800 ~ 4,
      incwhhcnt14 >= 800 ~ 5,
      TRUE ~ incwhhcnt14
    )
  )

# Recode missing values for wave_two (age 15)
merged_data <- merged_data %>%
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH == -999.0 ~ -3,
      W2GrsswkHH == -992.0 ~ -9,
      W2GrsswkHH == -99.0 ~ -3,
      W2GrsswkHH == -94.0 ~ -8,
      W2GrsswkHH == -92.0 ~ -9,
      W2GrsswkHH == -91.0 ~ -1,
      W2GrsswkHH == -3.0 ~ -1,
      W2GrsswkHH == -1.0 ~ -8,
      TRUE ~ W2GrsswkHH
    ),
    incwhh15 = case_when(
      incwhhcnt15 >= 0 & incwhhcnt15 < 200 ~ 1,
      incwhhcnt15 >= 200 & incwhhcnt15 < 400 ~ 2,
      incwhhcnt15 >= 400 & incwhhcnt15 < 600 ~ 3,
      incwhhcnt15 >= 600 & incwhhcnt15 < 800 ~ 4,
      incwhhcnt15 >= 800 ~ 5,
      TRUE ~ incwhhcnt15
    )
  )

# Recode missing values for wave_three (age 16)
merged_data <- merged_data %>%
  mutate(
    incwhh16 = case_when(
      W3incestw == -99.0 ~ -3,
      W3incestw == -92.0 ~ -9,
      W3incestw == -1.0 ~ -8,
      TRUE ~ W3incestw
    )
  )

# Recode missing values for wave_four (age 17)
merged_data <- merged_data %>%
  mutate(
    incwhh17 = case_when(
      w4IncEstW == -996.0 ~ -3,
      w4IncEstW == -99.0 ~ -3,
      w4IncEstW == -92.0 ~ -9,
      w4IncEstW == -1.0 ~ -8,
      TRUE ~ w4IncEstW
    )
  )

# Define factor levels for banded variables
incwhh14_levels <- c(
  -9, -8, -3, -2, -1,
  1, 2, 3, 4, 5
)
incwhh14_labels <- c(
  "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable",
  "Up to £199", "£200 up to £399", "£400 up to £599", "£600 up to £799", "£800 or more"
)

incwhh15_levels <- c(
  -9, -8, -3, -2, -1,
  1, 2, 3, 4, 5
)
incwhh15_labels <- c(
  "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable",
  "Up to £199", "£200 up to £399", "£400 up to £599", "£600 up to £799", "£800 or more"
)

incwhh16_levels <- c(
  -9, -8, -3, -2, -1,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
)
incwhh16_labels <- c(
  "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable",
  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
  "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799",
  "£800 up to £899", "£900 up to £990", "£1,000 or more"
)

incwhh17_levels <- c(
  -9, -8, -3, -2, -1,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
)
incwhh17_labels <- c(
  "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable",
  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
  "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799",
  "£800 up to £899", "£900 up to £999", "£1,000 or more"
)

# Convert banded variables to factors
merged_data <- merged_data %>%
  mutate(
    incwhh14 = factor(incwhh14, levels = incwhh14_levels, labels = incwhh14_labels),
    incwhh15 = factor(incwhh15, levels = incwhh15_levels, labels = incwhh15_labels),
    incwhh16 = factor(incwhh16, levels = incwhh16_levels, labels = incwhh16_labels),
    incwhh17 = factor(incwhh17, levels = incwhh17_levels, labels = incwhh17_labels)
  )

# Select and output the required variables
output_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write the output file
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)