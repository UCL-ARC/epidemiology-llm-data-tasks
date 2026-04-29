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

# Recode missing values for age 14
merged_data <- merged_data %>% 
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH == -999.0 ~ -3,
      W1GrsswkHH == -992.0 ~ -8,
      W1GrsswkHH == -99.0 ~ -3,
      W1GrsswkHH == -94.0 ~ -8,
      W1GrsswkHH == -92.0 ~ -9,
      W1GrsswkHH == -91.0 ~ -1,
      W1GrsswkHH == -3.0 ~ -1,
      W1GrsswkHH == -1.0 ~ -8,
      TRUE ~ W1GrsswkHH
    )
  )

# Recode missing values for age 15
merged_data <- merged_data %>% 
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH == -999.0 ~ -3,
      W2GrsswkHH == -992.0 ~ -8,
      W2GrsswkHH == -99.0 ~ -3,
      W2GrsswkHH == -94.0 ~ -8,
      W2GrsswkHH == -92.0 ~ -9,
      W2GrsswkHH == -91.0 ~ -1,
      W2GrsswkHH == -3.0 ~ -1,
      W2GrsswkHH == -1.0 ~ -8,
      TRUE ~ W2GrsswkHH
    )
  )

# Recode missing values for age 16
merged_data <- merged_data %>% 
  mutate(
    incwhh16 = case_when(
      W3incestw == -99.0 ~ -3,
      W3incestw == -92.0 ~ -9,
      W3incestw == -1.0 ~ -8,
      TRUE ~ W3incestw
    )
  )

# Recode missing values for age 17
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

# Create banded variables for ages 14 and 15
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

# Label missing values
merged_data <- merged_data %>% 
  mutate(
    incwhh14 = case_when(
      incwhh14 == -9 ~ "Refusal",
      incwhh14 == -8 ~ "Don't know/insufficient information",
      incwhh14 == -1 ~ "Item not applicable",
      incwhh14 == -3 ~ "Not asked/interviewed",
      incwhh14 == -2 ~ "Script error/lost",
      TRUE ~ as.character(incwhh14)
    ),
    incwhh15 = case_when(
      incwhh15 == -9 ~ "Refusal",
      incwhh15 == -8 ~ "Don't know/insufficient information",
      incwhh15 == -1 ~ "Item not applicable",
      incwhh15 == -3 ~ "Not asked/interviewed",
      incwhh15 == -2 ~ "Script error/lost",
      TRUE ~ as.character(incwhh15)
    ),
    incwhh16 = case_when(
      incwhh16 == -9 ~ "Refusal",
      incwhh16 == -8 ~ "Don't know/insufficient information",
      incwhh16 == -1 ~ "Item not applicable",
      incwhh16 == -3 ~ "Not asked/interviewed",
      incwhh16 == -2 ~ "Script error/lost",
      TRUE ~ as.character(incwhh16)
    ),
    incwhh17 = case_when(
      incwhh17 == -9 ~ "Refusal",
      incwhh17 == -8 ~ "Don't know/insufficient information",
      incwhh17 == -1 ~ "Item not applicable",
      incwhh17 == -3 ~ "Not asked/interviewed",
      incwhh17 == -2 ~ "Script error/lost",
      TRUE ~ as.character(incwhh17)
    )
  )

# Select required variables
cleaned_data <- merged_data %>% 
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)