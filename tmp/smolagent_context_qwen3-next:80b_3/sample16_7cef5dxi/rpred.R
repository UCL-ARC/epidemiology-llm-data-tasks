library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load wave one (age 14)
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave1 <- wave1 %>%
  select(NSID, W1GrsswkHH) %>%
  mutate(W1GrsswkHH = case_when(
    W1GrsswkHH == -999 ~ -2,
    W1GrsswkHH == -992 ~ -9,
    W1GrsswkHH == -99 ~ -3,
    W1GrsswkHH == -94 ~ -8,
    W1GrsswkHH == -92 ~ -9,
    W1GrsswkHH == -91 ~ -1,
    W1GrsswkHH == -3 ~ -3,
    W1GrsswkHH == -1 ~ -8,
    is.na(W1GrsswkHH) ~ -3,
    TRUE ~ W1GrsswkHH
  )) %>%
  mutate(incwhhcnt14 = W1GrsswkHH) %>%
  mutate(incwhh14_numeric = case_when(
    incwhhcnt14 == -9 ~ -9,
    incwhhcnt14 == -8 ~ -8,
    incwhhcnt14 == -1 ~ -1,
    incwhhcnt14 == -3 ~ -3,
    incwhhcnt14 == -2 ~ -2,
    incwhhcnt14 >= 0 & incwhhcnt14 <= 49 ~ 1,
    incwhhcnt14 >= 50 & incwhhcnt14 <= 99 ~ 2,
    incwhhcnt14 >= 100 & incwhhcnt14 <= 199 ~ 3,
    incwhhcnt14 >= 200 & incwhhcnt14 <= 299 ~ 4,
    incwhhcnt14 >= 300 & incwhhcnt14 <= 399 ~ 5,
    incwhhcnt14 >= 400 & incwhhcnt14 <= 499 ~ 6,
    incwhhcnt14 >= 500 & incwhhcnt14 <= 599 ~ 7,
    incwhhcnt14 >= 600 & incwhhcnt14 <= 699 ~ 8,
    incwhhcnt14 >= 700 & incwhhcnt14 <= 799 ~ 9,
    incwhhcnt14 >= 800 & incwhhcnt14 <= 899 ~ 10,
    incwhhcnt14 >= 900 & incwhhcnt14 <= 999 ~ 11,
    incwhhcnt14 >= 1000 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(incwhh14 = factor(incwhh14_numeric, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -1, -3, -2),
                          labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more", "Refusal", "Don't know/insufficient information", "Item not applicable", "Not asked/interviewed", "Script error/lost"))) %>%
  select(NSID, incwhh14, incwhhcnt14)

# Wave two (age 15)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave2 <- wave2 %>%
  select(NSID, W2GrsswkHH) %>%
  mutate(W2GrsswkHH = case_when(
    W2GrsswkHH == -999 ~ -2,
    W2GrsswkHH == -992 ~ -9,
    W2GrsswkHH == -99 ~ -3,
    W2GrsswkHH == -94 ~ -8,
    W2GrsswkHH == -92 ~ -9,
    W2GrsswkHH == -91 ~ -1,
    W2GrsswkHH == -3 ~ -3,
    W2GrsswkHH == -1 ~ -8,
    is.na(W2GrsswkHH) ~ -3,
    TRUE ~ W2GrsswkHH
  )) %>%
  mutate(incwhhcnt15 = W2GrsswkHH) %>%
  mutate(incwhh15_numeric = case_when(
    incwhhcnt15 == -9 ~ -9,
    incwhhcnt15 == -8 ~ -8,
    incwhhcnt15 == -1 ~ -1,
    incwhhcnt15 == -3 ~ -3,
    incwhhcnt15 == -2 ~ -2,
    incwhhcnt15 >= 0 & incwhhcnt15 <= 49 ~ 1,
    incwhhcnt15 >= 50 & incwhhcnt15 <= 99 ~ 2,
    incwhhcnt15 >= 100 & incwhhcnt15 <= 199 ~ 3,
    incwhhcnt15 >= 200 & incwhhcnt15 <= 299 ~ 4,
    incwhhcnt15 >= 300 & incwhhcnt15 <= 399 ~ 5,
    incwhhcnt15 >= 400 & incwhhcnt15 <= 499 ~ 6,
    incwhhcnt15 >= 500 & incwhhcnt15 <= 599 ~ 7,
    incwhhcnt15 >= 600 & incwhhcnt15 <= 699 ~ 8,
    incwhhcnt15 >= 700 & incwhhcnt15 <= 799 ~ 9,
    incwhhcnt15 >= 800 & incwhhcnt15 <= 899 ~ 10,
    incwhhcnt15 >= 900 & incwhhcnt15 <= 999 ~ 11,
    incwhhcnt15 >= 1000 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(incwhh15 = factor(incwhh15_numeric, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -1, -3, -2),
                          labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more", "Refusal", "Don't know/insufficient information", "Item not applicable", "Not asked/interviewed", "Script error/lost"))) %>%
  select(NSID, incwhh15, incwhhcnt15)

# Wave three (age 16)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave3 <- wave3 %>%
  select(NSID, W3incestw) %>%
  mutate(W3incestw = case_when(
    W3incestw == -99 ~ -3,
    W3incestw == -92 ~ -9,
    W3incestw == -1 ~ -8,
    TRUE ~ W3incestw
  )) %>%
  mutate(incwhh16 = factor(W3incestw, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -3),
                          labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £990", "£1,000 or more", "Refusal", "Don't know/insufficient information", "Not asked/interviewed"))) %>%
  select(NSID, incwhh16)

# Wave four (age 17)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave4 <- wave4 %>%
  select(NSID, w4IncEstW) %>%
  mutate(w4IncEstW = case_when(
    w4IncEstW == -996 ~ -1,
    w4IncEstW == -99 ~ -3,
    w4IncEstW == -92 ~ -9,
    w4IncEstW == -1 ~ -8,
    TRUE ~ w4IncEstW
  )) %>%
  mutate(incwhh17 = factor(w4IncEstW, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -1, -3),
                          labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more", "Refusal", "Don't know/insufficient information", "Item not applicable", "Not asked/interviewed"))) %>%
  select(NSID, incwhh17)

# Merge all waves
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Select required variables
cleaned_data <- merged_data %>% select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write to CSV
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(cleaned_data, "data/output/cleaned_data.csv")