library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

data_wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
data_wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
data_wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
data_wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

wave1 <- data_wave1 %>%
  select(NSID, W1GrsswkHH) %>%
  rename(incwhhcnt14 = W1GrsswkHH) %>%
  mutate(
    incwhhcnt14 = case_when(
      is.na(incwhhcnt14) ~ -3,
      incwhhcnt14 == -999 ~ -2,
      incwhhcnt14 == -992 ~ -9,
      incwhhcnt14 == -99 ~ -3,
      incwhhcnt14 == -94 ~ -8,
      incwhhcnt14 == -92 ~ -9,
      incwhhcnt14 == -91 ~ -1,
      incwhhcnt14 == -3 ~ -1,
      incwhhcnt14 == -1 ~ -8,
      TRUE ~ incwhhcnt14
    ),
    incwhh14 = case_when(
      incwhhcnt14 == -9 ~ -9,
      incwhhcnt14 == -8 ~ -8,
      incwhhcnt14 == -3 ~ -3,
      incwhhcnt14 == -2 ~ -2,
      incwhhcnt14 == -1 ~ -1,
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
      incwhhcnt14 > 999 ~ 12,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(incwhh14 = factor(
    incwhh14,
    levels = c(1:12, -9, -8, -3, -2, -1),
    labels = c(
      "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
      "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
      "£900 up to £999", "£1,000 or more",
      "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable"
    )
  ))

wave2 <- data_wave2 %>%
  select(NSID, W2GrsswkHH) %>%
  rename(incwhhcnt15 = W2GrsswkHH) %>%
  mutate(
    incwhhcnt15 = case_when(
      is.na(incwhhcnt15) ~ -3,
      incwhhcnt15 == -999 ~ -2,
      incwhhcnt15 == -992 ~ -9,
      incwhhcnt15 == -99 ~ -3,
      incwhhcnt15 == -94 ~ -8,
      incwhhcnt15 == -92 ~ -9,
      incwhhcnt15 == -91 ~ -1,
      incwhhcnt15 == -3 ~ -1,
      incwhhcnt15 == -1 ~ -8,
      TRUE ~ incwhhcnt15
    ),
    incwhh15 = case_when(
      incwhhcnt15 == -9 ~ -9,
      incwhhcnt15 == -8 ~ -8,
      incwhhcnt15 == -3 ~ -3,
      incwhhcnt15 == -2 ~ -2,
      incwhhcnt15 == -1 ~ -1,
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
      incwhhcnt15 > 999 ~ 12,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(incwhh15 = factor(
    incwhh15,
    levels = c(1:12, -9, -8, -3, -2, -1),
    labels = c(
      "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
      "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
      "£900 up to £999", "£1,000 or more",
      "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable"
    )
  ))

wave3 <- data_wave3 %>%
  select(NSID, W3incestw) %>%
  rename(incwhh16 = W3incestw) %>%
  mutate(
    incwhh16 = case_when(
      is.na(incwhh16) ~ -3,
      incwhh16 == -999 ~ -2,
      incwhh16 == -99 ~ -3,
      incwhh16 == -92 ~ -9,
      incwhh16 == -1 ~ -8,
      TRUE ~ incwhh16
    )
  ) %>%
  mutate(incwhh16 = factor(
    incwhh16,
    levels = c(1:12, -9, -8, -3, -2, -1),
    labels = c(
      "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
      "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
      "£900 up to £999", "£1,000 or more",
      "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable"
    )
  ))

wave4 <- data_wave4 %>%
  select(NSID, w4IncEstW) %>%
  rename(incwhh17 = w4IncEstW) %>%
  mutate(
    incwhh17 = case_when(
      is.na(incwhh17) ~ -3,
      incwhh17 == -999 ~ -2,
      incwhh17 == -996 ~ -3,
      incwhh17 == -99 ~ -3,
      incwhh17 == -92 ~ -9,
      incwhh17 == -1 ~ -8,
      TRUE ~ incwhh17
    )
  ) %>%
  mutate(incwhh17 = factor(
    incwhh17,
    levels = c(1:12, -9, -8, -3, -2, -1),
    labels = c(
      "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
      "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
      "£900 up to £999", "£1,000 or more",
      "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable"
    )
  ))

merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

final_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

write_csv(final_data, "data/output/cleaned_data.csv")