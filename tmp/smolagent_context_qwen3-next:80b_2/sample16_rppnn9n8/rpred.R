library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave1 <- wave1 %>%
  mutate(incwhhcnt14 = case_when(
    W1GrsswkHH == -999.0 ~ -2,
    W1GrsswkHH == -992.0 ~ -9,
    W1GrsswkHH == -99.0 ~ -3,
    W1GrsswkHH == -94.0 ~ -8,
    W1GrsswkHH == -92.0 ~ -9,
    W1GrsswkHH == -91.0 ~ -1,
    W1GrsswkHH == -3.0 ~ -3,
    TRUE ~ W1GrsswkHH
  )) %>%
  mutate(incwhh14 = case_when(
    incwhhcnt14 == -9 ~ "Refusal",
    incwhhcnt14 == -8 ~ "Don't know/insufficient information",
    incwhhcnt14 == -1 ~ "Item not applicable",
    incwhhcnt14 == -3 ~ "Not asked/interviewed",
    incwhhcnt14 == -2 ~ "Script error/lost",
    incwhhcnt14 < 50 ~ "Up to £49",
    incwhhcnt14 >= 50 & incwhhcnt14 < 100 ~ "£50 up to £99",
    incwhhcnt14 >= 100 & incwhhcnt14 < 200 ~ "£100 up to £199",
    incwhhcnt14 >= 200 & incwhhcnt14 < 300 ~ "£200 up to £299",
    incwhhcnt14 >= 300 & incwhhcnt14 < 400 ~ "£300 up to £399",
    incwhhcnt14 >= 400 & incwhhcnt14 < 500 ~ "£400 up to £499",
    incwhhcnt14 >= 500 & incwhhcnt14 < 600 ~ "£500 up to £599",
    incwhhcnt14 >= 600 & incwhhcnt14 < 700 ~ "£600 up to £699",
    incwhhcnt14 >= 700 & incwhhcnt14 < 800 ~ "£700 up to £799",
    incwhhcnt14 >= 800 & incwhhcnt14 < 900 ~ "£800 up to £899",
    incwhhcnt14 >= 900 & incwhhcnt14 < 1000 ~ "£900 up to £999",
    incwhhcnt14 >= 1000 ~ "£1,000 or more"
  )) %>%
  mutate(incwhh14 = factor(incwhh14, levels = c(
    "Refusal", "Don't know/insufficient information", "Item not applicable", 
    "Not asked/interviewed", "Script error/lost",
    "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
    "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
    "£900 up to £999", "£1,000 or more"
  )))

wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave2 <- wave2 %>%
  mutate(incwhhcnt15 = case_when(
    W2GrsswkHH == -999.0 ~ -2,
    W2GrsswkHH == -992.0 ~ -9,
    W2GrsswkHH == -99.0 ~ -3,
    W2GrsswkHH == -94.0 ~ -8,
    W2GrsswkHH == -92.0 ~ -9,
    W2GrsswkHH == -91.0 ~ -1,
    W2GrsswkHH == -3.0 ~ -3,
    TRUE ~ W2GrsswkHH
  )) %>%
  mutate(incwhh15 = case_when(
    incwhhcnt15 == -9 ~ "Refusal",
    incwhhcnt15 == -8 ~ "Don't know/insufficient information",
    incwhhcnt15 == -1 ~ "Item not applicable",
    incwhhcnt15 == -3 ~ "Not asked/interviewed",
    incwhhcnt15 == -2 ~ "Script error/lost",
    incwhhcnt15 < 50 ~ "Up to £49",
    incwhhcnt15 >= 50 & incwhhcnt15 < 100 ~ "£50 up to £99",
    incwhhcnt15 >= 100 & incwhhcnt15 < 200 ~ "£100 up to £199",
    incwhhcnt15 >= 200 & incwhhcnt15 < 300 ~ "£200 up to £299",
    incwhhcnt15 >= 300 & incwhhcnt15 < 400 ~ "£300 up to £399",
    incwhhcnt15 >= 400 & incwhhcnt15 < 500 ~ "£400 up to £499",
    incwhhcnt15 >= 500 & incwhhcnt15 < 600 ~ "£500 up to £599",
    incwhhcnt15 >= 600 & incwhhcnt15 < 700 ~ "£600 up to £699",
    incwhhcnt15 >= 700 & incwhhcnt15 < 800 ~ "£700 up to £799",
    incwhhcnt15 >= 800 & incwhhcnt15 < 900 ~ "£800 up to £899",
    incwhhcnt15 >= 900 & incwhhcnt15 < 1000 ~ "£900 up to £999",
    incwhhcnt15 >= 1000 ~ "£1,000 or more"
  )) %>%
  mutate(incwhh15 = factor(incwhh15, levels = c(
    "Refusal", "Don't know/insufficient information", "Item not applicable", 
    "Not asked/interviewed", "Script error/lost",
    "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
    "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
    "£900 up to £999", "£1,000 or more"
  )))

wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave3 <- wave3 %>%
  mutate(incwhh16 = case_when(
    W3incestw == -99 ~ -3,
    W3incestw == -92 ~ -9,
    W3incestw == -1 ~ -8,
    TRUE ~ W3incestw
  )) %>%
  mutate(incwhh16 = case_when(
    incwhh16 == -3 ~ "Not asked/interviewed",
    incwhh16 == -9 ~ "Refusal",
    incwhh16 == -8 ~ "Don't know/insufficient information",
    incwhh16 == 1 ~ "Up to £49",
    incwhh16 == 2 ~ "£50 up to £99",
    incwhh16 == 3 ~ "£100 up to £199",
    incwhh16 == 4 ~ "£200 up to £299",
    incwhh16 == 5 ~ "£300 up to £399",
    incwhh16 == 6 ~ "£400 up to £499",
    incwhh16 == 7 ~ "£500 up to £599",
    incwhh16 == 8 ~ "£600 up to £699",
    incwhh16 == 9 ~ "£700 up to £799",
    incwhh16 == 10 ~ "£800 up to £899",
    incwhh16 == 11 ~ "£900 up to £999",
    incwhh16 == 12 ~ "£1,000 or more"
  )) %>%
  mutate(incwhh16 = factor(incwhh16, levels = c(
    "Not asked/interviewed", "Refusal", "Don't know/insufficient information",
    "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
    "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
    "£900 up to £999", "£1,000 or more"
  )))

wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave4 <- wave4 %>%
  mutate(incwhh17 = case_when(
    w4IncEstW == -996 ~ -1,
    w4IncEstW == -99 ~ -3,
    w4IncEstW == -92 ~ -9,
    w4IncEstW == -1 ~ -8,
    TRUE ~ w4IncEstW
  )) %>%
  mutate(incwhh17 = case_when(
    incwhh17 == -1 ~ "Item not applicable",
    incwhh17 == -3 ~ "Not asked/interviewed",
    incwhh17 == -9 ~ "Refusal",
    incwhh17 == -8 ~ "Don't know/insufficient information",
    incwhh17 == 1 ~ "Up to £49",
    incwhh17 == 2 ~ "£50 up to £99",
    incwhh17 == 3 ~ "£100 up to £199",
    incwhh17 == 4 ~ "£200 up to £299",
    incwhh17 == 5 ~ "£300 up to £399",
    incwhh17 == 6 ~ "£400 up to £499",
    incwhh17 == 7 ~ "£500 up to £599",
    incwhh17 == 8 ~ "£600 up to £699",
    incwhh17 == 9 ~ "£700 up to £799",
    incwhh17 == 10 ~ "£800 up to £899",
    incwhh17 == 11 ~ "£900 up to £999",
    incwhh17 == 12 ~ "£1,000 or more"
  )) %>%
  mutate(incwhh17 = factor(incwhh17, levels = c(
    "Item not applicable", "Not asked/interviewed", "Refusal", "Don't know/insufficient information",
    "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399",
    "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899",
    "£900 up to £999", "£1,000 or more"
  )))

merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

cleaned_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

write_csv(cleaned_data, "data/output/cleaned_data.csv")