library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

wave1_clean <- wave1 %>%
  select(NSID, W1empsmum, W1empsdad) %>%
  rename(ecoactmum14 = W1empsmum, ecoactdad14 = W1empsdad) %>%
  mutate(ecoactmum14 = case_when(
    ecoactmum14 == -999 ~ -2,
    ecoactmum14 == -99 ~ -3,
    ecoactmum14 == -98 ~ -1,
    ecoactmum14 == -94 ~ -8,
    TRUE ~ ecoactmum14
  )) %>%
  mutate(ecoactdad14 = case_when(
    ecoactdad14 == -999 ~ -2,
    ecoactdad14 == -99 ~ -3,
    ecoactdad14 == -98 ~ -1,
    ecoactdad14 == -94 ~ -8,
    TRUE ~ ecoactdad14
  ))

wave2_clean <- wave2 %>%
  select(NSID, W2empsmum, W2empsdad) %>%
  rename(ecoactmum15 = W2empsmum, ecoactdad15 = W2empsdad) %>%
  mutate(ecoactmum15 = case_when(
    ecoactmum15 == -999 ~ -2,
    ecoactmum15 == -99 ~ -3,
    ecoactmum15 == -98 ~ -1,
    ecoactmum15 == -94 ~ -8,
    TRUE ~ ecoactmum15
  )) %>%
  mutate(ecoactdad15 = case_when(
    ecoactdad15 == -999 ~ -2,
    ecoactdad15 == -99 ~ -3,
    ecoactdad15 == -98 ~ -1,
    ecoactdad15 == -94 ~ -8,
    TRUE ~ ecoactdad15
  ))

wave3_clean <- wave3 %>%
  select(NSID, W3empsmum, W3empsdad) %>%
  rename(ecoactmum16 = W3empsmum, ecoactdad16 = W3empsdad) %>%
  mutate(ecoactmum16 = case_when(
    ecoactmum16 == -999 ~ -2,
    ecoactmum16 == -99 ~ -3,
    ecoactmum16 == -98 ~ -1,
    ecoactmum16 == -94 ~ -8,
    TRUE ~ ecoactmum16
  )) %>%
  mutate(ecoactdad16 = case_when(
    ecoactdad16 == -999 ~ -2,
    ecoactdad16 == -99 ~ -3,
    ecoactdad16 == -98 ~ -1,
    ecoactdad16 == -94 ~ -8,
    TRUE ~ ecoactdad16
  ))

wave4_clean <- wave4 %>%
  select(NSID, w4empsmum, w4empsdad) %>%
  rename(ecoactmum17 = w4empsmum, ecoactdad17 = w4empsdad) %>%
  mutate(ecoactmum17 = case_when(
    ecoactmum17 == -999 ~ -2,
    ecoactmum17 == -99 ~ -3,
    ecoactmum17 == -98 ~ -1,
    ecoactmum17 == -94 ~ -8,
    TRUE ~ ecoactmum17
  )) %>%
  mutate(ecoactdad17 = case_when(
    ecoactdad17 == -996 ~ -1,
    ecoactdad17 == -99 ~ -3,
    ecoactdad17 == -98 ~ -1,
    ecoactdad17 == -94 ~ -8,
    ecoactdad17 == -92 ~ -9,
    TRUE ~ ecoactdad17
  ))

cleaned_data <- full_join(wave1_clean, wave2_clean, by = "NSID") %>%
  full_join(wave3_clean, by = "NSID") %>%
  full_join(wave4_clean, by = "NSID")

write_csv(cleaned_data, "data/output/cleaned_data.csv")