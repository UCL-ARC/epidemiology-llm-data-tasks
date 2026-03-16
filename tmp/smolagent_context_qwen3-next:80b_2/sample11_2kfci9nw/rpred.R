library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

wave1 <- wave1 %>%
  mutate(
    W1empsmum = case_when(
      W1empsmum == -999 ~ -2,
      W1empsmum == -99 ~ -3,
      W1empsmum == -98 ~ -3,
      W1empsmum == -94 ~ -8,
      is.na(W1empsmum) ~ -3,
      TRUE ~ W1empsmum
    ),
    W1empsdad = case_when(
      W1empsdad == -999 ~ -2,
      W1empsdad == -99 ~ -3,
      W1empsdad == -98 ~ -3,
      W1empsdad == -94 ~ -8,
      is.na(W1empsdad) ~ -3,
      TRUE ~ W1empsdad
    )
  ) %>%
  select(NSID, W1empsmum, W1empsdad) %>%
  rename(ecoactmum14 = W1empsmum, ecoactdad14 = W1empsdad)

wave2 <- wave2 %>%
  mutate(
    W2empsmum = case_when(
      W2empsmum == -999 ~ -2,
      W2empsmum == -99 ~ -3,
      W2empsmum == -98 ~ -3,
      W2empsmum == -94 ~ -8,
      is.na(W2empsmum) ~ -3,
      TRUE ~ W2empsmum
    ),
    W2empsdad = case_when(
      W2empsdad == -999 ~ -2,
      W2empsdad == -99 ~ -3,
      W2empsdad == -98 ~ -3,
      W2empsdad == -94 ~ -8,
      is.na(W2empsdad) ~ -3,
      TRUE ~ W2empsdad
    )
  ) %>%
  select(NSID, W2empsmum, W2empsdad) %>%
  rename(ecoactmum15 = W2empsmum, ecoactdad15 = W2empsdad)

wave3 <- wave3 %>%
  mutate(
    W3empsmum = case_when(
      W3empsmum == -999 ~ -2,
      W3empsmum == -99 ~ -3,
      W3empsmum == -98 ~ -3,
      W3empsmum == -94 ~ -8,
      is.na(W3empsmum) ~ -3,
      TRUE ~ W3empsmum
    ),
    W3empsdad = case_when(
      W3empsdad == -999 ~ -2,
      W3empsdad == -99 ~ -3,
      W3empsdad == -98 ~ -3,
      W3empsdad == -94 ~ -8,
      is.na(W3empsdad) ~ -3,
      TRUE ~ W3empsdad
    )
  ) %>%
  select(NSID, W3empsmum, W3empsdad) %>%
  rename(ecoactmum16 = W3empsmum, ecoactdad16 = W3empsdad)

wave4 <- wave4 %>%
  mutate(
    w4empsmum = case_when(
      w4empsmum == -999 ~ -2,
      w4empsmum == -99 ~ -3,
      w4empsmum == -98 ~ -3,
      w4empsmum == -94 ~ -8,
      is.na(w4empsmum) ~ -3,
      TRUE ~ w4empsmum
    ),
    w4empsdad = case_when(
      w4empsdad == -999 ~ -2,
      w4empsdad == -99 ~ -3,
      w4empsdad == -98 ~ -3,
      w4empsdad == -94 ~ -8,
      w4empsdad == -996 ~ -1,
      w4empsdad == -92 ~ -9,
      is.na(w4empsdad) ~ -3,
      TRUE ~ w4empsdad
    )
  ) %>%
  select(NSID, w4empsmum, w4empsdad) %>%
  rename(ecoactmum17 = w4empsmum, ecoactdad17 = w4empsdad)

cleaned_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

write_csv(cleaned_data, "data/output/cleaned_data.csv")