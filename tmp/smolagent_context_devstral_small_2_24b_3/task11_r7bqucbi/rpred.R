library(readr)
library(dplyr)
library(haven)
library(labelled)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Define missing value mappings
map_missing <- function(x) {
  case_when(
    x %in% c(-99, -98, -996) ~ -3,
    x %in% c(-999, -997, -995) ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    TRUE ~ x
  )
}

# Process wave 1
wave1_ecoact <- wave1 %>%
  select(NSID, W1empsmum, W1empsdad) %>%
  mutate(
    ecoactma14 = map_missing(W1empsmum),
    ecoactpa14 = map_missing(W1empsdad)
  ) %>%
  select(NSID, ecoactma14, ecoactpa14)

# Process wave 2
wave2_ecoact <- wave2 %>%
  select(NSID, W2empsmum, W2empsdad) %>%
  mutate(
    ecoactma15 = map_missing(W2empsmum),
    ecoactpa15 = map_missing(W2empsdad)
  ) %>%
  select(NSID, ecoactma15, ecoactpa15)

# Process wave 3
wave3_ecoact <- wave3 %>%
  select(NSID, W3empsmum, W3empsdad) %>%
  mutate(
    ecoactma16 = map_missing(W3empsmum),
    ecoactpa16 = map_missing(W3empsdad)
  ) %>%
  select(NSID, ecoactma16, ecoactpa16)

# Process wave 4
wave4_ecoact <- wave4 %>%
  select(NSID, w4empsmum, w4empsdad) %>%
  mutate(
    ecoactma17 = map_missing(w4empsmum),
    ecoactpa17 = map_missing(w4empsdad)
  ) %>%
  select(NSID, ecoactma17, ecoactpa17)

# Merge all datasets
cleaned_data <- wave1_ecoact %>%
  full_join(wave2_ecoact, by = "NSID") %>%
  full_join(wave3_ecoact, by = "NSID") %>%
  full_join(wave4_ecoact, by = "NSID")

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")