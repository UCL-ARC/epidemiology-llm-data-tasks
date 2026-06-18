library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define the mapping for missing values
map_missing <- function(x) {
  case_when(
    x %in% c(-99, -98, -996) ~ -3,
    TRUE ~ x
  )
}

# Process mother's economic activity variables
merged_data <- merged_data %>%
  mutate(
    ecoactma14 = map_missing(W1empsmum),
    ecoactma15 = map_missing(W2empsmum),
    ecoactma16 = map_missing(W3empsmum),
    ecoactma17 = map_missing(w4empsmum)
  )

# Process father's economic activity variables
merged_data <- merged_data %>%
  mutate(
    ecoactpa14 = map_missing(W1empsdad),
    ecoactpa15 = map_missing(W2empsdad),
    ecoactpa16 = map_missing(W3empsdad),
    ecoactpa17 = map_missing(w4empsdad)
  )

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, ecoactma14, ecoactma15, ecoactma16, ecoactma17, ecoactpa14, ecoactpa15, ecoactpa16, ecoactpa17)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")