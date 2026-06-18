library(readr)
library(dplyr)
library(haven)

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

# Function to map missing values
map_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable / script error / information lost
    x == -94 ~ -8,                          # Insufficient information
    x == -92 ~ -9,                          # Refusal
    x == -91 ~ -1,                          # Item not applicable
    x == -99 ~ -3,                          # Not asked at fieldwork stage / not interviewed
    x == -100 ~ -2,                         # Depends on label; default to -2
    x == -97 ~ -2,                          # Depends on label; default to -2
    x == -98 ~ -1,                          # Not present
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

# Select only the ID and derived variables
output_data <- merged_data %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")