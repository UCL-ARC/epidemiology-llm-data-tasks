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

# Define a function to map missing values to standard codes
map_missing <- function(x, metadata) {
  case_when(
    x %in% unlist(metadata[metadata == "Missing household information - lost" | metadata == "Missing household information", names(metadata)]) ~ -2,
    x %in% unlist(metadata[metadata == "Mother not interviewed" | metadata == "Father not interviewed", names(metadata)]) ~ -3,
    x %in% unlist(metadata[metadata == "Mother not present" | metadata == "Father not present", names(metadata)]) ~ -1,
    x %in% unlist(metadata[metadata == "Insufficient information", names(metadata)]) ~ -8,
    x %in% unlist(metadata[metadata == "Refusal", names(metadata)]) ~ -9,
    TRUE ~ x
  )
}

# Process mother's economic activity for each wave
# Wave 1 (Age 14)
merged_data <- merged_data %>%
  mutate(ecoactma14 = case_when(
    W1empsmum == 1 ~ 1,
    W1empsmum == 2 ~ 2,
    W1empsmum == 3 ~ 3,
    W1empsmum == 4 ~ 4,
    W1empsmum == 5 ~ 5,
    W1empsmum == 6 ~ 6,
    W1empsmum == 7 ~ 7,
    W1empsmum == 8 ~ 8,
    W1empsmum == 9 ~ 9,
    W1empsmum == -999 ~ -2,
    W1empsmum == -99 ~ -3,
    W1empsmum == -98 ~ -1,
    W1empsmum == -94 ~ -8,
    TRUE ~ as.numeric(W1empsmum)
  ))

# Wave 2 (Age 15)
merged_data <- merged_data %>%
  mutate(ecoactma15 = case_when(
    W2empsmum == 1 ~ 1,
    W2empsmum == 2 ~ 2,
    W2empsmum == 3 ~ 3,
    W2empsmum == 4 ~ 4,
    W2empsmum == 5 ~ 5,
    W2empsmum == 6 ~ 6,
    W2empsmum == 7 ~ 7,
    W2empsmum == 8 ~ 8,
    W2empsmum == 9 ~ 9,
    W2empsmum == -999 ~ -2,
    W2empsmum == -99 ~ -3,
    W2empsmum == -98 ~ -1,
    W2empsmum == -94 ~ -8,
    TRUE ~ as.numeric(W2empsmum)
  ))

# Wave 3 (Age 16)
merged_data <- merged_data %>%
  mutate(ecoactma16 = case_when(
    W3empsmum == 1 ~ 1,
    W3empsmum == 2 ~ 2,
    W3empsmum == 3 ~ 3,
    W3empsmum == 4 ~ 4,
    W3empsmum == 5 ~ 5,
    W3empsmum == 6 ~ 6,
    W3empsmum == 7 ~ 7,
    W3empsmum == 8 ~ 8,
    W3empsmum == 9 ~ 9,
    W3empsmum == -999 ~ -2,
    W3empsmum == -99 ~ -3,
    W3empsmum == -98 ~ -1,
    W3empsmum == -94 ~ -8,
    TRUE ~ as.numeric(W3empsmum)
  ))

# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(ecoactma17 = case_when(
    w4empsmum == 1 ~ 1,
    w4empsmum == 2 ~ 2,
    w4empsmum == 3 ~ 3,
    w4empsmum == 4 ~ 4,
    w4empsmum == 5 ~ 5,
    w4empsmum == 6 ~ 6,
    w4empsmum == 7 ~ 7,
    w4empsmum == 8 ~ 8,
    w4empsmum == 9 ~ 9,
    w4empsmum == -999 ~ -2,
    w4empsmum == -99 ~ -3,
    w4empsmum == -98 ~ -1,
    w4empsmum == -94 ~ -8,
    TRUE ~ as.numeric(w4empsmum)
  ))

# Process father's economic activity for each wave
# Wave 1 (Age 14)
merged_data <- merged_data %>%
  mutate(ecoactpa14 = case_when(
    W1empsdad == 1 ~ 1,
    W1empsdad == 2 ~ 2,
    W1empsdad == 3 ~ 3,
    W1empsdad == 4 ~ 4,
    W1empsdad == 5 ~ 5,
    W1empsdad == 6 ~ 6,
    W1empsdad == 7 ~ 7,
    W1empsdad == 8 ~ 8,
    W1empsdad == 9 ~ 9,
    W1empsdad == -999 ~ -2,
    W1empsdad == -99 ~ -3,
    W1empsdad == -98 ~ -1,
    W1empsdad == -94 ~ -8,
    TRUE ~ as.numeric(W1empsdad)
  ))

# Wave 2 (Age 15)
merged_data <- merged_data %>%
  mutate(ecoactpa15 = case_when(
    W2empsdad == 1 ~ 1,
    W2empsdad == 2 ~ 2,
    W2empsdad == 3 ~ 3,
    W2empsdad == 4 ~ 4,
    W2empsdad == 5 ~ 5,
    W2empsdad == 6 ~ 6,
    W2empsdad == 7 ~ 7,
    W2empsdad == 8 ~ 8,
    W2empsdad == 9 ~ 9,
    W2empsdad == -999 ~ -2,
    W2empsdad == -99 ~ -3,
    W2empsdad == -98 ~ -1,
    W2empsdad == -94 ~ -8,
    TRUE ~ as.numeric(W2empsdad)
  ))

# Wave 3 (Age 16)
merged_data <- merged_data %>%
  mutate(ecoactpa16 = case_when(
    W3empsdad == 1 ~ 1,
    W3empsdad == 2 ~ 2,
    W3empsdad == 3 ~ 3,
    W3empsdad == 4 ~ 4,
    W3empsdad == 5 ~ 5,
    W3empsdad == 6 ~ 6,
    W3empsdad == 7 ~ 7,
    W3empsdad == 8 ~ 8,
    W3empsdad == 9 ~ 9,
    W3empsdad == -999 ~ -2,
    W3empsdad == -99 ~ -3,
    W3empsdad == -98 ~ -1,
    W3empsdad == -94 ~ -8,
    TRUE ~ as.numeric(W3empsdad)
  ))

# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(ecoactpa17 = case_when(
    w4empsdad == 1 ~ 1,
    w4empsdad == 2 ~ 2,
    w4empsdad == 3 ~ 3,
    w4empsdad == 4 ~ 4,
    w4empsdad == 5 ~ 5,
    w4empsdad == 6 ~ 6,
    w4empsdad == 7 ~ 7,
    w4empsdad == 8 ~ 8,
    w4empsdad == 9 ~ 9,
    w4empsdad == -999 ~ -2,
    w4empsdad == -996 ~ -2,
    w4empsdad == -99 ~ -3,
    w4empsdad == -98 ~ -1,
    w4empsdad == -94 ~ -8,
    w4empsdad == -92 ~ -9,
    TRUE ~ as.numeric(w4empsdad)
  ))

# Select only the ID variable and the derived variables
output_data <- merged_data %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"