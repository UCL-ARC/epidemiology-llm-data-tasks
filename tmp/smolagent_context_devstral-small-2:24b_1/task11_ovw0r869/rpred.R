library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Standard missing value codes
standard_missing <- c(
  `-9` = "Refusal",
  `-8` = "Don't know/insufficient information",
  `-1` = "Item not applicable",
  `-3` = "Not asked at the fieldwork stage/participated/interviewed",
  `-2` = "Schedule not applicable/Script error/information lost",
  `-7` = "Prefer not to say"
)

# Harmonize missing values for parental economic activity
merged_data <- merged_data %>%
  mutate(
    ecoactdtma14 = case_when(
      W1empsmum == -999.0 ~ -3,
      W1empsmum == -99.0 ~ -3,
      W1empsmum == -98.0 ~ -3,
      W1empsmum == -94.0 ~ -8,
      W1empsmum == -92.0 ~ -9,
      TRUE ~ W1empsmum
    ),
    ecoactdtpa14 = case_when(
      W1empsdad == -999.0 ~ -3,
      W1empsdad == -99.0 ~ -3,
      W1empsdad == -98.0 ~ -3,
      W1empsdad == -94.0 ~ -8,
      W1empsdad == -92.0 ~ -9,
      TRUE ~ W1empsdad
    ),
    ecoactdtma15 = case_when(
      W2empsmum == -999.0 ~ -3,
      W2empsmum == -99.0 ~ -3,
      W2empsmum == -98.0 ~ -3,
      W2empsmum == -94.0 ~ -8,
      W2empsmum == -92.0 ~ -9,
      TRUE ~ W2empsmum
    ),
    ecoactdtpa15 = case_when(
      W2empsdad == -999.0 ~ -3,
      W2empsdad == -99.0 ~ -3,
      W2empsdad == -98.0 ~ -3,
      W2empsdad == -94.0 ~ -8,
      W2empsdad == -92.0 ~ -9,
      TRUE ~ W2empsdad
    ),
    ecoactdtma16 = case_when(
      W3empsmum == -999.0 ~ -3,
      W3empsmum == -99.0 ~ -3,
      W3empsmum == -98.0 ~ -3,
      W3empsmum == -94.0 ~ -8,
      W3empsmum == -92.0 ~ -9,
      TRUE ~ W3empsmum
    ),
    ecoactdtpa16 = case_when(
      W3empsdad == -999.0 ~ -3,
      W3empsdad == -99.0 ~ -3,
      W3empsdad == -98.0 ~ -3,
      W3empsdad == -94.0 ~ -8,
      W3empsdad == -92.0 ~ -9,
      TRUE ~ W3empsdad
    ),
    ecoactdtma17 = case_when(
      w4empsmum == -999.0 ~ -3,
      w4empsmum == -99.0 ~ -3,
      w4empsmum == -98.0 ~ -3,
      w4empsmum == -94.0 ~ -8,
      w4empsmum == -92.0 ~ -9,
      TRUE ~ w4empsmum
    ),
    ecoactdtpa17 = case_when(
      w4empsdad == -999.0 ~ -3,
      w4empsdad == -99.0 ~ -3,
      w4empsdad == -98.0 ~ -3,
      w4empsdad == -94.0 ~ -8,
      w4empsdad == -92.0 ~ -9,
      w4empsdad == -996.0 ~ -1,
      TRUE ~ w4empsdad
    )
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, ecoactdtma14, ecoactdtpa14, ecoactdtma15, ecoactdtpa15, ecoactdtma16, ecoactdtpa16, ecoactdtma17, ecoactdtpa17)

# Save output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)