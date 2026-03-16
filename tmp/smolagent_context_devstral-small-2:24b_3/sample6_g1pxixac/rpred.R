library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
ns9_2022_main <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_2015_derived, by = "NSID") %>%
  full_join(ns9_2022_derived, by = "NSID") %>%
  full_join(ns9_2022_main, by = "NSID")

# Standard missing value codes
standard_missing_codes <- c(-9, -8, -1, -3, -2, -7)

# Harmonize missing value codes for numeric columns only
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~ case_when(
    .x %in% c(-100, -97, -999, -998, -997, -995, -94, -92, -91, -99) ~ -3,
    TRUE ~ .x
  )))

# Create derived variables
merged_data <- merged_data %>%
  mutate(
    # Example: Create a binary UK/abroad indicator from W9NATIONRES
    uk_abroad = case_when(
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1,
      W9NATIONRES == 5 ~ 0,
      TRUE ~ NA_integer_
    ) %>%
      factor(levels = c(1, 0), labels = c("UK", "Abroad"))
  )

# Save the cleaned data
write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)
