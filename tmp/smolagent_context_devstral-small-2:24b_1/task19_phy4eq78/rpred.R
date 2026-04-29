library(dplyr)
library(purrr)
library(readr)

# Define file paths
wave_one_path <- "data/input/wave_one_lsype_young_person_2020.tab"
wave_four_path <- "data/input/wave_four_lsype_young_person_2020.tab"
ns8_path <- "data/input/ns8_2015_derived.tab"
ns9_path <- "data/input/ns9_2022_derived_variables.tab"

# Load datasets
wave_one <- readr::read_delim(wave_one_path, delim = "\t")
wave_four <- readr::read_delim(wave_four_path, delim = "\t")
ns8 <- readr::read_delim(ns8_path, delim = "\t")
ns9 <- readr::read_delim(ns9_path, delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Harmonize missing values for BMI variables
merged_data <- merged_data %>%
  mutate(
    bmi25 = case_when(
      W8DBMI %in% c(-9, -8, -1) ~ W8DBMI,
      W8DBMI < 0 ~ -3,
      TRUE ~ W8DBMI
    ),
    bmi32 = case_when(
      W9DBMI %in% c(-9, -8, -1) ~ W9DBMI,
      W9DBMI < 0 ~ -3,
      TRUE ~ W9DBMI
    )
  ) %>%
  select(NSID, bmi25, bmi32)

# Write the cleaned dataset to CSV
write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)
