library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave_one, wave_four, by = "NSID")
merged_data <- full_join(merged_data, ns8_2015, by = "NSID")
merged_data <- full_join(merged_data, ns9_2022, by = "NSID")

# Harmonize missing values
merged_data <- merged_data %>%
  mutate(
    bmi25 = case_when(
      W8DBMI %in% c(-9, -8, -1) ~ W8DBMI,
      W8DBMI < 0 & W8DBMI != -9 & W8DBMI != -8 & W8DBMI != -1 ~ -3,
      is.na(W8DBMI) ~ -3,
      TRUE ~ W8DBMI
    ),
    bmi32 = case_when(
      W9DBMI %in% c(-9, -8, -1) ~ W9DBMI,
      W9DBMI < 0 & W9DBMI != -9 & W9DBMI != -8 & W9DBMI != -1 ~ -3,
      is.na(W9DBMI) ~ -3,
      TRUE ~ W9DBMI
    )
  )

# Select and output required variables
cleaned_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)