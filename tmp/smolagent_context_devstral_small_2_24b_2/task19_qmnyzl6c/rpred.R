library(readr)
library(dplyr)
library(haven)

# Load the datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Derive bmi25 from W8DBMI (Wave 8, Age 25) and apply standard missing value codes
merged_data <- merged_data %>%
  mutate(bmi25 = case_when(
    is.na(W8DBMI) ~ -3,
    W8DBMI == -9 ~ -9,
    W8DBMI == -8 ~ -8,
    W8DBMI == -1 ~ -1,
    W8DBMI < 0 ~ -2,
    TRUE ~ W8DBMI
  ))

# Derive bmi32 from W9DBMI (Wave 9, Age 32) and apply standard missing value codes
merged_data <- merged_data %>%
  mutate(bmi32 = case_when(
    is.na(W9DBMI) ~ -3,
    W9DBMI == -9 ~ -9,
    W9DBMI == -8 ~ -8,
    W9DBMI == -1 ~ -1,
    W9DBMI < 0 ~ -2,
    TRUE ~ W9DBMI
  ))

# Select only the ID variable and the derived BMI variables
final_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write the output CSV
write_csv(final_data, "data/output/cleaned_data.csv")
