library(haven)
library(dplyr)
library(purrr)
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

# Harmonize missing values for BMI at age 25 (W8DBMI)
merged_data <- merged_data %>% 
  mutate(bmi25 = case_when(
    W8DBMI >= 0 ~ W8DBMI,  # Retain valid BMI values
    W8DBMI == -9 ~ -9,    # Refusal
    W8DBMI == -8 ~ -8,    # Don't know/insufficient information
    W8DBMI == -1 ~ -1,    # Item not applicable
    W8DBMI == -3 ~ -3,    # Not asked/interviewed
    W8DBMI == -2 ~ -2,    # Script error/lost
    TRUE ~ -3             # Any other negative value or NA -> -3
  ))

# Harmonize missing values for BMI at age 32 (W9DBMI)
merged_data <- merged_data %>% 
  mutate(bmi32 = case_when(
    W9DBMI >= 0 ~ W9DBMI,  # Retain valid BMI values
    W9DBMI == -9 ~ -9,    # Refusal
    W9DBMI == -8 ~ -8,    # Don't know/insufficient information
    W9DBMI == -1 ~ -1,    # Item not applicable
    W9DBMI == -3 ~ -3,    # Not asked/interviewed
    W9DBMI == -2 ~ -2,    # Script error/lost
    TRUE ~ -3             # Any other negative value or NA -> -3
  ))

# Select and output required variables
cleaned_data <- merged_data %>% 
  select(NSID, bmi25, bmi32)

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)