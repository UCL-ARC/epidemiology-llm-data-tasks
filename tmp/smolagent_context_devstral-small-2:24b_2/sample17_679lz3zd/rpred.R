library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_three, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")
merged_data <- full_join(merged_data, ns9_2022, by = "NSID")

# Select and rename variables
merged_data <- merged_data %>%
  select(NSID, IMDRSCORE, W9DIMDD) %>%
  rename(imd15 = IMDRSCORE, imd16 = IMDRSCORE, imd32 = W9DIMDD)

# Harmonize missing values
merged_data <- merged_data %>%
  mutate(across(c(imd15, imd16, imd32), ~ case_when(
    .x == -94 ~ -8,
    is.na(.x) ~ -3,
    TRUE ~ .x
  )))

# Label variables
merged_data <- merged_data %>%
  mutate(across(c(imd15, imd16, imd32), ~ labelled(.x, labels = c("-9" = "Refusal", "-8" = "Don't know / insufficient information", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Script error / information lost", "-1" = "Not applicable"))))

# Output cleaned data
write_csv(merged_data, "data/output/cleaned_data.csv")