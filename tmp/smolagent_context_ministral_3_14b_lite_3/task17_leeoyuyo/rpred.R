
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required files
wave_one_young_person <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two_family_background <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three_family_background <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four_young_person <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9_derived_variables <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Rename IMDRSCORE columns to avoid ambiguity
wave_two_family_background <- wave_two_family_background %>%
  rename(IMDRSCORE_wave2 = IMDRSCORE)

wave_three_family_background <- wave_three_family_background %>%
  rename(IMDRSCORE_wave3 = IMDRSCORE)

# Merge datasets by NSID
cleaned_data <- wave_one_young_person %>%
  full_join(wave_two_family_background, by = "NSID") %>%
  full_join(wave_three_family_background, by = "NSID") %>%
  full_join(wave_four_young_person, by = "NSID") %>%
  full_join(ns9_derived_variables, by = "NSID")

# Derive imd15 (Age 15) from wave_two_family_background IMDRSCORE_wave2
cleaned_data <- cleaned_data %>%
  mutate(imd15 = case_when(
    IMDRSCORE_wave2 == -94 ~ -8,
    !is.na(IMDRSCORE_wave2) ~ IMDRSCORE_wave2,
    TRUE ~ -3
  )) %>%
  select(-IMDRSCORE_wave2)

# Derive imd16 (Age 16) from wave_three_family_background IMDRSCORE_wave3
cleaned_data <- cleaned_data %>%
  mutate(imd16 = case_when(
    IMDRSCORE_wave3 == -94 ~ -8,
    !is.na(IMDRSCORE_wave3) ~ IMDRSCORE_wave3,
    TRUE ~ -3
  )) %>%
  select(-IMDRSCORE_wave3)

# Derive imd32 (Age 32) from ns9_derived_variables W9DIMDD
cleaned_data <- cleaned_data %>%
  mutate(imd32 = case_when(
    W9DIMDD == -8 ~ -8,
    !is.na(W9DIMDD) ~ W9DIMDD,
    TRUE ~ -3
  ))

# Select only NSID and derived variables
final_output <- cleaned_data %>%
  select(NSID, imd15, imd16, imd32)

# Print summary of the final dataset
print(summary(final_output))

# Write the output to CSV
output_path <- "data/output/cleaned_data.csv"
readr::write_csv(final_output, output_path)

# Confirm file creation
message(paste("Data successfully written to:", output_path))
