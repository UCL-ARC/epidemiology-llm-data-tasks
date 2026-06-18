library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Define the mapping for income bands
map_income_bands <- function(data, income_var) {
  data <- data %>%
    mutate("{{income_var}}" = case_when(
      {{income_var}} == -1.0 ~ -1,  # Not applicable
      {{income_var}} == 1.0 ~ 1,   # less than 25
      {{income_var}} == 2.0 ~ 2,   # 25 to 50
      {{income_var}} == 3.0 ~ 3,   # 50 to 90
      {{income_var}} == 4.0 ~ 4,   # 90 to 140
      {{income_var}} == 5.0 ~ 5,   # 140 to 240
      {{income_var}} == 6.0 ~ 6,   # 240 to 300
      {{income_var}} == 7.0 ~ 7,   # 300 to 350
      {{income_var}} == 8.0 ~ 8,   # 350 to 400
      {{income_var}} == 9.0 ~ 9,   # 400 to 500
      {{income_var}} == 10.0 ~ 10, # 500 to 600
      {{income_var}} == 11.0 ~ 11, # 600 to 700
      {{income_var}} == 12.0 ~ 12, # 700 to 800
      {{income_var}} == 13.0 ~ 13, # 800 to 900
      {{income_var}} == 14.0 ~ 14, # 900 to 1200
      {{income_var}} == 15.0 ~ 15, # 1200 to 1400
      {{income_var}} == 16.0 ~ 16, # more than 1400
      is.na({{income_var}}) ~ -3, # Not interviewed
      TRUE ~ as.numeric({{income_var}}) # Retain other codes as-is
    ))
  
  return(data)
}

# Apply mapping to income variables
merged_data <- map_income_bands(merged_data, W8DINCB)
merged_data <- map_income_bands(merged_data, W9DINCB)

# Rename variables to inc25 and inc32
merged_data <- merged_data %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

# Select only NSID and derived variables
output_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")