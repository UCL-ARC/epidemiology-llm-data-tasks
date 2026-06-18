# Load required packages
library(readr)
library(dplyr)
library(tidyr)

# Define file paths
input_dir <- "data/input/"

# Load files
wave_one <- read_delim(file.path(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(), locale = locale(decimal_mark = "."))
wave_four <- read_delim(file.path(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(), locale = locale(decimal_mark = "."))
wave_two <- read_delim(file.path(input_dir, "wave_two_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(), locale = locale(decimal_mark = "."))
wave_three <- read_delim(file.path(input_dir, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(), locale = locale(decimal_mark = "."))
ns9_derived <- read_delim(file.path(input_dir, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = cols(), locale = locale(decimal_mark = "."))

# Extract and clean IMD variables
imd15 <- wave_two %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd15 = IMDRSCORE) %>%
  mutate(imd15 = case_when(
    imd15 == -94 ~ -8,          # Insufficient information
    imd15 < 0 ~ -3,             # Other negative codes treated as missing
    is.na(imd15) ~ -3,          # Convert NA to -3
    TRUE ~ imd15
  ))

imd16 <- wave_three %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd16 = IMDRSCORE) %>%
  mutate(imd16 = case_when(
    imd16 == -94 ~ -8,
    imd16 < 0 ~ -3,
    is.na(imd16) ~ -3,
    TRUE ~ imd16
  ))

imd32 <- ns9_derived %>%
  select(NSID, W9DIMDD) %>%
  rename(imd32 = W9DIMDD) %>%
  mutate(imd32 = case_when(
    imd32 == -8 ~ -8,          # Insufficient information
    is.na(imd32) ~ -3,
    TRUE ~ imd32
  ))

# Merge all datasets to preserve full cohort frame
merged <- full_join(wave_one, wave_four, by = "NSID") %>%
  full_join(., imd15, by = "NSID") %>%
  full_join(., imd16, by = "NSID") %>%
  full_join(., imd32, by = "NSID")

# Select only final derived variables and ID
output_df <- merged %>%
  select(NSID, imd15, imd16, imd32)

# Write to CSV
output_path <- "data/output/cleaned_data.csv"
write_csv(output_df, output_path, na = "")

print(paste("Cleaned data written to", output_path))