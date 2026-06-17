library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load all files
data_list <- map(files, ~read_delim(paste0("data/input/", .x), delim = "\t", col_types = cols(.default = col_guess())))

# Name the list elements
names(data_list) <- files

# Merge all datasets by NSID
cleaned_data <- reduce(data_list, full_join, by = "NSID")

# Create bmi25 from W8DBMI (Wave 8, Age 25)
cleaned_data <- cleaned_data %>%
  mutate(
    bmi25 = case_when(
      W8DBMI == -9 ~ -9,  # Refused
      W8DBMI == -8 ~ -8,  # Insufficient information
      W8DBMI == -1 ~ -1,  # Not applicable
      is.na(W8DBMI) ~ -3, # Not asked at fieldwork stage
      TRUE ~ W8DBMI       # Valid BMI value
    )
  )

# Create bmi32 from W9DBMI (Wave 9, Age 32)
cleaned_data <- cleaned_data %>%
  mutate(
    bmi32 = case_when(
      W9DBMI == -9 ~ -9,  # Refused
      W9DBMI == -8 ~ -8,  # Insufficient information
      W9DBMI == -1 ~ -1,  # Not applicable
      is.na(W9DBMI) ~ -3, # Not asked at fieldwork stage
      TRUE ~ W9DBMI       # Valid BMI value
    )
  )

# Keep only NSID and final BMI variables
output_data <- cleaned_data %>%
  select(NSID, bmi25, bmi32)

# Write to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
