library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load input files
# Path prefix as requested
path_prefix <- "data/input/"

# 1. File Loading
file1 <- read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols())
file2 <- read_delim(paste0(path_prefix, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols())
file3 <- read_delim(paste0(path_prefix, "ns8_2015_derived.tab"), delim = "\t", col_types = readr::cols())
file4 <- read_delim(paste0(path_prefix, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = readr::cols())

# Merge datasets using full_join by NSID
merged_data <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file3, by = "NSID") %>%
  full_join(file4, by = "NSID")

# Helper function to handle missing values for BMI
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / NA
# -2 = Schedule not applicable / script error
# -1 = Item not applicable

process_bmi <- function(var_vector) {
  # Convert NA to -3
  res <- var_vector
  res[is.na(res)] <- -3
  
  # Standard missing value codes mapping
  # According to metadata for W8DBMI and W9DBMI:
  # -9.0: Refused -> -9
  # -8.0: Insufficient information -> -8
  # -1.0: Not applicable -> -1
  # We keep these as they align with the standard codes provided in the prompt instructions
  
  return(res)
}

# Derive bmi25 (from W8DBMI - Wave 8 corresponds to age 25 in this cohort context)
merged_data <- merged_data %>%
  mutate(bmi25 = process_bmi(W8DBMI))

# Derive bmi32 (from W9DBMI - Wave 9 corresponds to age 32)
merged_data <- merged_data %>%
  mutate(bmi32 = process_bmi(W9DBMI))

# Keep only ID and derived variables
final_df <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write to CSV
readr::write_csv(final_df, "data/output/cleaned_data.csv")
