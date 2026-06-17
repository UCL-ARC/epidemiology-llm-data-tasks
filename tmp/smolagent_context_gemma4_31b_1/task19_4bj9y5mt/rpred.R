library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Prepending data/input/ as per general guidance
# Load all files listed in metadata to preserve full cohort frame
file_w1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file_w4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file_w8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(NSID = "c", W8DBMI = "numeric"))
file_w9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(NSID = "c", W9DBMI = "numeric"))

# 2. Merge datasets using full_join by NSID
full_frame <- file_w1 %>%
  full_join(file_w4, by = "NSID") %>%
  full_join(file_w8, by = "NSID") %>%
  full_join(file_w9, by = "NSID")

# 3. Process BMI variables
# Requirement: bmi25 and bmi32
# Wave 8 corresponds to Age 25; Wave 9 corresponds to Age 32

process_bmi <- function(var, labels) {
  # Map original missing values to standard codes
  # -9 = Refusal, -8 = Don't know/insufficient, -1 = Not applicable
  # NA to -3
  
  res <- var
  
  # The metadata says -9.0, -8.0, -1.0 are already used for these labels in source
  # Let's ensure they are exactly the standard codes
  # -9 Refused, -8 Insufficient, -1 Not applicable
  # We just need to handle NA -> -3
  
  res[is.na(res)] <- -3
  
  return(res)
}

# Apply to W8DBMI (bmi25) and W9DBMI (bmi32)
full_frame <- full_frame %>%
  mutate(
    bmi25 = process_bmi(W8DBMI, NULL),
    bmi32 = process_bmi(W9DBMI, NULL)
  )

# 4. Final Selection
# Keep only NSID and the derived variables
final_data <- full_frame %>%
  select(NSID, bmi25, bmi32)

# 5. Output Requirements
readr::write_csv(final_data, "data/output/cleaned_data.csv")