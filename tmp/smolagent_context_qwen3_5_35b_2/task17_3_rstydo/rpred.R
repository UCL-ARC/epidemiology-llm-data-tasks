library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Rename IMDRSCORE in wave_two and wave_three to keep them distinct before merging
wave_two <- wave_two %>% rename(IMDRSCORE_15 = IMDRSCORE)
wave_three <- wave_three %>% rename(IMDRSCORE_16 = IMDRSCORE)

# Create base cohort frame from wave one
all_ids <- select(wave_one, NSID)

# Merge all files by NSID using full_join
merged <- all_ids %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(ns9, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Check if IMDRSCORE_15 and IMDRSCORE_16 exist
cat("Columns in merged:\n")
print(names(merged))

cat("\nFirst 10 values of IMDRSCORE_15:\n")
print(head(merged$IMDRSCORE_15, 10))

cat("\nFirst 10 values of IMDRSCORE_16:\n")
print(head(merged$IMDRSCORE_16, 10))

# Derive IMD variables from metadata:
# - wave_two (age 15): IMDRSCORE = 2004 Index of Multiple Deprivation ROUNDED SCORE
# - wave_three (age 16): IMDRSCORE = 2004 Index of Multiple Deprivation ROUNDED SCORE  
# - ns9 (age 32): W9DIMDD = 2019 Index of Multiple Deprivation rank decile

# Handle missing values according to metadata:
# IMDRSCORE: -94 = Insufficient Information -> -8
# IMDRSCORE: other negative codes (<0) -> -2 (schedule not applicable)
# W9DIMDD: -8 = Insufficient information -> -8

merged <- merged %>%
  mutate(
    # imd15 from wave_two IMDRSCORE
    imd15 = case_when(
      IMDRSCORE_15 == -94 ~ -8,  # Insufficient Information
      IMDRSCORE_15 < 0 ~ -2,      # Other negative codes -> schedule not applicable
      TRUE ~ IMDRSCORE_15         # Valid scores preserved
    ),
    
    # imd16 from wave_three IMDRSCORE
    imd16 = case_when(
      IMDRSCORE_16 == -94 ~ -8,  # Insufficient Information
      IMDRSCORE_16 < 0 ~ -2,      # Other negative codes -> schedule not applicable
      TRUE ~ IMDRSCORE_16         # Valid scores preserved
    ),
    
    # imd32 from ns9 W9DIMDD (2019 decile)
    imd32 = case_when(
      W9DIMDD == -8 ~ -8,      # Insufficient information
      TRUE ~ W9DIMDD           # Valid decile scores preserved
    )
  )

# Keep only NSID and final derived variables
output <- select(merged, NSID, imd15, imd16, imd32)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")
cat("\nFirst 10 rows:\n")
print(head(output, 10))
