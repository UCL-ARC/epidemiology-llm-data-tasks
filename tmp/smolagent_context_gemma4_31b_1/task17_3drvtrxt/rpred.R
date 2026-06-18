library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load specific columns needed from each file to avoid naming conflicts and memory issues
# Age 15: wave_two_lsype_family_background_2020.tab -> IMDRSCORE
load_imd_15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = "c", IMDRSCORE = "d")) %>%
  select(NSID, imd15_raw = IMDRSCORE)

# Age 16: wave_three_lsype_family_background_2020.tab -> IMDRSCORE
load_imd_16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = "c", IMDRSCORE = "d")) %>%
  select(NSID, imd16_raw = IMDRSCORE)

# Age 32: ns9_2022_derived_variables.tab -> W9DIMDD
load_imd_32 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols(NSID = "c", W9DIMDD = "d")) %>%
  select(NSID, imd32_raw = W9DIMDD)

# Load files to preserve full cohort frame
cohort_w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = "c")) %>% select(NSID)
cohort_w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = "c")) %>% select(NSID)

# Merge datasets using full_join to maintain the full frame
final_df <- cohort_w1 %>%
  full_join(cohort_w4, by = "NSID") %>%
  full_join(load_imd_15, by = "NSID") %>%
  full_join(load_imd_16, by = "NSID") %>%
  full_join(load_imd_32, by = "NSID")

# Harmonize Missing Values
# For IMD 15 and 16 (IMDRSCORE):
# -94.0 = Insufficient Information -> -8
# -999.0 thru -1.0 = Other missing/NA -> -2 (General mapping for these ranges)
# R NA -> -3

final_df <- final_df %>%
  mutate(
    imd15 = case_when(
      is.na(imd15_raw) ~ -3,
      imd15_raw == -94 ~ -8,
      imd15_raw <= -1 ~ -2,
      TRUE ~ imd15_raw
    ),
    imd16 = case_when(
      is.na(imd16_raw) ~ -3,
      imd16_raw == -94 ~ -8,
      imd16_raw <= -1 ~ -2,
      TRUE ~ imd16_raw
    ),
    imd32 = case_when(
      is.na(imd32_raw) ~ -3,
      imd32_raw == -8 ~ -8,
      TRUE ~ imd32_raw
    )
  )

# Final selection of variables
final_df <- final_df %>%
  select(NSID, imd15, imd16, imd32)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")