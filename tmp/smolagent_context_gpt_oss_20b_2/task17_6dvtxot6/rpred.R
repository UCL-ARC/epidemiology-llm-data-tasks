library(readr)
library(dplyr)
library(tidyr)

# Define paths
input_dir <- "data/input/"
output_dir <- "data/output/"
output_file <- file.path(output_dir, "cleaned_data.csv")

# Read necessary variables from each file
# Wave 1 (Age 14) - only NSID
wave1 <- read_delim(file.path(input_dir, "wave_one_lsype_young_person_2020.tab"),
                    delim = "\t",
                    col_select = c("NSID"),
                    col_types = cols(.default = "c"))

# Wave 4 (Age 17) - only NSID
wave4 <- read_delim(file.path(input_dir, "wave_four_lsype_young_person_2020.tab"),
                    delim = "\t",
                    col_select = c("NSID"),
                    col_types = cols(.default = "c"))

# Wave 2 (Age 15) IMDRSCORE
wave2 <- read_delim(file.path(input_dir, "wave_two_lsype_family_background_2020.tab"),
                    delim = "\t",
                    col_select = c("NSID", "IMDRSCORE"),
                    col_types = cols(.default = "c")) %>%
  rename(imd15_raw = IMDRSCORE)

# Wave 3 (Age 16) IMDRSCORE
wave3 <- read_delim(file.path(input_dir, "wave_three_lsype_family_background_2020.tab"),
                    delim = "\t",
                    col_select = c("NSID", "IMDRSCORE"),
                    col_types = cols(.default = "c")) %>%
  rename(imd16_raw = IMDRSCORE)

# Wave 9 (Age 32) W9DIMDD
wave9 <- read_delim(file.path(input_dir, "ns9_2022_derived_variables.tab"),
                    delim = "\t",
                    col_select = c("NSID", "W9DIMDD"),
                    col_types = cols(.default = "c")) %>%
  rename(imd32_raw = W9DIMDD)

# Convert raw columns to numeric
wave2 <- wave2 %>% mutate(imd15_raw = as.numeric(imd15_raw))
wave3 <- wave3 %>% mutate(imd16_raw = as.numeric(imd16_raw))
wave9 <- wave9 %>% mutate(imd32_raw = as.numeric(imd32_raw))

# Merge all waves by NSID
full_df <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Harmonise missing values and preserve scores
final_df <- full_df %>%
  mutate(
    imd15 = case_when(
      is.na(imd15_raw) ~ -3,
      imd15_raw == -94 ~ -8,
      imd15_raw < 0 ~ -2,
      TRUE ~ imd15_raw
    ),
    imd16 = case_when(
      is.na(imd16_raw) ~ -3,
      imd16_raw == -94 ~ -8,
      imd16_raw < 0 ~ -2,
      TRUE ~ imd16_raw
    ),
    imd32 = case_when(
      is.na(imd32_raw) ~ -3,
      imd32_raw == -8 ~ -8,
      imd32_raw < 0 ~ -2,
      TRUE ~ imd32_raw
    )
  ) %>%
  select(NSID, imd15, imd16, imd32)

# Write cleaned data
write_csv(final_df, output_file)
