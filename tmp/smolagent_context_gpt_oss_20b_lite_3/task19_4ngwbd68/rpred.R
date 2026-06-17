library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Load data files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols())
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols())

# Merge by NSID
full_df <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Create raw BMI columns
full_df <- full_df %>%
  mutate(bmi25_raw = W8DBMI,
         bmi32_raw = W9DBMI)

# Harmonise missing values according to the standard scheme
harmonise_missing <- function(x) {
  # Replace R NA with -3 (Not asked)
  x <- ifelse(is.na(x), -3, x)
  as.numeric(x)
}

full_df <- full_df %>%
  mutate(bmi25 = harmonise_missing(bmi25_raw),
         bmi32 = harmonise_missing(bmi32_raw))

# Select final variables
final_df <- full_df %>%
  select(NSID, bmi25, bmi32)

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")