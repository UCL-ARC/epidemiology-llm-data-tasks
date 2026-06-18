library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Define standard missing-value codes
standardize_missing <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -999 | x == -998 | x == -997 | x == -995 ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    x == -100 | x == -97 ~ -7,
    TRUE ~ x
  )
}

# Derive partnr19 from W6MarStatYP
partnr19 <- merged_data$W6MarStatYP
partnr19 <- standardize_missing(partnr19)
partnr19 <- case_when(
  partnr19 == 1 ~ 1,
  partnr19 == 2 ~ 2,
  partnr19 == 3 ~ 3,
  partnr19 == 4 ~ 4,
  partnr19 == 5 ~ 5,
  partnr19 == -1 ~ -1,
  partnr19 == -2 ~ -2,
  partnr19 == -3 ~ -3,
  partnr19 == -7 ~ -7,
  partnr19 == -8 ~ -8,
  partnr19 == -9 ~ -9,
  TRUE ~ -3
)

# Derive detailed adult variables partnradu25 from W8DMARSTAT and partnradu32 from W9DMARSTAT
partnradu25 <- merged_data$W8DMARSTAT
partnradu25 <- standardize_missing(partnradu25)
partnradu25 <- case_when(
  partnradu25 == 1 ~ 1,
  partnradu25 == 2 ~ 2,
  partnradu25 == 3 ~ 3,
  partnradu25 == 4 ~ 4,
  partnradu25 == 5 ~ 5,
  partnradu25 == 6 ~ 6,
  partnradu25 == 7 ~ 7,
  partnradu25 == 8 ~ 8,
  partnradu25 == 9 ~ 9,
  partnradu25 == -1 ~ -1,
  partnradu25 == -2 ~ -2,
  partnradu25 == -3 ~ -3,
  partnradu25 == -7 ~ -7,
  partnradu25 == -8 ~ -8,
  partnradu25 == -9 ~ -9,
  TRUE ~ -3
)

partnradu32 <- merged_data$W9DMARSTAT
partnradu32 <- standardize_missing(partnradu32)
partnradu32 <- case_when(
  partnradu32 == 1 ~ 1,
  partnradu32 == 2 ~ 2,
  partnradu32 == 3 ~ 3,
  partnradu32 == 4 ~ 4,
  partnradu32 == 5 ~ 5,
  partnradu32 == 6 ~ 6,
  partnradu32 == 7 ~ 7,
  partnradu32 == 8 ~ 8,
  partnradu32 == -1 ~ -1,
  partnradu32 == -2 ~ -2,
  partnradu32 == -3 ~ -3,
  partnradu32 == -7 ~ -7,
  partnradu32 == -8 ~ -8,
  partnradu32 == -9 ~ -9,
  TRUE ~ -3
)

# Collapse detailed adult variables into comparable partnr25 and partnr32 categories
partnr25 <- case_when(
  partnradu25 == 1 ~ 1,
  partnradu25 == 2 ~ 2,
  partnradu25 == 3 ~ 3,
  partnradu25 == 4 ~ 4,
  partnradu25 == 5 ~ 5,
  partnradu25 == 6 ~ 2,
  partnradu25 == 7 ~ 3,
  partnradu25 == 8 ~ 4,
  partnradu25 == 9 ~ 5,
  partnradu25 == -1 ~ -1,
  partnradu25 == -2 ~ -2,
  partnradu25 == -3 ~ -3,
  partnradu25 == -7 ~ -7,
  partnradu25 == -8 ~ -8,
  partnradu25 == -9 ~ -9,
  TRUE ~ -3
)

partnr32 <- case_when(
  partnradu32 == 1 ~ 1,
  partnradu32 == 2 ~ 2,
  partnradu32 == 3 ~ 4,
  partnradu32 == 4 ~ 3,
  partnradu32 == 5 ~ 5,
  partnradu32 == 6 ~ 2,
  partnradu32 == 7 ~ 4,
  partnradu32 == 8 ~ 5,
  partnradu32 == -1 ~ -1,
  partnradu32 == -2 ~ -2,
  partnradu32 == -3 ~ -3,
  partnradu32 == -7 ~ -7,
  partnradu32 == -8 ~ -8,
  partnradu32 == -9 ~ -9,
  TRUE ~ -3
)

# Create the final dataset with only the required variables
final_data <- tibble(
  NSID = merged_data$NSID,
  partnr19 = partnr19,
  partnr25 = partnr25,
  partnr32 = partnr32,
  partnradu25 = partnradu25,
  partnradu32 = partnradu32
)

# Write the final dataset to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"