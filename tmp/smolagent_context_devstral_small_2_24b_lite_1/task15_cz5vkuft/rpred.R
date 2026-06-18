library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the standard missing-value codes
standard_missing_codes <- c(
  `-9` = -9,
  `-8` = -8,
  `-7` = -7,
  `-3` = -3,
  `-2` = -2,
  `-1` = -1
)

# Function to convert wave-specific missing codes to standard codes
convert_missing <- function(var, wave) {
  if (wave == "wave8") {
    var <- case_when(
      var == -1.0 ~ -1,
      is.na(var) ~ -3,
      TRUE ~ var
    )
  } else if (wave == "wave9") {
    var <- case_when(
      var == -1.0 ~ -1,
      is.na(var) ~ -3,
      TRUE ~ var
    )
  }
  return(var)
}

# Process W8DINCB (wave8, age 25)
inc25 <- merged_data$W8DINCB
inc25 <- convert_missing(inc25, "wave8")
inc25 <- case_when(
  inc25 == 1.0 ~ 1,
  inc25 == 2.0 ~ 2,
  inc25 == 3.0 ~ 3,
  inc25 == 4.0 ~ 4,
  inc25 == 5.0 ~ 5,
  inc25 == 6.0 ~ 6,
  inc25 == 7.0 ~ 7,
  inc25 == 8.0 ~ 8,
  inc25 == 9.0 ~ 9,
  inc25 == 10.0 ~ 10,
  inc25 == 11.0 ~ 11,
  inc25 == 12.0 ~ 12,
  inc25 == 13.0 ~ 13,
  inc25 == 14.0 ~ 14,
  inc25 == 15.0 ~ 15,
  inc25 == 16.0 ~ 16,
  TRUE ~ inc25
)

# Process W9DINCB (wave9, age 32)
inc32 <- merged_data$W9DINCB
inc32 <- convert_missing(inc32, "wave9")
inc32 <- case_when(
  inc32 == 1.0 ~ 1,
  inc32 == 2.0 ~ 2,
  inc32 == 3.0 ~ 3,
  inc32 == 4.0 ~ 4,
  inc32 == 5.0 ~ 5,
  inc32 == 6.0 ~ 6,
  inc32 == 7.0 ~ 7,
  inc32 == 8.0 ~ 8,
  inc32 == 9.0 ~ 9,
  inc32 == 10.0 ~ 10,
  inc32 == 11.0 ~ 11,
  inc32 == 12.0 ~ 12,
  inc32 == 13.0 ~ 13,
  inc32 == 14.0 ~ 14,
  inc32 == 15.0 ~ 15,
  inc32 == 16.0 ~ 16,
  TRUE ~ inc32
)

# Create the final dataset with only NSID, inc25, and inc32
cleaned_data <- data.frame(
  NSID = merged_data$NSID,
  inc25 = inc25,
  inc32 = inc32
)

# Write the output CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
