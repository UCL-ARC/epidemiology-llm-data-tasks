library(readr)
library(dplyr)
library(haven)

# Load all files listed in the metadata
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Function to apply standard missing value codes
apply_missing_codes <- function(var) {
  case_when(
    var < 0 & var >= -9 & var <= -8 ~ -8,  # Insufficient information
    var < 0 & var == -9 ~ -9,              # Refused
    var < 0 & var == -1 ~ -1,              # Not applicable
    var < 0 & var == -3 ~ -3,              # Not asked / not interviewed
    var < 0 & var == -2 ~ -2,              # Schedule not applicable
    var < 0 & var == -7 ~ -7,              # Prefer not to say
    is.na(var) ~ -3,                      # Default for NA
    TRUE ~ var                            # Keep valid values
  )
}

# Derive bmi25 from W8DBMI (Wave 8, Age 25)
bmi25 <- apply_missing_codes(merged_data$W8DBMI)

# Derive bmi32 from W9DBMI (Wave 9, Age 32)
bmi32 <- apply_missing_codes(merged_data$W9DBMI)

# Create final dataset with only NSID and derived variables
cleaned_data <- tibble(
  NSID = merged_data$NSID,
  bmi25 = bmi25,
  bmi32 = bmi32
)

# Write output CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
