library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from metadata
# Load each file explicitly by name into a separate object

# Wave 1 (Age 14) - Young Person Data File
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab",
                     delim = "\t",
                     show_col_types = FALSE)

# Wave 4 (Age 17) - Young Person Data File
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab",
                     delim = "\t",
                     show_col_types = FALSE)

# Wave 8 (Age 25) - Derived Variables Data File
w8 <- read_delim("data/input/ns8_2015_derived.tab",
                  delim = "\t",
                  show_col_types = FALSE)

# Wave 9 (Age 32) - Derived Variables Data File
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab",
                  delim = "\t",
                  show_col_types = FALSE)

# Merge datasets using full_join by NSID
df <- full_join(wave1, wave4, by = "NSID")
df <- full_join(df, w8, by = "NSID")
df <- full_join(df, w9, by = "NSID")

# Create bmi25 from W8DBMI (Age 25)
# Convert missing values according to standard scheme
bmi25_raw <- df$W8DBMI

# Replace R NA with -3 (Not asked at fieldwork stage)
bmi25_raw[is.na(bmi25_raw)] <- -3

# W8DBMI has -9, -8, -1 as user missing values (already match standard codes)
# Keep as numeric since BMI can be negative (though unlikely in practice)
bmi25 <- bmi25_raw

# Create bmi32 from W9DBMI (Age 32)
bmi32_raw <- df$W9DBMI

# Replace R NA with -3 (Not asked at fieldwork stage)
bmi32_raw[is.na(bmi32_raw)] <- -3

# W9DBMI has -9, -8, -1 as user missing values (already match standard codes)
# Keep as numeric since BMI can be negative (though unlikely in practice)
bmi32 <- bmi32_raw

# Create output dataframe with ID and final derived variables only
output <- df %>%
  mutate(bmi25 = bmi25,
         bmi32 = bmi32) %>%
  select(NSID, all_of(c("bmi25", "bmi32")))

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
