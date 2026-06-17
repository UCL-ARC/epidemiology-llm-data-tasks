# Load libraries
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load all files from the metadata
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load each file explicitly by name
w1 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
w4 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
w8 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
w9 <- read_delim(files[4], delim = "\t", show_col_types = FALSE)

# Merge datasets using full_join by NSID
cleaned <- full_join(w1, w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Derive bmi25 from W8DBMI (Wave 8, Age 25)
# Apply standard missing value codes to negative source values
# Source has: -9 (Refused), -8 (Insufficient information), -1 (Not applicable)
# These already match standard codes, so preserve them
# Convert any R NA to -3 (Not asked)

cleaned <- cleaned %>%
  mutate(
    bmi25 = case_when(
      is.na(W8DBMI) ~ -3,
      W8DBMI < 0 ~ W8DBMI,  # preserve negative codes as-is since they match standard
      TRUE ~ W8DBMI
    ),
    bmi32 = case_when(
      is.na(W9DBMI) ~ -3,
      W9DBMI < 0 ~ W9DBMI,  # preserve negative codes as-is since they match standard
      TRUE ~ W9DBMI
    )
  )

# Keep only NSID and the derived BMI variables
final <- cleaned %>%
  select(NSID, bmi25, bmi32)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(final, "data/output/cleaned_data.csv")

# Verify output
cat("Output shape:", dim(final), "\n")
cat("Number of rows:", nrow(final), "\n")
cat("Sample rows:\n")
print(head(final, 10))
cat("\nMissing value distribution for bmi25:\n")
table(final$bmi25, useNA = "ifany")
cat("\nMissing value distribution for bmi32:\n")
table(final$bmi32, useNA = "ifany")
