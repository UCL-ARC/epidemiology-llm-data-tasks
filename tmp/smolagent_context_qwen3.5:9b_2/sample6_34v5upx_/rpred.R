library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Define file paths
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8 = "data/input/ns8_2015_derived.tab",
  ns9_derived = "data/input/ns9_2022_derived_variables.tab",
  ns9_main = "data/input/ns9_2022_main_interview.tab"
)

# Function to convert missing values to standard codes for a single column
code_missing <- function(col, standard_code) {
  if (is.factor(col)) {
    return(col)
  }
  col <- as.integer(col)
  # Replace non-standard negative codes with standard codes
  col[col < 0] <- standard_code
  return(col)
}

# Load wave 1 (Age 14)
wave1 <- read_delim(files$wave_one, delim = "\t", col_types = cols())

# Load wave 2 (Age 15)
wave2 <- read_delim(files$wave_two, delim = "\t", col_types = cols())

# Convert wave 2 variables - map user_missing_values to standard codes
wave2_converted <- wave2 %>%
  mutate(
    urbind = code_missing(urbind, -3),
    gor = code_missing(gor, -3)
  )

# Load wave 3 (Age 16)
wave3 <- read_delim(files$wave_three, delim = "\t", col_types = cols())

# Convert wave 3 variables
wave3_converted <- wave3 %>%
  mutate(
    urbind = code_missing(urbind, -3),
    gor = code_missing(gor, -3)
  )

# Load wave 4 (Age 17)
wave4 <- read_delim(files$wave_four, delim = "\t", col_types = cols())

# Load ns8 (Wave 8)
ns8 <- read_delim(files$ns8, delim = "\t", col_types = cols())

# Convert wave 8 variable
ns8_converted <- ns8 %>%
  mutate(
    W8DGOR = code_missing(W8DGOR, -3)
  )

# Load ns9_derived (Age 32)
ns9_derived <- read_delim(files$ns9_derived, delim = "\t", col_types = cols())

# Convert wave 9 derived variable
ns9_derived_converted <- ns9_derived %>%
  mutate(
    W9DRGN = code_missing(W9DRGN, -3)
  )

# Load ns9_main (Age 32)
ns9_main <- read_delim(files$ns9_main, delim = "\t", col_types = cols())

# Convert wave 9 main interview variables
ns9_main_converted <- ns9_main %>%
  mutate(
    W9NATIONRES = code_missing(W9NATIONRES, -3)
  )

# Merge all datasets using full_join by NSID
# Use select to ensure NSID is properly named before joining
merged_data <- full_join(wave1, wave2_converted, by = "NSID", suffix = c(".w1", ".w2"))

# Rename NSID to single name
colnames(merged_data)[colnames(merged_data) == "NSID.w1"] <- "NSID"

merged_data <- full_join(merged_data, wave3_converted, by = "NSID", suffix = c(".w1", ".w2"))

merged_data <- full_join(merged_data, wave4, by = "NSID", suffix = c(".w1", ".w2"))

merged_data <- full_join(merged_data, ns8_converted, by = "NSID", suffix = c(".w1", ".w2"))

merged_data <- full_join(merged_data, ns9_derived_converted, by = "NSID", suffix = c(".w1", ".w2"))

merged_data <- full_join(merged_data, ns9_main_converted, by = "NSID", suffix = c(".w1", ".w2"))

# Convert NSID to character to ensure unique identification
merged_data$NSID <- as.character(merged_data$NSID)

cat("After merge:\n")
cat("Variables count:", ncol(merged_data), "\n")
cat("Row count:", nrow(merged_data), "\n")

# Write to CSV
write_csv(merged_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Total cases:", nrow(merged_data), "\n")
cat("Total variables:", ncol(merged_data), "\n")
