
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Step 1: Load all datasets from metadata
wave1_data <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4_data <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_data <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_data <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Step 2: Merge datasets using full_join by NSID
merged_data <- full_join(wave1_data, wave4_data, by = "NSID") %>%
  full_join(ns8_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

# Step 3: Identify and clean target variables (inc25 and inc32)
# For inc25 (Age 25, Wave 8)
merged_data <- merged_data %>%
  mutate(
    inc25 = case_when(
      W8DINCB == -1 ~ -1,  # Not applicable
      is.na(W8DINCB) ~ -3,  # Convert NA to -3 (Not asked)
      TRUE ~ as.numeric(W8DINCB)  # Ensure numeric type
    )
  )

# Create labelled factor for inc25
merged_data$inc25 <- factor(
  merged_data$inc25,
  levels = c(-1, -3, 1:16),
  labels = c(
    "Not applicable", "Missing",  # Labels for -1 and -3
    "less than 25", "25 to 50", "50 to 90", "90 to 140", "140 to 240", "240 to 300", "300 to 350", "350 to 400", "400 to 500", "500 to 600", "600 to 700", "700 to 800", "800 to 900", "900 to 1200", "1200 to 1400", "more than 1400"
  )
)

# For inc32 (Age 32, Wave 9)
merged_data <- merged_data %>%
  mutate(
    inc32 = case_when(
      W9DINCB == -1 ~ -1,  # Not applicable
      is.na(W9DINCB) ~ -3,  # Convert NA to -3 (Not asked)
      TRUE ~ as.numeric(W9DINCB)  # Ensure numeric type
    )
  )

# Create labelled factor for inc32
merged_data$inc32 <- factor(
  merged_data$inc32,
  levels = c(-1, -3, 1:16),
  labels = c(
    "Not applicable", "Missing",  # Labels for -1 and -3
    "less than 25", "25 to 50", "50 to 90", "90 to 140", "140 to 240", "240 to 300", "300 to 350", "350 to 400", "400 to 500", "500 to 600", "600 to 700", "700 to 800", "800 to 900", "900 to 1200", "1200 to 1400", "more than 1400"
  )
)

# Step 4: Select only required columns (NSID, inc25, inc32)
final_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Step 5: Write output to CSV
file_path <- "data/output/cleaned_data.csv"
readr::write_csv(final_data, file_path)

# Confirmation
cat("Data cleaning and preprocessing completed successfully!")
cat("Output file path:", file_path, "\n")
cat("Number of rows in output:", nrow(final_data), "\n")
cat("Number of columns in output:", ncol(final_data), "\n")
