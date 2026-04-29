# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load data files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID using full_join
merged_data <- full_join(wave1, wave4, by = "NSID")
merged_data <- full_join(merged_data, wave8, by = "NSID")
merged_data <- full_join(merged_data, wave9, by = "NSID")

# Select only income variables from waves 8 and 9
income_data <- merged_data %>%
  select(NSID, W8DINCB, W9DINCB)

# Rename variables to inc25 and inc32
income_data <- income_data %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

# Define income category labels (from metadata)
income_labels <- c(
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

# Define standard missing value labels
missing_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked/interviewed",
  "-2" = "Script error/lost"
)

# Function to recode and label income variables
recode_income <- function(x) {
  # Replace NA with -3 (not asked/interviewed)
  x[is.na(x)] <- -3
  
  # Convert to factor with labels
  factor(x, 
         levels = c(-9, -8, -3, -2, -1, 1:16),
         labels = c(missing_labels, income_labels))
}

# Apply recoding to income variables
income_data$inc25 <- recode_income(income_data$inc25)
income_data$inc32 <- recode_income(income_data$inc32)

# Write output CSV
write_csv(income_data, "data/output/cleaned_data.csv")

cat("Cleaning complete. Output written to data/output/cleaned_data.csv\n")