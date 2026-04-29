# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the input files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets using full_join by NSID
merged_data <- full_join(wave1, wave4, by = "NSID")
merged_data <- full_join(merged_data, wave8, by = "NSID")
merged_data <- full_join(merged_data, wave9, by = "NSID")

# Create bmi25 from W8DBMI (Wave 8 = Age 25)
bmi25 <- merged_data$W8DBMI

# Create bmi32 from W9DBMI (Wave 9 = Age 32)
bmi32 <- merged_data$W9DBMI

# Function to harmonize missing value codes
harmonize_missing <- function(x) {
  # Keep valid positive values as is
  # Map explicit missing codes (-9, -8, -1) directly
  # Recode NA and other negative values to -3 (not asked/interviewed)
  x[is.na(x)] <- -3
  return(x)
}

# Apply harmonization
bmi25 <- harmonize_missing(bmi25)
bmi32 <- harmonize_missing(bmi32)

# Create final dataset with only required variables
cleaned_data <- data.frame(
  NSID = merged_data$NSID,
  bmi25 = bmi25,
  bmi32 = bmi32
)

# Write output to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")

# Verify output
output <- read_csv("data/output/cleaned_data.csv")
cat("Output dimensions:", dim(output), "\n")
cat("First 5 rows:\n")
print(head(output, 5))
