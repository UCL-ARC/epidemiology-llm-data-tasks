# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID using full_join
merged_data <- wave1 %>%
  select(NSID) %>%
  full_join(wave4 %>% select(NSID), by = "NSID") %>%
  full_join(ns8 %>% select(NSID, W8DBMI), by = "NSID") %>%
  full_join(ns9 %>% select(NSID, W9DBMI), by = "NSID")

# Rename BMI variables to standard names
merged_data <- merged_data %>%
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

# Function to harmonize missing value codes
harmonize_bmi <- function(x) {
  # Convert NA to -3 (Not asked/interviewed)
  x[is.na(x)] <- -3
  
  # Map wave-specific codes to standard codes
  # -9 = Refusal (keep as -9)
  # -8 = Don't know/insufficient information (keep as -8)
  # -1 = Item not applicable (keep as -1)
  # Any other negative values -> -3 (Not asked/interviewed)
  x[x < -9 | (x < 0 & x != -9 & x != -8 & x != -1)] <- -3
  
  return(x)
}

# Apply harmonization to BMI variables
merged_data <- merged_data %>%
  mutate(
    bmi25 = harmonize_bmi(bmi25),
    bmi32 = harmonize_bmi(bmi32)
  )

# Apply labels for standard missing codes
missing_labels <- c(
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Item not applicable" = -1,
  "Not asked/interviewed" = -3,
  "Script error/lost" = -2
)

merged_data <- merged_data %>%
  mutate(
    bmi25 = labelled(bmi25, labels = missing_labels),
    bmi32 = labelled(bmi32, labels = missing_labels)
  )

# Select only required variables
final_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write output CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of records:", nrow(final_data), "\n")
cat("Variables:", paste(names(final_data), collapse = ", "), "\n")
cat("Output written to: data/output/cleaned_data.csv\n")