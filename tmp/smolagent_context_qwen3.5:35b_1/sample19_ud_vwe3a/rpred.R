# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load input files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all files by NSID using full_join
merged <- full_join(wave1, wave4, by = "NSID")
merged <- full_join(merged, wave8, by = "NSID")
merged <- full_join(merged, wave9, by = "NSID")

# Function to harmonize missing value codes
harmonize_missing <- function(x) {
  case_when(
    x == -9 ~ -9,  # Refusal
    x == -8 ~ -8,  # Don't know/insufficient information
    x == -1 ~ -1,  # Item not applicable
    x == -2 ~ -2,  # Script error/lost
    is.na(x) ~ -3, # Not asked/interviewed
    TRUE ~ x       # Valid positive BMI values
  )
}

# Create cleaned BMI variables from the merged data
bmi25 <- harmonize_missing(merged$W8DBMI)
bmi32 <- harmonize_missing(merged$W9DBMI)

# Create output dataframe with only required variables
output <- tibble(
  NSID = merged$NSID,
  bmi25 = bmi25,
  bmi32 = bmi32
)

# Apply labels using haven::labelled with numeric labels
output$bmi25 <- haven::labelled(output$bmi25, 
  c("Refusal" = -9, "Don't know/insufficient information" = -8, 
    "Item not applicable" = -1, "Not asked/interviewed" = -3, "Script error/lost" = -2))
output$bmi32 <- haven::labelled(output$bmi32, 
  c("Refusal" = -9, "Don't know/insufficient information" = -8, 
    "Item not applicable" = -1, "Not asked/interviewed" = -3, "Script error/lost" = -2))

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Cleaned dataset written to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(output), "\n")
cat("Variables:", names(output), "\n")