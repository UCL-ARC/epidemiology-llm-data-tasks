# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the four wave files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Function to harmonize missing value codes
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x <- case_when(
    x %in% c(-999, -998, -997, -995, -94) ~ -2,
    x == -99 ~ -3,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    is.na(x) ~ -3,
    TRUE ~ x
  )
  return(x)
}

# Create wave-specific variables with harmonized missing values
wave1$W1englangYP_S1 <- harmonize_missing(wave1$W1englangYP)
wave2$W2EnglangYP_S2 <- harmonize_missing(wave2$W2EnglangYP)
wave3$W3englangHH_S3 <- harmonize_missing(wave3$W3englangHH)
wave4$W4EngLangHH_S4 <- harmonize_missing(wave4$W4EngLangHH)

# Merge all waves using full_join by NSID
merged_data <- full_join(wave1, wave2, by = "NSID")
merged_data <- full_join(merged_data, wave3, by = "NSID")
merged_data <- full_join(merged_data, wave4, by = "NSID")

# Create consolidated language variable
merged_data$lang <- case_when(
  !is.na(merged_data$W1englangYP_S1) & merged_data$W1englangYP_S1 > 0 ~ merged_data$W1englangYP_S1,
  !is.na(merged_data$W2EnglangYP_S2) & merged_data$W2EnglangYP_S2 > 0 ~ merged_data$W2EnglangYP_S2,
  !is.na(merged_data$W3englangHH_S3) & merged_data$W3englangHH_S3 > 0 ~ merged_data$W3englangHH_S3,
  !is.na(merged_data$W4EngLangHH_S4) & merged_data$W4EngLangHH_S4 > 0 ~ merged_data$W4EngLangHH_S4,
  !is.na(merged_data$W1englangYP_S1) ~ merged_data$W1englangYP_S1,
  !is.na(merged_data$W2EnglangYP_S2) ~ merged_data$W2EnglangYP_S2,
  !is.na(merged_data$W3englangHH_S3) ~ merged_data$W3englangHH_S3,
  !is.na(merged_data$W4EngLangHH_S4) ~ merged_data$W4EngLangHH_S4,
  TRUE ~ -3
)

# Select only ID and consolidated variables
output_data <- merged_data %>%
  select(NSID, lang)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Output file created successfully!\n")
cat("Number of rows:", nrow(output_data), "\n")
cat("Number of columns:", ncol(output_data), "\n")
