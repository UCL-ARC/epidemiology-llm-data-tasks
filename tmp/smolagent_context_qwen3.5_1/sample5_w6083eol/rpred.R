# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# 1. FILE LOADING
# ============================================================================

# Load each dataset from data/input/ directory
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Print dimensions for verification
cat("Wave 1 (Age 14):", nrow(wave1), "rows,", ncol(wave1), "columns\n")
cat("Wave 4 (Age 17):", nrow(wave4), "rows,", ncol(wave4), "columns\n")
cat("Wave 6 (Age 19):", nrow(wave6), "rows,", ncol(wave6), "columns\n")
cat("Wave 8 (Age 25):", nrow(wave8), "rows,", ncol(wave8), "columns\n")
cat("Wave 9 (Age 32):", nrow(wave9), "rows,", ncol(wave9), "columns\n")

# ============================================================================
# 2. MERGE DATASETS
# ============================================================================

# Merge all datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

cat("\nMerged data:", nrow(merged_data), "rows,", ncol(merged_data), "columns\n")

# ============================================================================
# 3. DEFINE STANDARD MISSING VALUE CODES
# ============================================================================

# Standard missing value codes:
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# -2 = Schedule not applicable/Script error/information lost
# -7 = Prefer not to say

# Function to convert wave-specific missing codes to standard codes
convert_missing_codes <- function(x, var_name) {
  # First, replace NULL/NA with -3 (Not asked)
  x[is.na(x)] <- -3
  
  # Convert wave-specific codes to standard codes
  # Wave 6 specific codes:
  x[x == -997] <- -2  # Script error
  x[x == -97] <- -3   # Respondent declined self completion
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -1] <- -8    # Don't know
  
  # Wave 8 and 9 codes are already standard (-9, -8, -1)
  # But ensure consistency
  x[x == -999] <- -3  # Generic missing
  x[x == -998] <- -8  # Don't know variant
  x[x == -995] <- -7  # Prefer not to say variant
  x[x == -94] <- -9   # Refusal variant
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -100] <- -3  # Not asked
  
  return(x)
}

# ============================================================================
# 4. PROCESS MARRITAL STATUS VARIABLES
# ============================================================================

# Check which marital status variables exist in the merged data
marital_vars <- c("W6MarStatYP", "W8DMARSTAT", "W9DMARSTAT")
existing_marital <- marital_vars[marital_vars %in% names(merged_data)]
cat("\nExisting marital status variables:", paste(existing_marital, collapse = ", "), "\n")

# Process Wave 6 Marital Status (Age 19)
if ("W6MarStatYP" %in% names(merged_data)) {
  # Convert missing codes
  merged_data$W6MarStatYP <- convert_missing_codes(merged_data$W6MarStatYP, "W6MarStatYP")
  
  # Create harmonized marital status at age 19 (partnr19)
  # Map to common categories: 1=Single, 2=Married/Civil Partner, 3=Separated, 4=Divorced/Dissolved, 5=Widowed/Surviving
  merged_data$partnr19 <- case_when(
    merged_data$W6MarStatYP == 1 ~ 1,  # Single
    merged_data$W6MarStatYP == 2 ~ 2,  # Married
    merged_data$W6MarStatYP == 3 ~ 3,  # Separated
    merged_data$W6MarStatYP == 4 ~ 4,  # Divorced
    merged_data$W6MarStatYP == 5 ~ 5,  # Widowed
    merged_data$W6MarStatYP < 0 ~ merged_data$W6MarStatYP,  # Keep missing codes
    TRUE ~ -3  # Default to not asked
  )
  
  # Add labels
  merged_data$partnr19 <- labelled(merged_data$partnr19, labels = c(
    "Single" = 1, "Married" = 2, "Separated" = 3, 
    "Divorced" = 4, "Widowed" = 5,
    "Item not applicable" = -1, "Schedule not applicable" = -2,
    "Not asked" = -3, "Prefer not to say" = -7,
    "Don't know" = -8, "Refused" = -9
  ))
  
  # Keep detailed age-specific variable
  merged_data$partnradu19 <- merged_data$W6MarStatYP
  merged_data$partnradu19 <- labelled(merged_data$partnradu19, labels = c(
    "Single, never married" = 1, "Married" = 2, "Separated" = 3,
    "Divorced" = 4, "Widowed" = 5,
    "Item not applicable" = -1, "Schedule not applicable" = -2,
    "Not asked" = -3, "Prefer not to say" = -7,
    "Don't know" = -8, "Refused" = -9
  ))
}

# Process Wave 8 Marital Status (Age 25)
if ("W8DMARSTAT" %in% names(merged_data)) {
  # Convert missing codes
  merged_data$W8DMARSTAT <- convert_missing_codes(merged_data$W8DMARSTAT, "W8DMARSTAT")
  
  # Create harmonized marital status at age 25 (partnr25)
  # Map detailed categories to common categories
  merged_data$partnr25 <- case_when(
    merged_data$W8DMARSTAT == 1 ~ 1,  # Single and never married or in a CP
    merged_data$W8DMARSTAT == 2 ~ 2,  # Married
    merged_data$W8DMARSTAT == 6 ~ 2,  # A Civil Partner (map to Married for harmonization)
    merged_data$W8DMARSTAT == 3 ~ 3,  # Separated but still legally married
    merged_data$W8DMARSTAT == 7 ~ 3,  # Separated but still legally in a CP
    merged_data$W8DMARSTAT == 4 ~ 4,  # Divorced
    merged_data$W8DMARSTAT == 8 ~ 4,  # A former Civil Partner
    merged_data$W8DMARSTAT == 5 ~ 5,  # Widowed
    merged_data$W8DMARSTAT == 9 ~ 5,  # A surviving Civil Partner
    merged_data$W8DMARSTAT < 0 ~ merged_data$W8DMARSTAT,  # Keep missing codes
    TRUE ~ -3
  )
  
  # Add labels
  merged_data$partnr25 <- labelled(merged_data$partnr25, labels = c(
    "Single" = 1, "Married/CP" = 2, "Separated" = 3,
    "Divorced/Dissolved" = 4, "Widowed/Surviving" = 5,
    "Item not applicable" = -1, "Schedule not applicable" = -2,
    "Not asked" = -3, "Prefer not to say" = -7,
    "Don't know" = -8, "Refused" = -9
  ))
  
  # Keep detailed age-specific variable
  merged_data$partnradu25 <- merged_data$W8DMARSTAT
  merged_data$partnradu25 <- labelled(merged_data$partnradu25, labels = c(
    "Single, never married/CP" = 1, "Married" = 2, "Separated (married)" = 3,
    "Divorced" = 4, "Widowed" = 5, "Civil Partner" = 6,
    "Separated (CP)" = 7, "Former CP" = 8, "Surviving CP" = 9,
    "Item not applicable" = -1, "Schedule not applicable" = -2,
    "Not asked" = -3, "Prefer not to say" = -7,
    "Don't know" = -8, "Refused" = -9
  ))
}

# Process Wave 9 Marital Status (Age 32)
if ("W9DMARSTAT" %in% names(merged_data)) {
  # Convert missing codes
  merged_data$W9DMARSTAT <- convert_missing_codes(merged_data$W9DMARSTAT, "W9DMARSTAT")
  
  # Create harmonized marital status at age 32 (partnr32)
  merged_data$partnr32 <- case_when(
    merged_data$W9DMARSTAT == 1 ~ 1,  # Single that is never married or never in a Civil Partnership
    merged_data$W9DMARSTAT == 2 ~ 2,  # Married
    merged_data$W9DMARSTAT == 6 ~ 2,  # A Civil Partner
    merged_data$W9DMARSTAT == 4 ~ 3,  # Legally separated
    merged_data$W9DMARSTAT == 3 ~ 4,  # Divorced
    merged_data$W9DMARSTAT == 7 ~ 4,  # A former Civil Partner
    merged_data$W9DMARSTAT == 5 ~ 5,  # Widowed
    merged_data$W9DMARSTAT == 8 ~ 5,  # A surviving Civil Partner
    merged_data$W9DMARSTAT < 0 ~ merged_data$W9DMARSTAT,  # Keep missing codes
    TRUE ~ -3
  )
  
  # Add labels
  merged_data$partnr32 <- labelled(merged_data$partnr32, labels = c(
    "Single" = 1, "Married/CP" = 2, "Separated" = 3,
    "Divorced/Dissolved" = 4, "Widowed/Surviving" = 5,
    "Item not applicable" = -1, "Schedule not applicable" = -2,
    "Not asked" = -3, "Prefer not to say" = -7,
    "Don't know" = -8, "Refused" = -9
  ))
  
  # Keep detailed age-specific variable
  merged_data$partnradu32 <- merged_data$W9DMARSTAT
  merged_data$partnradu32 <- labelled(merged_data$partnradu32, labels = c(
    "Single, never married/CP" = 1, "Married" = 2, "Divorced" = 3,
    "Legally separated" = 4, "Widowed" = 5, "Civil Partner" = 6,
    "Former CP" = 7, "Surviving CP" = 8,
    "Item not applicable" = -1, "Schedule not applicable" = -2,
    "Not asked" = -3, "Prefer not to say" = -7,
    "Don't know" = -8, "Refused" = -9
  ))
}

# ============================================================================
# 5. HANDLE NULL VALUES ACROSS ALL VARIABLES
# ============================================================================

# Convert all remaining NA values to -3 (Not asked)
for (col in names(merged_data)) {
  if (is.numeric(merged_data[[col]])) {
    merged_data[[col]][is.na(merged_data[[col]])] <- -3
  }
}

# ============================================================================
# 6. SELECT FINAL VARIABLES FOR OUTPUT
# ============================================================================

# Define variables to include in output
output_vars <- c("NSID")

# Add harmonized marital status variables
if ("partnr19" %in% names(merged_data)) output_vars <- c(output_vars, "partnr19")
if ("partnr25" %in% names(merged_data)) output_vars <- c(output_vars, "partnr25")
if ("partnr32" %in% names(merged_data)) output_vars <- c(output_vars, "partnr32")

# Add detailed age-specific marital status variables
if ("partnradu19" %in% names(merged_data)) output_vars <- c(output_vars, "partnradu19")
if ("partnradu25" %in% names(merged_data)) output_vars <- c(output_vars, "partnradu25")
if ("partnradu32" %in% names(merged_data)) output_vars <- c(output_vars, "partnradu32")

cat("\nOutput variables:", paste(output_vars, collapse = ", "), "\n")

# Select only the output variables
final_data <- merged_data %>% select(all_of(output_vars))

# ============================================================================
# 7. WRITE OUTPUT
# ============================================================================

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

cat("\n=== CLEANING COMPLETE ===")
cat("\nOutput file: data/output/cleaned_data.csv")
cat("\nFinal dimensions:", nrow(final_data), "rows,", ncol(final_data), "columns\n")

# Print summary of marital status variables
cat("\n=== SUMMARY OF MARITAL STATUS VARIABLES ===")
for (var in output_vars[-1]) {
  if (var %in% names(final_data)) {
    cat("\n\n", var, ":\n")
    print(table(final_data[[var]], useNA = "ifany"))
  }
}
