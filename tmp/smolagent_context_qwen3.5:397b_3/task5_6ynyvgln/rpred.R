# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. FILE LOADING
# =============================================================================

# Load each dataset from data/input/ using read_delim with tab delimiter
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# =============================================================================
# 2. MERGE DATASETS BY NSID
# =============================================================================

# Merge all datasets using full_join by NSID (the ID variable from source data)
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# =============================================================================
# 3. MISSING VALUE CODE HARMONIZATION FUNCTIONS
# =============================================================================

# Standard missing value codes:
# -9 = Refusal
# -8 = Don't know/insufficient information  
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# -2 = Schedule not applicable/Script error/information lost
# -7 = Prefer not to say

# Function to convert wave 6 missing codes to standard codes
convert_wave6_missing <- function(x) {
  case_when(
    x == -997 ~ -2,   # Script error
    x == -97 ~ -3,    # Respondent declined self completion
    x == -92 ~ -9,    # Refused
    x == -91 ~ -1,    # Not applicable
    x == -1 ~ -8,     # Don't know
    TRUE ~ as.numeric(x)
  )
}

# Function to convert wave 8 missing codes to standard codes
convert_wave8_missing <- function(x) {
  case_when(
    x == -9 ~ -9,     # Refused
    x == -8 ~ -8,     # Insufficient information
    x == -1 ~ -1,     # Not applicable
    TRUE ~ as.numeric(x)
  )
}

# Function to convert wave 9 missing codes to standard codes
convert_wave9_missing <- function(x) {
  case_when(
    x == -9 ~ -9,     # Refused
    x == -8 ~ -8,     # Insufficient information
    TRUE ~ as.numeric(x)
  )
}

# Function to convert NULL/NA values to -3 (Not asked)
convert_null_to_missing <- function(x) {
  x[is.na(x)] <- -3
  return(x)
}

# =============================================================================
# 4. MARITAL STATUS HARMONIZATION
# =============================================================================

# Create harmonized marital status variables across waves
# Collapsed categories: 1=Single, 2=Married, 3=Separated, 4=Divorced, 5=Widowed

merged_data <- merged_data %>%
  mutate(
    # Wave 6 (Age 19) - W6MarStatYP
    W6MarStatYP_clean = ifelse(is.na(W6MarStatYP), -3, convert_wave6_missing(W6MarStatYP)),
    W6MarStatYP_clean = convert_null_to_missing(W6MarStatYP_clean),
    
    # partnr19 - harmonized partnership status at age 19
    partnr19 = case_when(
      W6MarStatYP_clean == 1 ~ 1,   # Single, never married
      W6MarStatYP_clean == 2 ~ 2,   # Married
      W6MarStatYP_clean == 3 ~ 3,   # Separated
      W6MarStatYP_clean == 4 ~ 4,   # Divorced
      W6MarStatYP_clean == 5 ~ 5,   # Widowed
      W6MarStatYP_clean < 0 ~ W6MarStatYP_clean,  # Missing codes preserved
      TRUE ~ -3
    ),
    
    # Wave 8 (Age 25) - W8DMARSTAT
    W8DMARSTAT_clean = ifelse(is.na(W8DMARSTAT), -3, convert_wave8_missing(W8DMARSTAT)),
    W8DMARSTAT_clean = convert_null_to_missing(W8DMARSTAT_clean),
    
    # partnr25 - harmonized partnership status at age 25 (collapsed)
    partnr25 = case_when(
      W8DMARSTAT_clean %in% c(1, 6) ~ 1,    # Single or Civil Partner
      W8DMARSTAT_clean == 2 ~ 2,             # Married
      W8DMARSTAT_clean %in% c(3, 7) ~ 3,    # Separated (married or CP)
      W8DMARSTAT_clean %in% c(4, 8) ~ 4,    # Divorced or former CP
      W8DMARSTAT_clean %in% c(5, 9) ~ 5,    # Widowed or surviving CP
      W8DMARSTAT_clean < 0 ~ W8DMARSTAT_clean,  # Missing codes preserved
      TRUE ~ -3
    ),
    
    # partnradu25 - detailed adult partnership status at age 25
    partnradu25 = W8DMARSTAT_clean,
    
    # Wave 9 (Age 32) - W9DMARSTAT
    W9DMARSTAT_clean = ifelse(is.na(W9DMARSTAT), -3, convert_wave9_missing(W9DMARSTAT)),
    W9DMARSTAT_clean = convert_null_to_missing(W9DMARSTAT_clean),
    
    # partnr32 - harmonized partnership status at age 32 (collapsed)
    partnr32 = case_when(
      W9DMARSTAT_clean %in% c(1, 6) ~ 1,    # Single or Civil Partner
      W9DMARSTAT_clean == 2 ~ 2,             # Married
      W9DMARSTAT_clean == 4 ~ 3,             # Legally separated
      W9DMARSTAT_clean %in% c(3, 7) ~ 4,    # Divorced or former CP
      W9DMARSTAT_clean %in% c(5, 8) ~ 5,    # Widowed or surviving CP
      W9DMARSTAT_clean < 0 ~ W9DMARSTAT_clean,  # Missing codes preserved
      TRUE ~ -3
    ),
    
    # partnradu32 - detailed adult partnership status at age 32
    partnradu32 = W9DMARSTAT_clean
  )

# =============================================================================
# 5. CONVERT DERIVED VARIABLES TO FACTORS WITH LABELS
# =============================================================================

# Define factor labels for harmonized marital status
marital_labels <- c(
  "1" = "Single/Never married",
  "2" = "Married",
  "3" = "Separated",
  "4" = "Divorced",
  "5" = "Widowed",
  "-9" = "Refusal",
  "-8" = "Don't know/Insufficient information",
  "-1" = "Not applicable",
  "-3" = "Not asked",
  "-2" = "Script error"
)

# Define factor labels for detailed marital status (Wave 8)
marital_detailed_w8_labels <- c(
  "1" = "Single/never married or in CP",
  "2" = "Married",
  "3" = "Separated but still legally married",
  "4" = "Divorced",
  "5" = "Widowed",
  "6" = "A Civil Partner",
  "7" = "Separated but still legally in CP",
  "8" = "A former Civil Partner",
  "9" = "A surviving Civil Partner",
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-1" = "Not applicable",
  "-3" = "Not asked"
)

# Define factor labels for detailed marital status (Wave 9)
marital_detailed_w9_labels <- c(
  "1" = "Single/never married or never in CP",
  "2" = "Married",
  "3" = "Divorced",
  "4" = "Legally separated",
  "5" = "Widowed",
  "6" = "A Civil Partner",
  "7" = "A former Civil Partner",
  "8" = "A surviving Civil Partner",
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-3" = "Not asked"
)

# =============================================================================
# 6. SELECT FINAL OUTPUT VARIABLES
# =============================================================================

# Select only the ID variable and derived variables for output
output_data <- merged_data %>%
  select(
    NSID,
    partnr19,
    partnr25,
    partnradu25,
    partnr32,
    partnradu32
  )

# =============================================================================
# 7. WRITE OUTPUT FILE
# =============================================================================

# Write the cleaned data to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of observations:", nrow(output_data), "\n")
cat("Number of variables:", ncol(output_data), "\n")
cat("Output file: data/output/cleaned_data.csv\n")
