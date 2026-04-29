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

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
cleaned_data <- full_join(wave1, wave4, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave6, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave8, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave9, by = "NSID")

cat("Merged dataset has", nrow(cleaned_data), "rows and", ncol(cleaned_data), "columns\n")

# Function to convert wave-specific missing codes to standard codes
convert_missing_codes <- function(x) {
  case_when(
    # Standard missing codes
    x == -9 ~ -9,  # Refusal
    x == -8 ~ -8,  # Don't know/insufficient information
    x == -1 ~ -1,  # Item not applicable
    x == -3 ~ -3,  # Not asked at the fieldwork stage/participated/interviewed
    x == -2 ~ -2,  # Schedule not applicable/Script error/information lost
    x == -7 ~ -7,  # Prefer not to say
    # Wave 6 specific codes
    x == -999 ~ -3,  # Not asked
    x == -997 ~ -2,  # Script error
    x == -97 ~ -3,   # Respondent declined self completion
    x == -92 ~ -9,   # Refused
    x == -91 ~ -1,   # Not applicable
    # Wave 8 specific codes
    x == -9 ~ -9,    # Refused
    x == -8 ~ -8,    # Insufficient information
    x == -1 ~ -1,    # Not applicable
    # Wave 9 specific codes
    x == -9 ~ -9,    # Refused
    x == -8 ~ -8,    # Insufficient information
    # Any other negative values
    x < 0 ~ -3,      # Default to not asked
    TRUE ~ x
  )
}

# Convert all missing codes in the dataset
for (col in names(cleaned_data)) {
  if (is.numeric(cleaned_data[[col]])) {
    cleaned_data[[col]] <- convert_missing_codes(cleaned_data[[col]])
    # Convert any remaining NA to -3
    cleaned_data[[col]][is.na(cleaned_data[[col]])] <- -3
  }
}

# Create age-specific marital status variables from merged data
# Age 19 (Wave 6) - W6MarStatYP
cleaned_data$marstat19 <- case_when(
  cleaned_data$W6MarStatYP %in% c(-999, -997, -97, -92, -91, -1) ~ convert_missing_codes(cleaned_data$W6MarStatYP),
  cleaned_data$W6MarStatYP == 1 ~ 1,
  cleaned_data$W6MarStatYP == 2 ~ 2,
  cleaned_data$W6MarStatYP == 3 ~ 3,
  cleaned_data$W6MarStatYP == 4 ~ 4,
  cleaned_data$W6MarStatYP == 5 ~ 5,
  TRUE ~ -3
)

# Age 25 (Wave 8) - W8DMARSTAT
cleaned_data$marstat25 <- case_when(
  cleaned_data$W8DMARSTAT %in% c(-9, -8, -1) ~ convert_missing_codes(cleaned_data$W8DMARSTAT),
  cleaned_data$W8DMARSTAT == 1 ~ 1,
  cleaned_data$W8DMARSTAT == 2 ~ 2,
  cleaned_data$W8DMARSTAT == 3 ~ 3,
  cleaned_data$W8DMARSTAT == 4 ~ 4,
  cleaned_data$W8DMARSTAT == 5 ~ 5,
  cleaned_data$W8DMARSTAT == 6 ~ 6,
  cleaned_data$W8DMARSTAT == 7 ~ 7,
  cleaned_data$W8DMARSTAT == 8 ~ 8,
  cleaned_data$W8DMARSTAT == 9 ~ 9,
  TRUE ~ -3
)

# Age 32 (Wave 9) - W9DMARSTAT
cleaned_data$marstat32 <- case_when(
  cleaned_data$W9DMARSTAT %in% c(-9, -8) ~ convert_missing_codes(cleaned_data$W9DMARSTAT),
  cleaned_data$W9DMARSTAT == 1 ~ 1,
  cleaned_data$W9DMARSTAT == 2 ~ 2,
  cleaned_data$W9DMARSTAT == 3 ~ 3,
  cleaned_data$W9DMARSTAT == 4 ~ 4,
  cleaned_data$W9DMARSTAT == 5 ~ 5,
  cleaned_data$W9DMARSTAT == 6 ~ 6,
  cleaned_data$W9DMARSTAT == 7 ~ 7,
  cleaned_data$W9DMARSTAT == 8 ~ 8,
  TRUE ~ -3
)

# Create collapsed harmonized marital status (consistent across ages)
# Use most recent valid response for time-invariant characteristics
harmonized_marstat <- case_when(
  !is.na(cleaned_data$marstat32) & cleaned_data$marstat32 > 0 ~ cleaned_data$marstat32,
  !is.na(cleaned_data$marstat25) & cleaned_data$marstat25 > 0 ~ cleaned_data$marstat25,
  !is.na(cleaned_data$marstat19) & cleaned_data$marstat19 > 0 ~ cleaned_data$marstat19,
  TRUE ~ -3
)

# Convert to harmonized collapsed categories
cleaned_data$marstat_adu <- case_when(
  harmonized_marstat %in% c(-9, -8, -1, -3, -2, -7) ~ harmonized_marstat,
  harmonized_marstat == 1 ~ 1,  # Single
  harmonized_marstat %in% c(2, 6) ~ 2,  # Married/Civil Partner
  harmonized_marstat %in% c(3, 7) ~ 3,  # Separated
  harmonized_marstat %in% c(4, 8) ~ 4,  # Divorced/Former CP
  harmonized_marstat %in% c(5, 9) ~ 5,  # Widowed/Surviving CP
  TRUE ~ -3
)

# Convert to factors with labels using labelled function
marstat19_lab <- labelled(cleaned_data$marstat19, 
  labels = c("Single, that is never married" = 1,
             "Married" = 2,
             "Separated" = 3,
             "Divorced" = 4,
             "Widowed" = 5,
             "Not asked" = -999,
             "Script error" = -997,
             "Respondent declined self completion" = -97,
             "Refused" = -92,
             "Not applicable" = -91,
             "Don't know" = -1))

marstat25_lab <- labelled(cleaned_data$marstat25,
  labels = c("Single and never married or in a CP" = 1,
             "Married" = 2,
             "Separated but still legally married" = 3,
             "Divorced" = 4,
             "Widowed" = 5,
             "A Civil Partner" = 6,
             "Separated but still legally in a CP" = 7,
             "A former Civil Partner" = 8,
             "A surviving Civil Partner" = 9,
             "Refused" = -9,
             "Insufficient information" = -8,
             "Not applicable" = -1))

marstat32_lab <- labelled(cleaned_data$marstat32,
  labels = c("Single that is never married or never in a Civil Partnership" = 1,
             "Married" = 2,
             "Divorced" = 3,
             "Legally separated" = 4,
             "Widowed" = 5,
             "A Civil Partner in a legally recognised Civil Partnership" = 6,
             "A former Civil Partner" = 7,
             "A surviving Civil Partner" = 8,
             "Refused" = -9,
             "Insufficient information" = -8))

marstat_adu_lab <- labelled(cleaned_data$marstat_adu,
  labels = c("Single" = 1,
             "Married/Civil Partner" = 2,
             "Separated" = 3,
             "Divorced/Former Civil Partner" = 4,
             "Widowed/Surviving Civil Partner" = 5,
             "Refused" = -9,
             "Insufficient information" = -8,
             "Not applicable" = -1,
             "Not asked at the fieldwork stage" = -3,
             "Schedule not applicable" = -2,
             "Prefer not to say" = -7))

# Replace columns with labelled versions
cleaned_data$marstat19 <- marstat19_lab
cleaned_data$marstat25 <- marstat25_lab
cleaned_data$marstat32 <- marstat32_lab
cleaned_data$marstat_adu <- marstat_adu_lab

# Select only the final variables for output
output_vars <- c("NSID", "marstat19", "marstat25", "marstat32", "marstat_adu")
output_data <- cleaned_data %>% select(all_of(output_vars))

# Write to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(output_data), "\n")
cat("Number of variables:", ncol(output_data), "\n")
