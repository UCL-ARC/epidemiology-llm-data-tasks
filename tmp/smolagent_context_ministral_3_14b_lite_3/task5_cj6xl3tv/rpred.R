
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths and variable mappings
file_paths <- list(
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_derived.tab",
  wave9 = "data/input/ns9_2022_derived_variables.tab"
)

# Load all files
load_files <- function(path) {
  read_delim(path, delim = "\t")
}

loaded_data <- map(file_paths, load_files)

# Assign each loaded dataset to a named object
wave6_data <- loaded_data[["wave6"]]
wave8_data <- loaded_data[["wave8"]]
wave9_data <- loaded_data[["wave9"]]

# Merge datasets by NSID
merged_data <- full_join(
  full_join(wave6_data, wave8_data, by = "NSID"),
  wave9_data,
  by = "NSID"
)

# Define mapping for missing values
missing_value_mapping <- list(
  '-997.0' = -2,  # Script error
  '-97.0' = -9,   # Respondent declined self completion (considered refusal)
  '-92.0' = -9,   # Refused
  '-91.0' = -1,   # Not applicable
  '-1.0' = -8,    # Don't know
  '-9.0' = -9,    # Refused
  '-8.0' = -8,    # Insufficient information
  '-99.0' = -3    # Not asked / not interviewed
)

# Function to recode missing values
recode_missing <- function(x) {
  if (!is.numeric(x)) return(x)
  x <- as.character(x)
  x[x %in% names(missing_value_mapping)] <- lapply(x[x %in% names(missing_value_mapping)], function(val) {
    missing_value_mapping[[val]]
  })
  x[is.na(x)] <- -3
  as.numeric(x)
}

# Create derived variables for partnership status at ages 19, 25, and 32
partnr19 <- merged_data %>%
  transmute(
    NSID,
    partnr19 = recode_missing(W6MarStatYP)
  ) %>%
  mutate(
    partnr19 = factor(partnr19, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5),
                      labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Item not applicable", "Single, never married", "Married", "Separated", "Divorced", "Widowed"))
  )

partnr25 <- merged_data %>%
  transmute(
    NSID,
    partnr25 = recode_missing(W8DMARSTAT)
  ) %>%
  mutate(
    partnr25 = factor(partnr25, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                      labels = c("Refusal", "Insufficient information", "Prefer not to say", "Not asked", "Schedule not applicable", "Item not applicable", "Single, never married or in a CP", "Married", "Separated but still legally married", "Divorced", "Widowed", "A Civil Partner", "Separated but still legally in a CP", "A former Civil Partner", "A surviving Civil Partner"))
  )

partnr32 <- merged_data %>%
  transmute(
    NSID,
    partnr32 = recode_missing(W9DMARSTAT)
  ) %>%
  mutate(
    partnr32 = factor(partnr32, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c("Refusal", "Insufficient information", "Prefer not to say", "Not asked", "Schedule not applicable", "Item not applicable", "Single, never married or never in a CP", "Married", "Divorced", "Legally separated", "Widowed", "A Civil Partner", "A former Civil Partner", "A surviving Civil Partner"))
  )

# Create detailed adult partnership status variables at ages 25 and 32
partnradu25 <- merged_data %>%
  transmute(
    NSID,
    partnradu25 = recode_missing(W8DMARSTAT)
  ) %>%
  mutate(
    partnradu25 = factor(partnradu25, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("Refusal", "Insufficient information", "Prefer not to say", "Not asked", "Schedule not applicable", "Item not applicable", "Single, never married or in a CP", "Married", "Separated but still legally married", "Divorced", "Widowed", "A Civil Partner", "Separated but still legally in a CP", "A former Civil Partner", "A surviving Civil Partner"))
  )

partnradu32 <- merged_data %>%
  transmute(
    NSID,
    partnradu32 = recode_missing(W9DMARSTAT)
  ) %>%
  mutate(
    partnradu32 = factor(partnradu32, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Refusal", "Insufficient information", "Prefer not to say", "Not asked", "Schedule not applicable", "Item not applicable", "Single, never married or never in a CP", "Married", "Divorced", "Legally separated", "Widowed", "A Civil Partner", "A former Civil Partner", "A surviving Civil Partner"))
  )

# Merge derived variables into the main dataset
final_data <- merged_data %>%
  left_join(partnr19, by = "NSID") %>%
  left_join(partnr25, by = "NSID") %>%
  left_join(partnr32, by = "NSID") %>%
  left_join(partnradu25, by = "NSID") %>%
  left_join(partnradu32, by = "NSID")

# Select only the NSID and the derived partnership status variables
final_data <- final_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write the final output to CSV
write_csv(final_data, "data/output/cleaned_data.csv")
