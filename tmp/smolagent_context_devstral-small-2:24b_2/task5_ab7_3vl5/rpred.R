library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022_derived_variables <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8_2015_derived, by = "NSID") %>%
  full_join(ns9_2022_derived_variables, by = "NSID")

# Standard missing value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-7" = "Prefer not to say"
)

# Function to harmonize missing values
harmonize_missing_values <- function(df, var) {
  if (var %in% colnames(df)) {
    df[[var]] <- ifelse(df[[var]] %in% c(-999, -997, -97, -92, -91, -1), -3, df[[var]])
    df[[var]] <- ifelse(df[[var]] %in% c(-9, -8, -1), -9, df[[var]])
  }
  return(df)
}

# Harmonize missing values for marital status variables
merged_data <- harmonize_missing_values(merged_data, "W6MarStatYP")
merged_data <- harmonize_missing_values(merged_data, "W8DMARSTAT")
merged_data <- harmonize_missing_values(merged_data, "W9DMARSTAT")

# Create age-specific marital status variables
merged_data <- merged_data %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      TRUE ~ -3
    ),
    partnr25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 6 ~ 6,
      W8DMARSTAT == 7 ~ 7,
      W8DMARSTAT == 8 ~ 8,
      W8DMARSTAT == 9 ~ 9,
      TRUE ~ -3
    ),
    partnr32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 3,
      W9DMARSTAT == 4 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 6 ~ 6,
      W9DMARSTAT == 7 ~ 7,
      W9DMARSTAT == 8 ~ 8,
      TRUE ~ -3
    )
  )

# Create factor variables with labels
merged_data <- merged_data %>%
  mutate(
    partnr19 = factor(partnr19, levels = c(-3, 1, 2, 3, 4, 5), labels = c("Not asked", "Single, never married", "Married", "Separated", "Divorced", "Widowed")),
    partnr25 = factor(partnr25, levels = c(-3, 1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("Not asked", "Single, never married or in a CP", "Married", "Separated but still legally married", "Divorced", "Widowed", "A Civil Partner", "Separated but still legally in a CP", "A former Civil Partner", "A surviving Civil Partner")),
    partnr32 = factor(partnr32, levels = c(-3, 1, 2, 3, 4, 5, 6, 7, 8), labels = c("Not asked", "Single, never married or never in a CP", "Married", "Divorced", "Legally separated", "Widowed", "A Civil Partner in a legally recognised CP", "A former Civil Partner", "A surviving Civil Partner"))
  )

# Save cleaned data
write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)
