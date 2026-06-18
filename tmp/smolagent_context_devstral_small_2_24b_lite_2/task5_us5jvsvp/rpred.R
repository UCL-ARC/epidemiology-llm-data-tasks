library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(var, labels) {
  var <- as.numeric(var)
  recoded <- case_when(
    labels == "Refused" ~ -9,
    labels == "Don't know" | labels == "Insufficient information" ~ -8,
    labels == "Prefer not to say" ~ -7,
    labels == "Not asked at the fieldwork stage" | labels == "Not interviewed" ~ -3,
    labels == "Schedule not applicable" | labels == "Script error" | labels == "Information lost" ~ -2,
    labels == "Not applicable" ~ -1,
    TRUE ~ var
  )
  return(recoded)
}

# Harmonize W6MarStatYP
merged_data$W6MarStatYP <- harmonize_missing(merged_data$W6MarStatYP, 
  case_when(
    merged_data$W6MarStatYP == -997 ~ "Script error",
    merged_data$W6MarStatYP == -97 ~ "Respondent declined self completion",
    merged_data$W6MarStatYP == -92 ~ "Refused",
    merged_data$W6MarStatYP == -91 ~ "Not applicable",
    merged_data$W6MarStatYP == -1 ~ "Don't know",
    TRUE ~ "Valid"
  )
)

# Harmonize W8DMARSTAT
merged_data$W8DMARSTAT <- harmonize_missing(merged_data$W8DMARSTAT, 
  case_when(
    merged_data$W8DMARSTAT == -9 ~ "Refused",
    merged_data$W8DMARSTAT == -8 ~ "Insufficient information",
    merged_data$W8DMARSTAT == -1 ~ "Not applicable",
    TRUE ~ "Valid"
  )
)

# Harmonize W9DMARSTAT
merged_data$W9DMARSTAT <- harmonize_missing(merged_data$W9DMARSTAT, 
  case_when(
    merged_data$W9DMARSTAT == -9 ~ "Refused",
    merged_data$W9DMARSTAT == -8 ~ "Insufficient information",
    TRUE ~ "Valid"
  )
)

# Create partnr19 (age 19)
merged_data$partnr19 <- merged_data$W6MarStatYP

# Create partnr25 (age 25)
merged_data$partnr25 <- merged_data$W8DMARSTAT

# Create partnr32 (age 32)
merged_data$partnr32 <- merged_data$W9DMARSTAT

# Create partnradu25 (adult version for age 25)
merged_data$partnradu25 <- merged_data$W8DMARSTAT

# Create partnradu32 (adult version for age 32)
merged_data$partnradu32 <- merged_data$W9DMARSTAT

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
