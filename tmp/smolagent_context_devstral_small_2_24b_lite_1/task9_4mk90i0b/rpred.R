library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define standard missing-value codes
standard_missing <- c(
  `-9` = "Refusal",
  `-8` = "Don't know / insufficient information",
  `-7` = "Prefer not to say",
  `-3` = "Not asked at the fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable"
)

# Function to map wave-specific missing codes to standard codes
map_missing <- function(x, wave_labels) {
  x <- as.numeric(x)
  mapped <- case_when(
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -94 ~ -8,
    x == -999 ~ -2,
    x == -1 ~ -8,
    TRUE ~ x
  )
  return(mapped)
}

# Function to create detailed education variable
derive_detailed_educ <- function(mum_var, dad_var) {
  mum_educ <- map_missing(merged_data[[mum_var]], wave_labels = NULL)
  dad_educ <- map_missing(merged_data[[dad_var]], wave_labels = NULL)
  
  # Consolidate using earliest-valid-first
  consolidator <- function(x) {
    if (all(is.na(x))) {
      return(-3)
    }
    valid_vals <- x[!is.na(x) & x > 0]
    if (length(valid_vals) == 0) {
      missing_vals <- x[!is.na(x) & x < 0]
      if (length(missing_vals) > 0) {
        return(missing_vals[1])
      } else {
        return(-3)
      }
    }
    return(valid_vals[1])
  }
  
  educdtlma <- apply(cbind(mum_educ), 1, consolidator)
  educdtlpa <- apply(cbind(dad_educ), 1, consolidator)
  
  return(list(educdtlma = educdtlma, educdtlpa = educdtlpa))
}

# Function to create NVQ education variable
derive_nvq_educ <- function(detailed_educ) {
  nvq_mapping <- c(
    `1` = 5,  # Higher Degree
    `2` = 4,  # First Degree
    `3` = 4,  # HE Diploma
    `4` = 3,  # HNC/HND/NVQ4
    `5` = 3,  # Teaching qualification, non-degree
    `6` = 3,  # Nursing qualification, non-degree
    `7` = 2,  # A Levels
    `8` = 2,  # OND/ONC
    `9` = 2,  # City and guilds part III, NVQ3
    `10` = 2, # CSYS
    `11` = 2, # Scottish Higher Grade
    `12` = 2, # AS Level
    `13` = 1, # Trade apprenticeship
    `14` = 1, # City and guilds part II, NVQ2
    `15` = 1, # GCSE grade A-C and equivalent
    `16` = 1, # GCSE grade D-E and equivalent
    `17` = 1, # City and guilds part I, NVQ1
    `18` = 1, # Youth training, skill seekers
    `19` = 1, # Qualification, level unspecified
    `20` = 0   # No qualification mentioned
  )
  
  nvq_educ <- detailed_educ
  nvq_educ <- ifelse(nvq_educ %in% names(nvq_mapping), nvq_mapping[as.character(nvq_educ)], nvq_educ)
  
  return(nvq_educ)
}

# Derive detailed education variables
detailed_vars <- derive_detailed_educ("W1hiqualmum", "W1hiqualdad")
merged_data$educdtlma <- detailed_vars$educdtlma
merged_data$educdtlpa <- detailed_vars$educdtlpa

# Derive NVQ education variables
merged_data$educma <- derive_nvq_educ(merged_data$educdtlma)
merged_data$educpa <- derive_nvq_educ(merged_data$educdtlpa)

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write the output file
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"