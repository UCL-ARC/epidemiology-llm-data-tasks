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

# Define a function to map missing value codes
map_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    x == -100 ~ -7,
    x == -97 ~ -7,
    TRUE ~ x
  )
}

# Define a function to create detailed education variable
create_detailed_educ <- function(data, mum_var, dad_var) {
  # Map missing values for mother's education
  data <- data %>%
    mutate(!!mum_var := map_missing(!!sym(mum_var)))

  # Map missing values for father's education
  data <- data %>%
    mutate(!!dad_var := map_missing(!!sym(dad_var)))

  # Create detailed education variables
  data <- data %>%
    mutate(
      educdtlma = case_when(
        !is.na(!!sym(mum_var)) & !!sym(mum_var) >= 1 ~ !!sym(mum_var),
        TRUE ~ -3
      ),
      educdtlpa = case_when(
        !is.na(!!sym(dad_var)) & !!sym(dad_var) >= 1 ~ !!sym(dad_var),
        TRUE ~ -3
      )
    )

  return(data)
}

# Apply the function to the merged data
merged_data <- create_detailed_educ(merged_data, "W1hiqualmum", "W1hiqualdad")
merged_data <- create_detailed_educ(merged_data, "W2hiqualmum", "W2hiqualdad")
merged_data <- create_detailed_educ(merged_data, "w4hiqualmum", "w4hiqualdad")

# Define a function to create NVQ education variable
create_nvq_educ <- function(data, mum_var, dad_var) {
  # Define NVQ mapping
  nvq_mapping <- c(
    `1` = 5,  # Higher Degree
    `2` = 5,  # First Degree
    `3` = 5,  # HE Diploma
    `4` = 4,  # HNC/HND/NVQ4
    `5` = 4,  # Teaching qualification, non-degree
    `6` = 4,  # Nursing qualification, non-degree
    `7` = 3,  # A Levels
    `8` = 3,  # OND/ONC
    `9` = 3,  # City and guilds part III, NVQ3
    `10` = 3, # CSYS
    `11` = 3, # Scottish Higher Grade
    `12` = 3, # AS Level
    `13` = 2, # Trade apprenticeship
    `14` = 2, # City and guilds part II, NVQ2
    `15` = 1, # GCSE grade A-C and equivalent
    `16` = 1, # GCSE grade D-E and equivalent
    `17` = 1, # City and guilds part I, NVQ1
    `18` = 1, # Youth training, skill seekers
    `19` = 0, # Qualification, level unspecified
    `20` = 0   # No qualification mentioned
  )

  # Create NVQ education variables
  data <- data %>%
    mutate(
      educma = case_when(
        !is.na(!!sym(mum_var)) & !!sym(mum_var) %in% as.numeric(names(nvq_mapping)) ~ nvq_mapping[as.character(!!sym(mum_var))],
        TRUE ~ -3
      ),
      educpa = case_when(
        !is.na(!!sym(dad_var)) & !!sym(dad_var) %in% as.numeric(names(nvq_mapping)) ~ nvq_mapping[as.character(!!sym(dad_var))],
        TRUE ~ -3
      )
    )

  return(data)
}

# Apply the function to the merged data
merged_data <- create_nvq_educ(merged_data, "W1hiqualmum", "W1hiqualdad")
merged_data <- create_nvq_educ(merged_data, "W2hiqualmum", "W2hiqualdad")
merged_data <- create_nvq_educ(merged_data, "w4hiqualmum", "w4hiqualdad")

# Select only the required variables
final_data <- merged_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write the final dataset
write_csv(final_data, "data/output/cleaned_data.csv")