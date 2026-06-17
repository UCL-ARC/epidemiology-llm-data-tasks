# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Define the input files
input_files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load each file using read_delim with tab delimiter
data_wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
data_wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
data_wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
data_wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
data_wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- full_join(data_wave1, data_wave2, by = "NSID")
merged_data <- full_join(merged_data, data_wave4, by = "NSID")
merged_data <- full_join(merged_data, data_wave8, by = "NSID")
merged_data <- full_join(merged_data, data_wave9, by = "NSID")

# Function to harmonize ethnicity categories to a standard scheme
harmonize_ethnicity <- function(x, wave) {
  if (wave == 1) {
    # W1ethnic2YP: -999=household lost, -94=insufficient info, -92=refused, -91=NA, -1=don't know
    x <- case_when(
      x == -999 ~ -2,
      x == -94 ~ -8,
      x == -92 ~ -9,
      x == -91 ~ -1,
      x == -1 ~ -8,
      x == 1 ~ 1,
      x == 2 ~ 2,
      x == 3 ~ 3,
      x == 4 ~ 4,
      x == 5 ~ 5,
      x == 6 ~ 6,
      x == 7 ~ 7,
      x == 8 ~ 8,
      x == 9 ~ 9,
      x == 10 ~ 10,
      x == 11 ~ 11,
      x == 12 ~ 12,
      x == 13 ~ 13,
      x == 14 ~ 14,
      x == 15 ~ 15,
      x == 16 ~ 16,
      TRUE ~ NA_real_
    )
  } else if (wave == 2) {
    # W2ethnicYP: -998=interviewer missed, -997=script error, -995=missing history, -99=not interviewed,
    # -92=refused, -91=NA, -1=don't know
    x <- case_when(
      x == -998 ~ -2,
      x == -997 ~ -2,
      x == -995 ~ -2,
      x == -99 ~ -3,
      x == -92 ~ -9,
      x == -91 ~ -1,
      x == -1 ~ -8,
      x == 1 ~ 1,
      x == 2 ~ 2,
      x == 3 ~ 3,
      x == 4 ~ 4,
      x == 5 ~ 5,
      x == 6 ~ 6,
      x == 7 ~ 7,
      x == 8 ~ 8,
      x == 9 ~ 9,
      x == 10 ~ 10,
      x == 11 ~ 11,
      x == 12 ~ 12,
      x == 13 ~ 13,
      x == 14 ~ 14,
      x == 15 ~ 15,
      x == 16 ~ 16,
      TRUE ~ NA_real_
    )
  } else if (wave == 4) {
    # w4ethnic2YP: -94=insufficient info, -1=don't know
    x <- case_when(
      x == -94 ~ -8,
      x == -1 ~ -8,
      x == 1 ~ 1,
      x == 2 ~ 2,
      x == 3 ~ 3,
      x == 4 ~ 4,
      x == 5 ~ 5,
      x == 6 ~ 6,
      x == 7 ~ 7,
      x == 8 ~ 8,
      x == 9 ~ 9,
      x == 10 ~ 10,
      x == 11 ~ 11,
      x == 12 ~ 12,
      x == 13 ~ 13,
      x == 14 ~ 14,
      x == 15 ~ 15,
      x == 16 ~ 16,
      TRUE ~ NA_real_
    )
  } else if (wave == 8) {
    # W8DETHN15: -9=refused, -8=insufficient info, -1=not applicable
    x <- case_when(
      x == -9 ~ -9,
      x == -8 ~ -8,
      x == -1 ~ -1,
      x == 1 ~ 1,
      x == 2 ~ 2,
      x == 3 ~ 3,
      x == 4 ~ 4,
      x == 5 ~ 5,
      x == 6 ~ 6,
      x == 7 ~ 7,
      x == 8 ~ 8,
      x == 9 ~ 9,
      x == 10 ~ 10,
      x == 11 ~ 11,
      x == 12 ~ 12,
      x == 13 ~ 13,
      x == 14 ~ 14,
      x == 15 ~ 15,
      x == 16 ~ 16,
      TRUE ~ NA_real_
    )
  } else if (wave == 9) {
    # W9DETHN15: -8=insufficient info
    x <- case_when(
      x == -8 ~ -8,
      x == 1 ~ 1,
      x == 2 ~ 2,
      x == 3 ~ 3,
      x == 4 ~ 4,
      x == 5 ~ 5,
      x == 6 ~ 6,
      x == 7 ~ 7,
      x == 8 ~ 8,
      x == 9 ~ 9,
      x == 10 ~ 10,
      x == 11 ~ 11,
      x == 12 ~ 12,
      x == 13 ~ 13,
      x == 14 ~ 14,
      x == 15 ~ 15,
      x == 16 ~ 16,
      TRUE ~ NA_real_
    )
  }
  return(x)
}

# Create wave-specific ethnicity variables
merged_data$eth14 <- harmonize_ethnicity(merged_data$W1ethnic2YP, wave = 1)
merged_data$eth15 <- harmonize_ethnicity(merged_data$W2ethnicYP, wave = 2)
merged_data$eth17 <- harmonize_ethnicity(merged_data$w4ethnic2YP, wave = 4)
merged_data$eth25 <- harmonize_ethnicity(merged_data$W8DETHN15, wave = 8)
merged_data$eth32 <- harmonize_ethnicity(merged_data$W9DETHN15, wave = 9)

# Create consolidated ethnicity variable using earliest-valid-first approach
merged_data$eth <- case_when(
  !is.na(merged_data$eth14) ~ merged_data$eth14,
  !is.na(merged_data$eth15) ~ merged_data$eth15,
  !is.na(merged_data$eth17) ~ merged_data$eth17,
  !is.na(merged_data$eth25) ~ merged_data$eth25,
  !is.na(merged_data$eth32) ~ merged_data$eth32,
  TRUE ~ NA_real_
)

# Keep only final derived variables
final_data <- merged_data %>%
  select(NSID, eth, eth14, eth15, eth17, eth25, eth32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
head(final_data)
