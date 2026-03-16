# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define file paths
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_young_person_2020.tab",
  wave_three = "data/input/wave_three_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  ns8_2015 = "data/input/ns8_2015_main_interview.tab",
  ns9_2022 = "data/input/ns9_2022_main_interview.tab"
)

# Load datasets
load_data <- function(file) {
  read_delim(file, delim = "\t")
}

data_list <- map(files, load_data)

# Merge all datasets using full_join
merged_data <- reduce(data_list, full_join, by = "NSID")

# Create a function to harmonize missing values
harmonize_missing <- function(var) {
  case_when(
    var == -999 ~ -3,
    var == -998 ~ -3,
    var == -997 ~ -3,
    var == -995 ~ -3,
    var == -99 ~ -3,
    var == -92 ~ -9,
    var == -91 ~ -1,
    var == -1 ~ -8,
    TRUE ~ var
  )
}

# Apply harmonization to sex variables
merged_data <- merged_data %>%
  mutate(
    W1sexYP = harmonize_missing(W1sexYP),
    W2SexYP = harmonize_missing(W2SexYP),
    W3sexYP = harmonize_missing(W3sexYP),
    W4SexYP = harmonize_missing(W4SexYP),
    W5SexYP = harmonize_missing(W5SexYP),
    W6Sex = harmonize_missing(W6Sex),
    W7Sex = harmonize_missing(W7Sex),
    W8CMSEX = harmonize_missing(W8CMSEX),
    W9DSEX = harmonize_missing(W9DSEX)
  )

# Create consolidated sex variable
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      !is.na(W9DSEX) & W9DSEX > 0 ~ W9DSEX,
      !is.na(W8CMSEX) & W8CMSEX > 0 ~ W8CMSEX,
      !is.na(W7Sex) & W7Sex > 0 ~ W7Sex,
      !is.na(W6Sex) & W6Sex > 0 ~ W6Sex,
      !is.na(W5SexYP) & W5SexYP > 0 ~ W5SexYP,
      !is.na(W4SexYP) & W4SexYP > 0 ~ W4SexYP,
      !is.na(W3sexYP) & W3sexYP > 0 ~ W3sexYP,
      !is.na(W2SexYP) & W2SexYP > 0 ~ W2SexYP,
      !is.na(W1sexYP) & W1sexYP > 0 ~ W1sexYP,
      TRUE ~ -3
    )
  )

# Add labels to sex variable
attr(merged_data$sex, "labels") <- c("1" = "Male", "2" = "Female", 
                                        "-9" = "Refusal", "-8" = "Don't know/insufficient information", 
                                        "-1" = "Item not applicable", "-3" = "Not asked at the fieldwork stage/participated/interviewed")

# Select only the ID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, sex)

# Save the cleaned data
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Print confirmation
cat("Cleaned data saved to data/output/cleaned_data.csv\n")