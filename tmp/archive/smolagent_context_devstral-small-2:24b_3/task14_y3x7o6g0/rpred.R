# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  "ns9_2022_derived_variables.tab" = "data/input/ns9_2022_derived_variables.tab",
  "wave_four_lsype_family_background_2020.tab" = "data/input/wave_four_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab" = "data/input/wave_three_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab" = "data/input/wave_two_lsype_family_background_2020.tab",
  "wave_one_lsype_family_background_2020.tab" = "data/input/wave_one_lsype_family_background_2020.tab",
  "ns8_2015_main_interview.tab" = "data/input/ns8_2015_main_interview.tab",
  "wave_five_lsype_family_background_2020.tab" = "data/input/wave_five_lsype_family_background_2020.tab",
  "wave_six_lsype_young_person_2020.tab" = "data/input/wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab" = "data/input/wave_seven_lsype_young_person_2020.tab"
)

# Load each dataset with progress messages
load_data <- function(file) {
  message("Loading: ", basename(file))
  read_delim(file, delim = "\t")
}

# Load all datasets
datasets <- map(files, load_data)

# Merge datasets using full_join by NSID with progress messages
message("Starting merge process...")
merged_data <- datasets[[1]]
for (i in 2:length(datasets)) {
  message("Merging dataset: ", names(files)[i])
  merged_data <- full_join(merged_data, datasets[[i]], by = "NSID")
  message("Completed merge ", i-1, " of ", length(datasets)-1)
}

message("Merge completed. Starting data processing...")

# Standard missing value codes
missing_codes <- list(
  -9, "Refusal",
  -8, "Don't know/insufficient information",
  -1, "Item not applicable",
  -3, "Not asked/participated/interviewed",
  -2, "Script error/information lost"
)

# Function to recode missing values
recode_missing <- function(x, missing_map) {
  x <- na_if(x, NA)
  x[x == -999] <- -3
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -99] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x
}

# Recode missing values for each wave
message("Recoding missing values...")
merged_data <- merged_data %>%
  mutate(
    W1hous12HH = recode_missing(W1hous12HH, missing_codes),
    W2Hous12HH = recode_missing(W2Hous12HH, missing_codes),
    W3hous12HH = recode_missing(W3hous12HH, missing_codes),
    W4Hous12HH = recode_missing(W4Hous12HH, missing_codes),
    W5Hous12HH = recode_missing(W5Hous12HH, missing_codes),
    W5Hous12BHH = recode_missing(W5Hous12BHH, missing_codes),
    W5Hous12CHH = recode_missing(W5Hous12CHH, missing_codes),
    W6Hous12YP = recode_missing(W6Hous12YP, missing_codes),
    W6Hous12bYP = recode_missing(W6Hous12bYP, missing_codes),
    W6Hous12cYP = recode_missing(W6Hous12cYP, missing_codes),
    W7Hous12YP = recode_missing(W7Hous12YP, missing_codes),
    W7Hous12bYP = recode_missing(W7Hous12bYP, missing_codes),
    W7Hous12cYP = recode_missing(W7Hous12cYP, missing_codes),
    W8TENURE = recode_missing(W8TENURE, missing_codes),
    W9DTENURE = recode_missing(W9DTENURE, missing_codes)
  )

# Create detailed adolescent variables
message("Creating detailed adolescent variables...")
merged_data <- merged_data %>%
  mutate(
    hownteen14 = W1hous12HH,
    hownteen15 = W2Hous12HH,
    hownteen16 = W3hous12HH,
    hownteen17 = W4Hous12HH,
    hownteen18 = W5Hous12HH,
    hownteen19 = W6Hous12YP,
    hownteen20 = W7Hous12YP
  )

# Create collapsed variables for each age wave
message("Creating collapsed variables...")
merged_data <- merged_data %>%
  mutate(
    hown14 = case_when(
      hownteen14 == 1 ~ 1,
      hownteen14 == 2 ~ 2,
      hownteen14 == 3 ~ 3,
      hownteen14 == 4 ~ 4,
      hownteen14 == 5 ~ 4,
      hownteen14 == 6 ~ 4,
      hownteen14 == 7 ~ 5,
      hownteen14 == 8 ~ 6,
      TRUE ~ hownteen14
    ),
    hown15 = case_when(
      hownteen15 == 1 ~ 1,
      hownteen15 == 2 ~ 2,
      hownteen15 == 3 ~ 3,
      hownteen15 == 4 ~ 4,
      hownteen15 == 5 ~ 4,
      hownteen15 == 6 ~ 4,
      hownteen15 == 7 ~ 5,
      hownteen15 == 8 ~ 6,
      TRUE ~ hownteen15
    ),
    hown16 = case_when(
      hownteen16 == 1 ~ 1,
      hownteen16 == 2 ~ 2,
      hownteen16 == 3 ~ 3,
      hownteen16 == 4 ~ 4,
      hownteen16 == 5 ~ 4,
      hownteen16 == 6 ~ 4,
      hownteen16 == 7 ~ 5,
      hownteen16 == 8 ~ 6,
      TRUE ~ hownteen16
    ),
    hown17 = case_when(
      hownteen17 == 1 ~ 1,
      hownteen17 == 2 ~ 2,
      hownteen17 == 3 ~ 3,
      hownteen17 == 4 ~ 4,
      hownteen17 == 5 ~ 4,
      hownteen17 == 6 ~ 4,
      hownteen17 == 7 ~ 5,
      hownteen17 == 8 ~ 6,
      TRUE ~ hownteen17
    ),
    hown18 = case_when(
      W5Hous12HH == 1 ~ 1,
      W5Hous12HH == 2 ~ 4,
      W5Hous12HH == 3 ~ 6,
      W5Hous12HH == 6 ~ -3,
      W5Hous12HH == -92 ~ -9,
      W5Hous12HH == -91 ~ -1,
      W5Hous12HH == -1 ~ -8,
      TRUE ~ W5Hous12HH
    ),
    hown19 = case_when(
      W6Hous12YP == 1 ~ 1,
      W6Hous12YP == 2 ~ 4,
      W6Hous12YP == 3 ~ 6,
      W6Hous12YP == -92 ~ -9,
      W6Hous12YP == -91 ~ -1,
      W6Hous12YP == -1 ~ -8,
      TRUE ~ W6Hous12YP
    ),
    hown20 = case_when(
      W7Hous12YP == 1 ~ 1,
      W7Hous12YP == 2 ~ 4,
      W7Hous12YP == 3 ~ 6,
      W7Hous12YP == -92 ~ -9,
      W7Hous12YP == -91 ~ -1,
      W7Hous12YP == -1 ~ -8,
      TRUE ~ W7Hous12YP
    ),
    hown25 = case_when(
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE == 4 ~ 4,
      W8TENURE == 5 ~ 5,
      W8TENURE == 6 ~ 6,
      W8TENURE == 7 ~ 6,
      W8TENURE == -9 ~ -9,
      W8TENURE == -8 ~ -8,
      W8TENURE == -1 ~ -1,
      TRUE ~ W8TENURE
    ),
    hown32 = case_when(
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4,
      W9DTENURE == 5 ~ 5,
      W9DTENURE == 6 ~ 6,
      W9DTENURE == 7 ~ 6,
      W9DTENURE == -9 ~ -9,
      W9DTENURE == -8 ~ -8,
      W9DTENURE == -1 ~ -1,
      TRUE ~ W9DTENURE
    )
  )

# Select only the required variables
message("Selecting final variables...")
cleaned_data <- merged_data %>%
  select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write the cleaned dataset to a single CSV
message("Writing output file...")
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
message("Processing completed successfully!")
