# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
w5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged <- full_join(w1, w2, by = "NSID")
merged <- full_join(merged, w3, by = "NSID")
merged <- full_join(merged, w4, by = "NSID")
merged <- full_join(merged, w5, by = "NSID")
merged <- full_join(merged, w6, by = "NSID")
merged <- full_join(merged, w7, by = "NSID")
merged <- full_join(merged, w8, by = "NSID")
merged <- full_join(merged, w9, by = "NSID")

# Function to convert missing codes to standard scheme
convert_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995, -99) ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    x == -8 ~ -8,
    x == -7 ~ -7,
    x == -9 ~ -9,
    TRUE ~ x
  )
}

# Detailed housing tenure variables (hownteen) for ages 14-20
# Keep detailed categories per wave
merged <- merged %>%
  mutate(
    # Age 14 - detailed
    hownteen14 = case_when(
      W1hous12HH %in% c(-999, -92, -91, -1) ~ convert_missing(W1hous12HH),
      W1hous12HH == 1 ~ 1,
      W1hous12HH == 2 ~ 2,
      W1hous12HH == 3 ~ 3,
      W1hous12HH == 4 ~ 4,
      W1hous12HH == 5 ~ 5,
      W1hous12HH == 6 ~ 6,
      W1hous12HH == 7 ~ 7,
      W1hous12HH == 8 ~ 8,
      TRUE ~ NA_real_
    ),
    
    # Age 15 - detailed
    hownteen15 = case_when(
      W2Hous12HH %in% c(-998, -997, -995, -99, -92, -91, -1) ~ convert_missing(W2Hous12HH),
      W2Hous12HH == 1 ~ 1,
      W2Hous12HH == 2 ~ 2,
      W2Hous12HH == 3 ~ 3,
      W2Hous12HH == 4 ~ 4,
      W2Hous12HH == 5 ~ 5,
      W2Hous12HH == 6 ~ 6,
      W2Hous12HH == 7 ~ 7,
      W2Hous12HH == 8 ~ 8,
      TRUE ~ NA_real_
    ),
    
    # Age 16 - detailed
    hownteen16 = case_when(
      W3hous12HH %in% c(-999, -99, -92, -91, -1) ~ convert_missing(W3hous12HH),
      W3hous12HH == 1 ~ 1,
      W3hous12HH == 2 ~ 2,
      W3hous12HH == 3 ~ 3,
      W3hous12HH == 4 ~ 4,
      W3hous12HH == 5 ~ 5,
      W3hous12HH == 6 ~ 6,
      W3hous12HH == 7 ~ 7,
      W3hous12HH == 8 ~ 8,
      TRUE ~ NA_real_
    ),
    
    # Age 17 - detailed
    hownteen17 = case_when(
      W4Hous12HH %in% c(-999, -997, -92, -91, -1) ~ convert_missing(W4Hous12HH),
      W4Hous12HH == 1 ~ 1,
      W4Hous12HH == 2 ~ 2,
      W4Hous12HH == 3 ~ 3,
      W4Hous12HH == 4 ~ 4,
      W4Hous12HH == 5 ~ 5,
      W4Hous12HH == 6 ~ 6,
      W4Hous12HH == 7 ~ 7,
      W4Hous12HH == 8 ~ 8,
      TRUE ~ NA_real_
    ),
    
    # Age 18 - detailed (W5 has simpler categories)
    hownteen18 = case_when(
      W5Hous12HH %in% c(-999, -92, -91, -1) ~ convert_missing(W5Hous12HH),
      W5Hous12HH == 1 ~ 1,
      W5Hous12HH == 2 ~ 4,  # Rented mapped to rent category
      W5Hous12HH == 3 ~ 8,  # Something else
      W5Hous12HH == 6 ~ -2, # Not to be asked
      TRUE ~ NA_real_
    ),
    
    # Age 19 - detailed (W6 has simpler categories)
    hownteen19 = case_when(
      W6Hous12YP %in% c(-92, -91, -1) ~ convert_missing(W6Hous12YP),
      W6Hous12YP == 1 ~ 1,
      W6Hous12YP == 2 ~ 4,  # Rented mapped to rent category
      W6Hous12YP == 3 ~ 8,  # Something else
      TRUE ~ NA_real_
    ),
    
    # Age 20 - detailed (W7 has simpler categories)
    hownteen20 = case_when(
      W7Hous12YP %in% c(-92, -91, -1) ~ convert_missing(W7Hous12YP),
      W7Hous12YP == 1 ~ 1,
      W7Hous12YP == 2 ~ 4,  # Rented mapped to rent category
      W7Hous12YP == 3 ~ 8,  # Something else
      TRUE ~ NA_real_
    )
  )

# Collapsed housing tenure variables (hown) for ages 14-32
# Merge rental categories into single "Rent it" category
merged <- merged %>%
  mutate(
    # Age 14 - collapsed (rental categories 4,5,6,7,8 -> 4)
    hown14 = case_when(
      W1hous12HH %in% c(-999, -92, -91, -1) ~ convert_missing(W1hous12HH),
      W1hous12HH == 1 ~ 1,
      W1hous12HH == 2 ~ 2,
      W1hous12HH == 3 ~ 3,
      W1hous12HH %in% c(4, 5, 6, 7, 8) ~ 4,  # All rental -> Rent it
      TRUE ~ NA_real_
    ),
    
    # Age 15 - collapsed
    hown15 = case_when(
      W2Hous12HH %in% c(-998, -997, -995, -99, -92, -91, -1) ~ convert_missing(W2Hous12HH),
      W2Hous12HH == 1 ~ 1,
      W2Hous12HH == 2 ~ 2,
      W2Hous12HH == 3 ~ 3,
      W2Hous12HH %in% c(4, 5, 6, 7, 8) ~ 4,  # All rental -> Rent it
      TRUE ~ NA_real_
    ),
    
    # Age 16 - collapsed
    hown16 = case_when(
      W3hous12HH %in% c(-999, -99, -92, -91, -1) ~ convert_missing(W3hous12HH),
      W3hous12HH == 1 ~ 1,
      W3hous12HH == 2 ~ 2,
      W3hous12HH == 3 ~ 3,
      W3hous12HH %in% c(4, 5, 6, 7, 8) ~ 4,  # All rental -> Rent it
      TRUE ~ NA_real_
    ),
    
    # Age 17 - collapsed
    hown17 = case_when(
      W4Hous12HH %in% c(-999, -997, -92, -91, -1) ~ convert_missing(W4Hous12HH),
      W4Hous12HH == 1 ~ 1,
      W4Hous12HH == 2 ~ 2,
      W4Hous12HH == 3 ~ 3,
      W4Hous12HH %in% c(4, 5, 6, 7, 8) ~ 4,  # All rental -> Rent it
      TRUE ~ NA_real_
    ),
    
    # Age 18 - collapsed
    hown18 = case_when(
      W5Hous12HH %in% c(-999, -92, -91, -1) ~ convert_missing(W5Hous12HH),
      W5Hous12HH == 1 ~ 1,
      W5Hous12HH == 2 ~ 4,  # Rented -> Rent it
      W5Hous12HH == 3 ~ 8,  # Something else
      W5Hous12HH == 6 ~ -2, # Not to be asked
      TRUE ~ NA_real_
    ),
    
    # Age 19 - collapsed
    hown19 = case_when(
      W6Hous12YP %in% c(-92, -91, -1) ~ convert_missing(W6Hous12YP),
      W6Hous12YP == 1 ~ 1,
      W6Hous12YP == 2 ~ 4,  # Rented -> Rent it
      W6Hous12YP == 3 ~ 8,  # Something else
      TRUE ~ NA_real_
    ),
    
    # Age 20 - collapsed
    hown20 = case_when(
      W7Hous12YP %in% c(-92, -91, -1) ~ convert_missing(W7Hous12YP),
      W7Hous12YP == 1 ~ 1,
      W7Hous12YP == 2 ~ 4,  # Rented -> Rent it
      W7Hous12YP == 3 ~ 8,  # Something else
      TRUE ~ NA_real_
    ),
    
    # Age 25 - collapsed (W8)
    hown25 = case_when(
      W8TENURE %in% c(-9, -8, -1) ~ convert_missing(W8TENURE),
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE %in% c(4, 5) ~ 4,  # Rent -> Rent it
      W8TENURE == 6 ~ 6,  # Squatting
      W8TENURE == 7 ~ 7,  # Other
      TRUE ~ NA_real_
    ),
    
    # Age 32 - collapsed (W9)
    hown32 = case_when(
      W9DTENURE %in% c(-8) ~ convert_missing(W9DTENURE),
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4,
      W9DTENURE == 5 ~ 4,  # Rent-free -> Rent it
      W9DTENURE == 6 ~ 6,  # Squatting
      W9DTENURE == 7 ~ 7,  # Other
      TRUE ~ NA_real_
    )
  )

# Select only NSID and final derived variables
output <- merged %>%
  select(NSID, 
         starts_with("hownteen"),
         starts_with("hown"))

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Output saved to data/output/cleaned_data.csv\n")
cat("Dimensions:", dim(output), "\n")
print(head(output))
