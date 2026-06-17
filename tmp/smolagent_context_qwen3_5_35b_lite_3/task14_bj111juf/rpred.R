library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from metadata
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = "\t")
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = "\t")
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = "\t")
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = "\t")
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = "\t")
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = "\t")
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = "\t")
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = "\t")
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = "\t")

# Merge all datasets using NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave3, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")
merged <- full_join(merged, wave5, by = "NSID")
merged <- full_join(merged, wave6, by = "NSID")
merged <- full_join(merged, wave7, by = "NSID")
merged <- full_join(merged, wave8, by = "NSID")
merged <- full_join(merged, wave9, by = "NSID")

# Function to recode tenure with standard missing codes
code_missing <- function(x) {
  # Replace all missing value codes with NA first
  x[is.na(x) | x == -999 | x == -998 | x == -997 | x == -995 | x == -99 | x == -92 | 
    x == -91 | x == -1 | x == -8 | x == -7 | x == -9] <- NA
  return(x)
}

# Recode wave 1 (age 14) - W1hous12HH
w1 <- code_missing(merged$W1hous12HH)
w1_std <- case_when(
  is.na(w1) ~ -3,
  w1 == 1 ~ 1,  # Owned outright
  w1 == 2 ~ 2,  # Being bought
  w1 == 3 ~ 3,  # Shared ownership
  w1 %in% c(4, 5, 6, 7, 8) ~ 4,  # All rental types -> Rent it
  TRUE ~ -3
)

# Recode wave 2 (age 15) - W2Hous12HH
w2 <- code_missing(merged$W2Hous12HH)
w2_std <- case_when(
  is.na(w2) ~ -3,
  w2 == 1 ~ 1,
  w2 == 2 ~ 2,
  w2 == 3 ~ 3,
  w2 %in% c(4, 5, 6, 7, 8) ~ 4,
  TRUE ~ -3
)

# Recode wave 3 (age 16) - W3hous12HH
w3 <- code_missing(merged$W3hous12HH)
w3_std <- case_when(
  is.na(w3) ~ -3,
  w3 == 1 ~ 1,
  w3 == 2 ~ 2,
  w3 == 3 ~ 3,
  w3 %in% c(4, 5, 6, 7, 8) ~ 4,
  TRUE ~ -3
)

# Recode wave 4 (age 17) - W4Hous12HH
w4 <- code_missing(merged$W4Hous12HH)
w4_std <- case_when(
  is.na(w4) ~ -3,
  w4 == 1 ~ 1,
  w4 == 2 ~ 2,
  w4 == 3 ~ 3,
  w4 %in% c(4, 5, 6, 7, 8) ~ 4,
  TRUE ~ -3
)

# Recode wave 5 (age 18) - W5Hous12HH (simplified categories)
w5 <- code_missing(merged$W5Hous12HH)
w5_std <- case_when(
  is.na(w5) ~ -3,
  w5 == 1 ~ 1,  # Owned
  w5 == 2 ~ 4,  # Rented -> Rent it
  w5 == 3 ~ 8,  # Something else
  w5 == 6 ~ -3,  # Not to be asked
  TRUE ~ -3
)

# Recode wave 6 (age 19) - W6Hous12YP (simplified categories)
w6 <- code_missing(merged$W6Hous12YP)
w6_std <- case_when(
  is.na(w6) ~ -3,
  w6 == 1 ~ 1,
  w6 == 2 ~ 4,
  w6 == 3 ~ 8,
  TRUE ~ -3
)

# Recode wave 7 (age 20) - W7Hous12YP (simplified categories)
w7 <- code_missing(merged$W7Hous12YP)
w7_std <- case_when(
  is.na(w7) ~ -3,
  w7 == 1 ~ 1,
  w7 == 2 ~ 4,
  w7 == 3 ~ 8,
  TRUE ~ -3
)

# Recode wave 8 (age 25) - W8TENURE
w8 <- code_missing(merged$W8TENURE)
w8_std <- case_when(
  is.na(w8) ~ -3,
  w8 == 1 ~ 1,
  w8 == 2 ~ 2,
  w8 == 3 ~ 3,
  w8 %in% c(4, 5, 6, 7) ~ 4,  # All rental types -> Rent it
  TRUE ~ -3
)

# Recode wave 9 (age 32) - W9DTENURE
w9 <- code_missing(merged$W9DTENURE)
w9_std <- case_when(
  is.na(w9) ~ -3,
  w9 == 1 ~ 1,
  w9 == 2 ~ 2,
  w9 == 3 ~ 3,
  w9 %in% c(4, 5, 6) ~ 4,  # Rent it, rent-free, squatting -> Rent it
  w9 == 7 ~ 8,  # Other
  TRUE ~ -3
)

# Create detailed hownteen variables for ages 14-20
merged$hownteen14 <- w1_std
merged$hownteen15 <- w2_std
merged$hownteen16 <- w3_std
merged$hownteen17 <- w4_std
merged$hownteen18 <- w5_std
merged$hownteen19 <- w6_std
merged$hownteen20 <- w7_std

# Create collapsed hown variables for ages 14-32
merged$hown14 <- w1_std
merged$hown15 <- w2_std
merged$hown16 <- w3_std
merged$hown17 <- w4_std
merged$hown18 <- w5_std
merged$hown19 <- w6_std
merged$hown20 <- w7_std
merged$hown25 <- w8_std
merged$hown32 <- w9_std

# Select only NSID and derived variables
output <- merged %>% select(NSID, 
                            hownteen14, hownteen15, hownteen16, hownteen17, 
                            hownteen18, hownteen19, hownteen20,
                            hown14, hown15, hown16, hown17, hown18, hown19, 
                            hown20, hown25, hown32)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')
cat('Output variables:', paste(colnames(output), collapse=', '), '\n')
cat('Number of cases:', nrow(output), '\n')
cat('Data written to data/output/cleaned_data.csv\n')