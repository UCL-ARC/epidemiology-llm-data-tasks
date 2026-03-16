# Load required libraries
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab",
  "data/input/wave_five_lsype_family_background_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_main_interview.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load all files
wave1 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
wave2 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
wave3 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
wave5 <- read_delim(files[5], delim = "\t", show_col_types = FALSE)
wave6 <- read_delim(files[6], delim = "\t", show_col_types = FALSE)
wave7 <- read_delim(files[7], delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(files[8], delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(files[9], delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
cleaned_data <- full_join(wave1, wave2, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave3, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave4, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave5, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave6, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave7, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave8, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave9, by = "NSID")

# Function to recode missing values to standard codes using case_when
recode_missing <- function(x) {
  x <- as.numeric(x)
  x <- case_when(
    x == -999 ~ -3,  # Not asked/missing household grid
    x == -998 ~ -3,  # Interviewer missed question
    x == -997 ~ -2,  # Script error
    x == -995 ~ -3,  # Missing history section data
    x == -92 ~ -9,   # Refusal
    x == -91 ~ -1,   # Not applicable
    x == -1 ~ -8,    # Don't know
    TRUE ~ x
  )
  # Convert any remaining NA to -3
  x[is.na(x)] <- -3
  return(x)
}

# Recode all tenure variables
# Age 14 (Wave 1)
cleaned_data$W1hous12HH <- recode_missing(cleaned_data$W1hous12HH)

# Age 15 (Wave 2)
cleaned_data$W2Hous12HH <- recode_missing(cleaned_data$W2Hous12HH)

# Age 16 (Wave 3)
cleaned_data$W3hous12HH <- recode_missing(cleaned_data$W3hous12HH)

# Age 17 (Wave 4)
cleaned_data$W4Hous12HH <- recode_missing(cleaned_data$W4Hous12HH)

# Age 18 (Wave 5) - multiple variables
# First recode the main type variable
cleaned_data$W5Hous12HH <- recode_missing(cleaned_data$W5Hous12HH)
# Then recode sub-type variables
cleaned_data$W5Hous12BHH <- recode_missing(cleaned_data$W5Hous12BHH)
cleaned_data$W5Hous12CHH <- recode_missing(cleaned_data$W5Hous12CHH)

# Age 19 (Wave 6)
cleaned_data$W6Hous12YP <- recode_missing(cleaned_data$W6Hous12YP)
cleaned_data$W6Hous12bYP <- recode_missing(cleaned_data$W6Hous12bYP)
cleaned_data$W6Hous12cYP <- recode_missing(cleaned_data$W6Hous12cYP)

# Age 20 (Wave 7)
cleaned_data$W7Hous12YP <- recode_missing(cleaned_data$W7Hous12YP)
cleaned_data$W7Hous12bYP <- recode_missing(cleaned_data$W7Hous12bYP)
cleaned_data$W7Hous12cYP <- recode_missing(cleaned_data$W7Hous12cYP)

# Age 32 (Wave 8)
cleaned_data$W8TENURE <- recode_missing(cleaned_data$W8TENURE)

# Age 32 (Wave 9)
cleaned_data$W9DTENURE <- recode_missing(cleaned_data$W9DTENURE)

# Create detailed adolescent variables (hownteen14-20)
# Age 14
cleaned_data$hownteen14 <- cleaned_data$W1hous12HH

# Age 15
cleaned_data$hownteen15 <- cleaned_data$W2Hous12HH

# Age 16
cleaned_data$hownteen16 <- cleaned_data$W3hous12HH

# Age 17
cleaned_data$hownteen17 <- cleaned_data$W4Hous12HH

# Age 18 (Wave 5) - combine W5Hous12HH with sub-types
age18_tenure <- cleaned_data$W5Hous12HH
age18_tenure[age18_tenure == 1] <- 1  # Owned
age18_tenure[age18_tenure == 2] <- 2  # Rented
age18_tenure[age18_tenure == 3] <- 6  # Something else
age18_tenure[age18_tenure == 6] <- -3 # Not to be asked HH Grid
cleaned_data$hownteen18 <- age18_tenure

# Age 19 (Wave 6)
age19_tenure <- cleaned_data$W6Hous12YP
age19_tenure[age19_tenure == 1] <- 1  # Owned
age19_tenure[age19_tenure == 2] <- 2  # Rented
age19_tenure[age19_tenure == 3] <- 6  # Something else
cleaned_data$hownteen19 <- age19_tenure

# Age 20 (Wave 7)
age20_tenure <- cleaned_data$W7Hous12YP
age20_tenure[age20_tenure == 1] <- 1  # Owned
age20_tenure[age20_tenure == 2] <- 2  # Rented
age20_tenure[age20_tenure == 3] <- 6  # Something else
cleaned_data$hownteen20 <- age20_tenure

# Create collapsed variables for all ages (hown14-20, hown25, hown32)
# Age 14-20 collapsed
hown14 <- cleaned_data$hownteen14
hown14[hown14 == 1] <- 1  # Owned outright
hown14[hown14 == 2] <- 2  # Owned, buying with help of mortgage/loan
hown14[hown14 == 3] <- 3  # Part rent, part mortgage
hown14[hown14 %in% c(4, 5, 6)] <- 4  # Rent it
hown14[hown14 == 7] <- 5  # Live rent-free
hown14[hown14 == 8] <- 6  # Other
hown14[hown14 == -1] <- 7  # Item not applicable
hown14[hown14 == -2] <- 8  # Script error
hown14[hown14 == -3] <- 9  # Not asked
hown14[hown14 == -8] <- 10  # Don't know
hown14[hown14 == -9] <- 11  # Refusal
cleaned_data$hown14 <- hown14

# Age 15 collapsed
hown15 <- cleaned_data$hownteen15
hown15[hown15 == 1] <- 1
hown15[hown15 == 2] <- 2
hown15[hown15 == 3] <- 3
hown15[hown15 %in% c(4, 5, 6)] <- 4
hown15[hown15 == 7] <- 5
hown15[hown15 == 8] <- 6
hown15[hown15 == -1] <- 7
hown15[hown15 == -2] <- 8
hown15[hown15 == -3] <- 9
hown15[hown15 == -8] <- 10
hown15[hown15 == -9] <- 11
cleaned_data$hown15 <- hown15

# Age 16 collapsed
hown16 <- cleaned_data$hownteen16
hown16[hown16 == 1] <- 1
hown16[hown16 == 2] <- 2
hown16[hown16 == 3] <- 3
hown16[hown16 %in% c(4, 5, 6)] <- 4
hown16[hown16 == 7] <- 5
hown16[hown16 == 8] <- 6
hown16[hown16 == -1] <- 7
hown16[hown16 == -2] <- 8
hown16[hown16 == -3] <- 9
hown16[hown16 == -8] <- 10
hown16[hown16 == -9] <- 11
cleaned_data$hown16 <- hown16

# Age 17 collapsed
hown17 <- cleaned_data$hownteen17
hown17[hown17 == 1] <- 1
hown17[hown17 == 2] <- 2
hown17[hown17 == 3] <- 3
hown17[hown17 %in% c(4, 5, 6)] <- 4
hown17[hown17 == 7] <- 5
hown17[hown17 == 8] <- 6
hown17[hown17 == -1] <- 7
hown17[hown17 == -2] <- 8
hown17[hown17 == -3] <- 9
hown17[hown17 == -8] <- 10
hown17[hown17 == -9] <- 11
cleaned_data$hown17 <- hown17

# Age 18 collapsed
hown18 <- cleaned_data$hownteen18
hown18[hown18 == 1] <- 1
hown18[hown18 == 2] <- 2
hown18[hown18 == 3] <- 3
hown18[hown18 %in% c(4, 5, 6)] <- 4
hown18[hown18 == 7] <- 5
hown18[hown18 == 8] <- 6
hown18[hown18 == -1] <- 7
hown18[hown18 == -2] <- 8
hown18[hown18 == -3] <- 9
hown18[hown18 == -8] <- 10
hown18[hown18 == -9] <- 11
cleaned_data$hown18 <- hown18

# Age 19 collapsed
hown19 <- cleaned_data$hownteen19
hown19[hown19 == 1] <- 1
hown19[hown19 == 2] <- 2
hown19[hown19 == 3] <- 3
hown19[hown19 %in% c(4, 5, 6)] <- 4
hown19[hown19 == 7] <- 5
hown19[hown19 == 8] <- 6
hown19[hown19 == -1] <- 7
hown19[hown19 == -2] <- 8
hown19[hown19 == -3] <- 9
hown19[hown19 == -8] <- 10
hown19[hown19 == -9] <- 11
cleaned_data$hown19 <- hown19

# Age 20 collapsed
hown20 <- cleaned_data$hownteen20
hown20[hown20 == 1] <- 1
hown20[hown20 == 2] <- 2
hown20[hown20 == 3] <- 3
hown20[hown20 %in% c(4, 5, 6)] <- 4
hown20[hown20 == 7] <- 5
hown20[hown20 == 8] <- 6
hown20[hown20 == -1] <- 7
hown20[hown20 == -2] <- 8
hown20[hown20 == -3] <- 9
hown20[hown20 == -8] <- 10
hown20[hown20 == -9] <- 11
cleaned_data$hown20 <- hown20

# Age 25 collapsed (from Wave 8)
hown25 <- cleaned_data$W8TENURE
hown25[hown25 == 1] <- 1
hown25[hown25 == 2] <- 2
hown25[hown25 == 3] <- 3
hown25[hown25 == 4] <- 4
hown25[hown25 == 5] <- 5
hown25[hown25 == 6] <- 6
hown25[hown25 == 7] <- 6
hown25[hown25 == -1] <- 7
hown25[hown25 == -2] <- 8
hown25[hown25 == -3] <- 9
hown25[hown25 == -8] <- 10
hown25[hown25 == -9] <- 11
cleaned_data$hown25 <- hown25

# Age 32 collapsed (from Wave 9)
hown32 <- cleaned_data$W9DTENURE
hown32[hown32 == 1] <- 1
hown32[hown32 == 2] <- 2
hown32[hown32 == 3] <- 3
hown32[hown32 == 4] <- 4
hown32[hown32 == 5] <- 5
hown32[hown32 == 6] <- 6
hown32[hown32 == 7] <- 6
hown32[hown32 == -1] <- 7
hown32[hown32 == -2] <- 8
hown32[hown32 == -3] <- 9
hown32[hown32 == -8] <- 10
hown32[hown32 == -9] <- 11
cleaned_data$hown32 <- hown32

# Select only required variables
output_vars <- c("NSID", 
                 "hown14", "hown15", "hown16", "hown17", "hown18", "hown19", "hown20",
                 "hown25", "hown32",
                 "hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen18", "hownteen19", "hownteen20")

cleaned_data <- cleaned_data %>% select(all_of(output_vars))

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(cleaned_data), "\n")
cat("Number of variables:", ncol(cleaned_data), "\n")
