# Load required packages
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
data1 <- read_delim(files[1], delim = "\t")
data2 <- read_delim(files[2], delim = "\t")
data3 <- read_delim(files[3], delim = "\t")
data4 <- read_delim(files[4], delim = "\t")
data5 <- read_delim(files[5], delim = "\t")
data6 <- read_delim(files[6], delim = "\t")
data7 <- read_delim(files[7], delim = "\t")
data8 <- read_delim(files[8], delim = "\t")
data9 <- read_delim(files[9], delim = "\t")

# Merge all datasets by NSID using full_join
merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID") %>%
  full_join(data5, by = "NSID") %>%
  full_join(data6, by = "NSID") %>%
  full_join(data7, by = "NSID") %>%
  full_join(data8, by = "NSID") %>%
  full_join(data9, by = "NSID")

# Function to recode missing values to standard codes
recode_missing <- function(x) {
  x <- ifelse(is.na(x), -3, x)  # Null to -3
  x <- ifelse(x == -999, -3, x)  # Not asked
  x <- ifelse(x == -998, -3, x)  # Not asked
  x <- ifelse(x == -997, -2, x)  # Script error
  x <- ifelse(x == -995, -3, x)  # Not asked
  x <- ifelse(x == -92, -9, x)   # Refusal
  x <- ifelse(x == -91, -1, x)   # Not applicable
  x <- ifelse(x == -1, -8, x)    # Don't know
  x <- ifelse(x == -9, -9, x)    # Refusal
  x <- ifelse(x == -8, -8, x)    # Don't know
  x <- ifelse(x == -3, -3, x)    # Not asked
  x <- ifelse(x == -2, -2, x)    # Script error
  return(x)
}

# Recode all tenure variables
merged_data$W1hous12HH <- recode_missing(merged_data$W1hous12HH)
merged_data$W2Hous12HH <- recode_missing(merged_data$W2Hous12HH)
merged_data$W3hous12HH <- recode_missing(merged_data$W3hous12HH)
merged_data$W4Hous12HH <- recode_missing(merged_data$W4Hous12HH)
merged_data$W5Hous12HH <- recode_missing(merged_data$W5Hous12HH)
merged_data$W5Hous12BHH <- recode_missing(merged_data$W5Hous12BHH)
merged_data$W5Hous12CHH <- recode_missing(merged_data$W5Hous12CHH)
merged_data$W6Hous12YP <- recode_missing(merged_data$W6Hous12YP)
merged_data$W6Hous12bYP <- recode_missing(merged_data$W6Hous12bYP)
merged_data$W6Hous12cYP <- recode_missing(merged_data$W6Hous12cYP)
merged_data$W7Hous12YP <- recode_missing(merged_data$W7Hous12YP)
merged_data$W7Hous12bYP <- recode_missing(merged_data$W7Hous12bYP)
merged_data$W7Hous12cYP <- recode_missing(merged_data$W7Hous12cYP)
merged_data$W8TENURE <- recode_missing(merged_data$W8TENURE)
merged_data$W9DTENURE <- recode_missing(merged_data$W9DTENURE)

# Create detailed adolescent variables (hownteen14-20)
# Age 14 (Sweep 1)
merged_data$hownteen14 <- merged_data$W1hous12HH

# Age 15 (Sweep 2)
merged_data$hownteen15 <- merged_data$W2Hous12HH

# Age 16 (Sweep 3)
merged_data$hownteen16 <- merged_data$W3hous12HH

# Age 17 (Sweep 4)
merged_data$hownteen17 <- merged_data$W4Hous12HH

# Age 18 (Sweep 5) - combine W5Hous12HH, W5Hous12BHH, W5Hous12CHH
merged_data$hownteen18 <- case_when(
  merged_data$W5Hous12HH == 1 ~ merged_data$W5Hous12BHH,
  merged_data$W5Hous12HH == 2 ~ merged_data$W5Hous12CHH,
  merged_data$W5Hous12HH == 3 ~ 3,  # Something else
  merged_data$W5Hous12HH == 6 ~ -3,  # Not to be asked
  TRUE ~ merged_data$W5Hous12HH
)

# Age 19 (Sweep 6) - combine W6Hous12YP, W6Hous12bYP, W6Hous12cYP
merged_data$hownteen19 <- case_when(
  merged_data$W6Hous12YP == 1 ~ merged_data$W6Hous12bYP,
  merged_data$W6Hous12YP == 2 ~ merged_data$W6Hous12cYP,
  merged_data$W6Hous12YP == 3 ~ 3,  # Something else
  TRUE ~ merged_data$W6Hous12YP
)

# Age 20 (Sweep 7) - combine W7Hous12YP, W7Hous12bYP, W7Hous12cYP
merged_data$hownteen20 <- case_when(
  merged_data$W7Hous12YP == 1 ~ merged_data$W7Hous12bYP,
  merged_data$W7Hous12YP == 2 ~ merged_data$W7Hous12cYP,
  merged_data$W7Hous12YP == 3 ~ 3,  # Something else
  TRUE ~ merged_data$W7Hous12YP
)

# Create collapsed variables for all ages
# Age 14-20 collapsed (hown14-20)
merged_data$hown14 <- case_when(
  merged_data$hownteen14 == 1 ~ 1,  # Owned outright
  merged_data$hownteen14 == 2 ~ 2,  # Owned, buying with help of mortgage/loan
  merged_data$hownteen14 == 3 ~ 3,  # Part rent, part mortgage
  merged_data$hownteen14 %in% c(4, 5, 6) ~ 4,  # Rent it
  merged_data$hownteen14 == 7 ~ 5,  # Live rent-free
  merged_data$hownteen14 == 8 ~ 6,  # Other
  merged_data$hownteen14 == -1 ~ -1,  # Not applicable
  merged_data$hownteen14 == -2 ~ -2,  # Script error
  merged_data$hownteen14 == -3 ~ -3,  # Not asked
  merged_data$hownteen14 == -8 ~ -8,  # Don't know
  merged_data$hownteen14 == -9 ~ -9,  # Refusal
  TRUE ~ NA_real_
)

merged_data$hown15 <- case_when(
  merged_data$hownteen15 == 1 ~ 1,
  merged_data$hownteen15 == 2 ~ 2,
  merged_data$hownteen15 == 3 ~ 3,
  merged_data$hownteen15 %in% c(4, 5, 6) ~ 4,
  merged_data$hownteen15 == 7 ~ 5,
  merged_data$hownteen15 == 8 ~ 6,
  merged_data$hownteen15 == -1 ~ -1,
  merged_data$hownteen15 == -2 ~ -2,
  merged_data$hownteen15 == -3 ~ -3,
  merged_data$hownteen15 == -8 ~ -8,
  merged_data$hownteen15 == -9 ~ -9,
  TRUE ~ NA_real_
)

merged_data$hown16 <- case_when(
  merged_data$hownteen16 == 1 ~ 1,
  merged_data$hownteen16 == 2 ~ 2,
  merged_data$hownteen16 == 3 ~ 3,
  merged_data$hownteen16 %in% c(4, 5, 6) ~ 4,
  merged_data$hownteen16 == 7 ~ 5,
  merged_data$hownteen16 == 8 ~ 6,
  merged_data$hownteen16 == -1 ~ -1,
  merged_data$hownteen16 == -2 ~ -2,
  merged_data$hownteen16 == -3 ~ -3,
  merged_data$hownteen16 == -8 ~ -8,
  merged_data$hownteen16 == -9 ~ -9,
  TRUE ~ NA_real_
)

merged_data$hown17 <- case_when(
  merged_data$hownteen17 == 1 ~ 1,
  merged_data$hownteen17 == 2 ~ 2,
  merged_data$hownteen17 == 3 ~ 3,
  merged_data$hownteen17 %in% c(4, 5, 6) ~ 4,
  merged_data$hownteen17 == 7 ~ 5,
  merged_data$hownteen17 == 8 ~ 6,
  merged_data$hownteen17 == -1 ~ -1,
  merged_data$hownteen17 == -2 ~ -2,
  merged_data$hownteen17 == -3 ~ -3,
  merged_data$hownteen17 == -8 ~ -8,
  merged_data$hownteen17 == -9 ~ -9,
  TRUE ~ NA_real_
)

merged_data$hown18 <- case_when(
  merged_data$hownteen18 == 1 ~ 1,
  merged_data$hownteen18 == 2 ~ 2,
  merged_data$hownteen18 == 3 ~ 3,
  merged_data$hownteen18 %in% c(4, 5, 6) ~ 4,
  merged_data$hownteen18 == 7 ~ 5,
  merged_data$hownteen18 == 8 ~ 6,
  merged_data$hownteen18 == -1 ~ -1,
  merged_data$hownteen18 == -2 ~ -2,
  merged_data$hownteen18 == -3 ~ -3,
  merged_data$hownteen18 == -8 ~ -8,
  merged_data$hownteen18 == -9 ~ -9,
  TRUE ~ NA_real_
)

merged_data$hown19 <- case_when(
  merged_data$hownteen19 == 1 ~ 1,
  merged_data$hownteen19 == 2 ~ 2,
  merged_data$hownteen19 == 3 ~ 3,
  merged_data$hownteen19 %in% c(4, 5, 6) ~ 4,
  merged_data$hownteen19 == 7 ~ 5,
  merged_data$hownteen19 == 8 ~ 6,
  merged_data$hownteen19 == -1 ~ -1,
  merged_data$hownteen19 == -2 ~ -2,
  merged_data$hownteen19 == -3 ~ -3,
  merged_data$hownteen19 == -8 ~ -8,
  merged_data$hownteen19 == -9 ~ -9,
  TRUE ~ NA_real_
)

merged_data$hown20 <- case_when(
  merged_data$hownteen20 == 1 ~ 1,
  merged_data$hownteen20 == 2 ~ 2,
  merged_data$hownteen20 == 3 ~ 3,
  merged_data$hownteen20 %in% c(4, 5, 6) ~ 4,
  merged_data$hownteen20 == 7 ~ 5,
  merged_data$hownteen20 == 8 ~ 6,
  merged_data$hownteen20 == -1 ~ -1,
  merged_data$hownteen20 == -2 ~ -2,
  merged_data$hownteen20 == -3 ~ -3,
  merged_data$hownteen20 == -8 ~ -8,
  merged_data$hownteen20 == -9 ~ -9,
  TRUE ~ NA_real_
)

# Age 25 collapsed (hown25)
merged_data$hown25 <- case_when(
  merged_data$W8TENURE == 1 ~ 1,
  merged_data$W8TENURE == 2 ~ 2,
  merged_data$W8TENURE == 3 ~ 3,
  merged_data$W8TENURE == 4 ~ 4,
  merged_data$W8TENURE == 5 ~ 5,
  merged_data$W8TENURE == 6 ~ 6,
  merged_data$W8TENURE == 7 ~ 6,
  merged_data$W8TENURE == -1 ~ -1,
  merged_data$W8TENURE == -2 ~ -2,
  merged_data$W8TENURE == -3 ~ -3,
  merged_data$W8TENURE == -8 ~ -8,
  merged_data$W8TENURE == -9 ~ -9,
  TRUE ~ NA_real_
)

# Age 32 collapsed (hown32)
merged_data$hown32 <- case_when(
  merged_data$W9DTENURE == 1 ~ 1,
  merged_data$W9DTENURE == 2 ~ 2,
  merged_data$W9DTENURE == 3 ~ 3,
  merged_data$W9DTENURE == 4 ~ 4,
  merged_data$W9DTENURE == 5 ~ 5,
  merged_data$W9DTENURE == 6 ~ 6,
  merged_data$W9DTENURE == 7 ~ 6,
  merged_data$W9DTENURE == -1 ~ -1,
  merged_data$W9DTENURE == -2 ~ -2,
  merged_data$W9DTENURE == -3 ~ -3,
  merged_data$W9DTENURE == -8 ~ -8,
  merged_data$W9DTENURE == -9 ~ -9,
  TRUE ~ NA_real_
)

# Select only required variables
output_data <- merged_data %>%
  select(NSID, 
         hown14, hown15, hown16, hown17, hown18, hown19, hown20,
         hown25, hown32,
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)

# Write output
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(output_data), "\n")
cat("Number of variables:", ncol(output_data), "\n")