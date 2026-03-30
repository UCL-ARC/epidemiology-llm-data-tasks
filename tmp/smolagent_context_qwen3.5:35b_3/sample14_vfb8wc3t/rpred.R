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
wave1 <- read_delim(files[1], delim = "\t")
wave2 <- read_delim(files[2], delim = "\t")
wave3 <- read_delim(files[3], delim = "\t")
wave4 <- read_delim(files[4], delim = "\t")
wave5 <- read_delim(files[5], delim = "\t")
wave6 <- read_delim(files[6], delim = "\t")
wave7 <- read_delim(files[7], delim = "\t")
wave8 <- read_delim(files[8], delim = "\t")
wave9 <- read_delim(files[9], delim = "\t")

# Merge all datasets by NSID using full_join
cleaned_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to create collapsed tenure variable
collapse_tenure <- function(detailed_var) {
  collapsed <- detailed_var
  collapsed[detailed_var == 1] <- 1
  collapsed[detailed_var == 2] <- 2
  collapsed[detailed_var == 3] <- 3
  collapsed[detailed_var %in% c(4, 5, 6, 7)] <- 4
  collapsed[detailed_var == 8] <- 6
  return(collapsed)
}

# Process Age 14 (Wave 1)
hownteen14_raw <- cleaned_data$W1hous12HH
hownteen14_raw[hownteen14_raw == -999] <- -3
hownteen14_raw[hownteen14_raw == -92] <- -9
hownteen14_raw[hownteen14_raw == -91] <- -1
hownteen14_raw[hownteen14_raw == -1] <- -8

hown14_raw <- collapse_tenure(hownteen14_raw)

# Process Age 15 (Wave 2)
hownteen15_raw <- cleaned_data$W2Hous12HH
hownteen15_raw[hownteen15_raw == -998] <- -3
hownteen15_raw[hownteen15_raw == -997] <- -2
hownteen15_raw[hownteen15_raw == -995] <- -3
hownteen15_raw[hownteen15_raw == -99] <- -3
hownteen15_raw[hownteen15_raw == -92] <- -9
hownteen15_raw[hownteen15_raw == -91] <- -1
hownteen15_raw[hownteen15_raw == -1] <- -8

hown15_raw <- collapse_tenure(hownteen15_raw)

# Process Age 16 (Wave 3)
hownteen16_raw <- cleaned_data$W3hous12HH
hownteen16_raw[hownteen16_raw == -999] <- -3
hownteen16_raw[hownteen16_raw == -99] <- -3
hownteen16_raw[hownteen16_raw == -92] <- -9
hownteen16_raw[hownteen16_raw == -91] <- -1
hownteen16_raw[hownteen16_raw == -1] <- -8

hown16_raw <- collapse_tenure(hownteen16_raw)

# Process Age 17 (Wave 4)
hownteen17_raw <- cleaned_data$W4Hous12HH
hownteen17_raw[hownteen17_raw == -999] <- -3
hownteen17_raw[hownteen17_raw == -997] <- -2
hownteen17_raw[hownteen17_raw == -92] <- -9
hownteen17_raw[hownteen17_raw == -91] <- -1
hownteen17_raw[hownteen17_raw == -1] <- -8

hown17_raw <- collapse_tenure(hownteen17_raw)

# Process Age 18 (Wave 5) - Multi-variable
hownteen18_raw <- cleaned_data$W5Hous12HH
hownteen18_raw[hownteen18_raw == -999] <- -3
hownteen18_raw[hownteen18_raw == -92] <- -9
hownteen18_raw[hownteen18_raw == -91] <- -1
hownteen18_raw[hownteen18_raw == -1] <- -8

# For code 1 (Owned), look at W5Hous12BHH
owned_idx <- which(cleaned_data$W5Hous12HH == 1)
if (length(owned_idx) > 0) {
  owned_housing <- cleaned_data$W5Hous12BHH[owned_idx]
  owned_housing[owned_housing == -999] <- -3
  owned_housing[owned_housing == -92] <- -9
  owned_housing[owned_housing == -91] <- -1
  owned_housing[owned_housing == -1] <- -8
  hownteen18_raw[owned_idx] <- owned_housing
}

# For code 2 (Rented), look at W5Hous12CHH
rented_idx <- which(cleaned_data$W5Hous12HH == 2)
if (length(rented_idx) > 0) {
  rented_housing <- cleaned_data$W5Hous12CHH[rented_idx]
  rented_housing[rented_housing == -999] <- -3
  rented_housing[rented_housing == -92] <- -9
  rented_housing[rented_housing == -91] <- -1
  rented_housing[rented_housing == -1] <- -8
  hownteen18_raw[rented_idx] <- rented_housing
}

hown18_raw <- collapse_tenure(hownteen18_raw)

# Process Age 19 (Wave 6) - Multi-variable
hownteen19_raw <- cleaned_data$W6Hous12YP
hownteen19_raw[hownteen19_raw == -92] <- -9
hownteen19_raw[hownteen19_raw == -91] <- -1
hownteen19_raw[hownteen19_raw == -1] <- -8

owned_idx <- which(cleaned_data$W6Hous12YP == 1)
if (length(owned_idx) > 0) {
  owned_housing <- cleaned_data$W6Hous12bYP[owned_idx]
  owned_housing[owned_housing == -92] <- -9
  owned_housing[owned_housing == -91] <- -1
  owned_housing[owned_housing == -1] <- -8
  hownteen19_raw[owned_idx] <- owned_housing
}

rented_idx <- which(cleaned_data$W6Hous12YP == 2)
if (length(rented_idx) > 0) {
  rented_housing <- cleaned_data$W6Hous12cYP[rented_idx]
  rented_housing[rented_housing == -92] <- -9
  rented_housing[rented_housing == -91] <- -1
  rented_housing[rented_housing == -1] <- -8
  hownteen19_raw[rented_idx] <- rented_housing
}

hown19_raw <- collapse_tenure(hownteen19_raw)

# Process Age 20 (Wave 7) - Multi-variable
hownteen20_raw <- cleaned_data$W7Hous12YP
hownteen20_raw[hownteen20_raw == -92] <- -9
hownteen20_raw[hownteen20_raw == -91] <- -1
hownteen20_raw[hownteen20_raw == -1] <- -8

owned_idx <- which(cleaned_data$W7Hous12YP == 1)
if (length(owned_idx) > 0) {
  owned_housing <- cleaned_data$W7Hous12bYP[owned_idx]
  owned_housing[owned_housing == -92] <- -9
  owned_housing[owned_housing == -91] <- -1
  owned_housing[owned_housing == -1] <- -8
  hownteen20_raw[owned_idx] <- owned_housing
}

rented_idx <- which(cleaned_data$W7Hous12YP == 2)
if (length(rented_idx) > 0) {
  rented_housing <- cleaned_data$W7Hous12cYP[rented_idx]
  rented_housing[rented_housing == -92] <- -9
  rented_housing[rented_housing == -91] <- -1
  rented_housing[rented_housing == -1] <- -8
  hownteen20_raw[rented_idx] <- rented_housing
}

hown20_raw <- collapse_tenure(hownteen20_raw)

# Process Age 25 (Wave 8)
hown25_raw <- cleaned_data$W8TENURE
hown25_raw[is.na(hown25_raw)] <- -3

# Process Age 32 (Wave 9)
hown32_raw <- cleaned_data$W9DTENURE
hown32_raw[is.na(hown32_raw)] <- -3

# Create final data frame with just the required columns
final_data <- data.frame(
  NSID = cleaned_data$NSID,
  hown14 = hown14_raw,
  hown15 = hown15_raw,
  hown16 = hown16_raw,
  hown17 = hown17_raw,
  hown18 = hown18_raw,
  hown19 = hown19_raw,
  hown20 = hown20_raw,
  hown25 = hown25_raw,
  hown32 = hown32_raw,
  hownteen14 = hownteen14_raw,
  hownteen15 = hownteen15_raw,
  hownteen16 = hownteen16_raw,
  hownteen17 = hownteen17_raw,
  hownteen18 = hownteen18_raw,
  hownteen19 = hownteen19_raw,
  hownteen20 = hownteen20_raw
)

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Print summary
cat("Data cleaning complete.\n")
cat("Number of cases:", nrow(final_data), "\n")
cat("Number of variables:", ncol(final_data), "\n")
cat("Variables in output:\n")
print(names(final_data))
