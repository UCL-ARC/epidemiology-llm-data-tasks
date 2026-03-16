
# Load required packages
library(dplyr)
library(readr)

# Step 1: Load datasets
wave_one_data <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two_data <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three_data <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four_data <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave_five_data <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave_six_data <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven_data <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_data <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9_data <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Convert NSID to character
wave_one_data$NSID <- as.character(wave_one_data$NSID)
wave_two_data$NSID <- as.character(wave_two_data$NSID)
wave_three_data$NSID <- as.character(wave_three_data$NSID)
wave_four_data$NSID <- as.character(wave_four_data$NSID)
wave_five_data$NSID <- as.character(wave_five_data$NSID)
wave_six_data$NSID <- as.character(wave_six_data$NSID)
wave_seven_data$NSID <- as.character(wave_seven_data$NSID)
ns8_data$NSID <- as.character(ns8_data$NSID)
ns9_data$NSID <- as.character(ns9_data$NSID)

# Step 2: Merge datasets by NSID
merged_data <- wave_one_data %>%
  full_join(wave_two_data, by = "NSID") %>%
  full_join(wave_three_data, by = "NSID") %>%
  full_join(wave_four_data, by = "NSID") %>%
  full_join(wave_five_data, by = "NSID") %>%
  full_join(wave_six_data, by = "NSID") %>%
  full_join(wave_seven_data, by = "NSID") %>%
  full_join(ns8_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

# Step 3: Define recoding function for missing values
recode_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999] <- -3
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  return(x)
}

# Create variables based on metadata
# Age 14 (Wave 1)
if ('W1hous12HH' %in% names(merged_data)) {
  merged_data$hown14_20 <- recode_missing(merged_data$W1hous12HH)
}

# Age 15 (Wave 2)
if ('W2Hous12HH' %in% names(merged_data)) {
  merged_data$hown15_20 <- recode_missing(merged_data$W2Hous12HH)
}

# Age 16 (Wave 3)
if ('W3hous12HH' %in% names(merged_data)) {
  merged_data$hown16_20 <- recode_missing(merged_data$W3hous12HH)
}

# Age 17 (Wave 4)
if ('W4Hous12HH' %in% names(merged_data)) {
  merged_data$hown17_20 <- recode_missing(merged_data$W4Hous12HH)
}

# Age 18 (Wave 5)
if ('W5Hous12YP' %in% names(merged_data)) {
  merged_data$hown18_20 <- recode_missing(merged_data$W5Hous12YP)
}

# Age 19 (Wave 6)
if ('W6Hous12YP' %in% names(merged_data)) {
  merged_data$hown19_20 <- recode_missing(merged_data$W6Hous12YP)
}

# Age 20 (Wave 7)
if ('W7Hous12YP' %in% names(merged_data)) {
  merged_data$hown20_20 <- recode_missing(merged_data$W7Hous12YP)
}

# Age 25 (Wave 8)
if ('W8TENURE' %in% names(merged_data)) {
  merged_data$hown25 <- recode_missing(merged_data$W8TENURE)
}

# Age 32 (Wave 9)
if ('W9DTENURE' %in% names(merged_data)) {
  merged_data$hown32 <- recode_missing(merged_data$W9DTENURE)
}

# Select only NSID and the created variables
required_vars <- c("NSID", "hown14_20", "hown15_20", "hown16_20", "hown17_20",
                   "hown18_20", "hown19_20", "hown20_20", "hown25", "hown32")

# Create cleaned dataset with only existing variables
cleaned_data <- merged_data[, required_vars[required_vars %in% names(merged_data)], drop = FALSE]

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
