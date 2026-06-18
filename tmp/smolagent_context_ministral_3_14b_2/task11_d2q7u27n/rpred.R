# Load required libraries
library(dplyr)
library(readr)

# Define file paths
file_paths <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load datasets with explicit column types
wave1 <- readr::read_delim(file_paths[1], delim = "\t", col_types = cols(NSID = col_character()))
wave2 <- readr::read_delim(file_paths[2], delim = "\t", col_types = cols(NSID = col_character()))
wave3 <- readr::read_delim(file_paths[3], delim = "\t", col_types = cols(NSID = col_character()))
wave4 <- readr::read_delim(file_paths[4], delim = "\t", col_types = cols(NSID = col_character()))

# Function to harmonize missing values
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -3
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[x == -996] <- -3
  x[x == -94] <- -3
  x[x == -92] <- -9
  x[x == -97] <- -3
  x[x == -91] <- -1
  return(x)
}

# Process each wave individually
wave1_processed <- data.frame(
  NSID = wave1$NSID,
  ecoactma14 = harmonize_missing(wave1[[302]]),
  ecoactpa14 = harmonize_missing(wave1[[303]])
)

wave2_processed <- data.frame(
  NSID = wave2$NSID,
  ecoactma15 = harmonize_missing(wave2[[805]]),
  ecoactpa15 = harmonize_missing(wave2[[804]])
)

wave3_processed <- data.frame(
  NSID = wave3$NSID,
  ecoactma16 = harmonize_missing(wave3[[82]]),
  ecoactpa16 = harmonize_missing(wave3[[81]])
)

wave4_processed <- data.frame(
  NSID = wave4$NSID,
  ecoactma17 = harmonize_missing(wave4[[375]]),
  ecoactpa17 = harmonize_missing(wave4[[374]])
)

# Combine all waves
combined_data <- full_join(wave1_processed, wave2_processed, by = "NSID")
combined_data <- full_join(combined_data, wave3_processed, by = "NSID")
combined_data <- full_join(combined_data, wave4_processed, by = "NSID")

# Define labels for ecoact variables
ecoact_labels <- c(
  '-9' = 'Refusal',
  '-8' = 'Dont know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage',
  '-2' = 'Schedule not applicable',
  '-1' = 'Item not applicable',
  '1' = 'Paid work 30+ hours',
  '2' = 'Paid work <30 hours',
  '3' = 'Unemployed',
  '4' = 'Training course',
  '5' = 'Full-time education',
  '6' = 'Looking after family',
  '7' = 'Retired',
  '8' = 'Sick/disabled',
  '9' = 'Other'
)

# Convert ecoact variables to factors
ecoact_vars <- grep("ecoact", names(combined_data), value = TRUE)
for (var in ecoact_vars) {
  combined_data[[var]] <- factor(combined_data[[var]], 
                                levels = c(-9, -8, -7, -3, -2, -1, 1:9), 
                                labels = ecoact_labels)
}

# Write output
readr::write_csv(combined_data, "data/output/cleaned_data.csv")