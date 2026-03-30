# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load each file
wave1 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
wave2 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(files[5], delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
cleaned_data <- full_join(wave1, wave2, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave4, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave8, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave9, by = "NSID")

# Function to convert wave-specific missing codes to standard codes
convert_missing_codes <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -3  # Missing - household data lost
  x[x == -998] <- -3  # Interviewer missed question
  x[x == -997] <- -3  # Script error
  x[x == -995] <- -3  # Missing history section data - unexplained
  x[x == -99] <- -3   # YP not interviewed
  x[x == -94] <- -8   # Insufficient information
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -1] <- -8    # Don't know
  x[x == -9] <- -9    # Refused
  x[x == -8] <- -8    # Insufficient information
  x[is.na(x)] <- -3   # Not asked/interviewed
  return(x)
}

# Extract ethnicity variables from merged data (they will be NA for rows without that wave's data)
w1_ethnic14 <- convert_missing_codes(cleaned_data$W1ethnic2YP)
w2_ethnic15 <- convert_missing_codes(cleaned_data$W2ethnicYP)
w4_ethnic17 <- convert_missing_codes(cleaned_data$w4ethnic2YP)
w8_ethnic23 <- convert_missing_codes(cleaned_data$W8DETHN15)
w9_ethnic32 <- convert_missing_codes(cleaned_data$W9DETHN15)

# Create consolidated ethnicity variable (time-invariant)
# Prioritize earliest valid response for ethnicity
consolidated_ethnic <- rep(NA_integer_, nrow(cleaned_data))

# Start with wave 1
consolidated_ethnic[w1_ethnic14 %in% 1:16] <- w1_ethnic14[w1_ethnic14 %in% 1:16]

# Fill in gaps with wave 2
missing_idx <- is.na(consolidated_ethnic)
consolidated_ethnic[missing_idx & w2_ethnic15 %in% 1:16] <- w2_ethnic15[missing_idx & w2_ethnic15 %in% 1:16]

# Fill in gaps with wave 4
missing_idx <- is.na(consolidated_ethnic)
consolidated_ethnic[missing_idx & w4_ethnic17 %in% 1:16] <- w4_ethnic17[missing_idx & w4_ethnic17 %in% 1:16]

# Fill in gaps with wave 8
missing_idx <- is.na(consolidated_ethnic)
consolidated_ethnic[missing_idx & w8_ethnic23 %in% 1:16] <- w8_ethnic23[missing_idx & w8_ethnic23 %in% 1:16]

# Fill in gaps with wave 9
missing_idx <- is.na(consolidated_ethnic)
consolidated_ethnic[missing_idx & w9_ethnic32 %in% 1:16] <- w9_ethnic32[missing_idx & w9_ethnic32 %in% 1:16]

# Replace remaining NAs with -3 (not asked)
consolidated_ethnic[is.na(consolidated_ethnic)] <- -3

# Create factor with labels for ethnicity
ethnic_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background",
  "-9" = "Refused",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed"
)

consolidated_ethnic <- factor(consolidated_ethnic, 
                               levels = c(1:16, -9, -8, -1, -3),
                               labels = ethnic_labels)

# Select only the variables we need for output
output_data <- cleaned_data %>%
  select(NSID) %>%
  mutate(
    eth = consolidated_ethnic,
    eth14 = w1_ethnic14,
    eth15 = w2_ethnic15,
    eth17 = w4_ethnic17,
    eth23 = w8_ethnic23,
    eth32 = w9_ethnic32
  )

# Write output to CSV
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
