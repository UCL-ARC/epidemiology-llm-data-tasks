# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(purrr)
library(labelled)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load each file
data_w1 <- read_delim(files[1], delim = "\t")
data_w2 <- read_delim(files[2], delim = "\t")
data_w4 <- read_delim(files[3], delim = "\t")
data_w8 <- read_delim(files[4], delim = "\t")
data_w9 <- read_delim(files[5], delim = "\t")

# Merge all datasets by NSID
cleaned_data <- full_join(data_w1, data_w2, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_w4, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_w8, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_w9, by = "NSID")

# Function to standardize missing values
standardize_missing <- function(x) {
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x[x == -99] <- -3
  x[is.na(x)] <- -3
  return(x)
}

# Standardize missing values for all ethnicity variables
cleaned_data$W1ethnic2YP <- standardize_missing(cleaned_data$W1ethnic2YP)
cleaned_data$W2ethnicYP <- standardize_missing(cleaned_data$W2ethnicYP)
cleaned_data$w4ethnic2YP <- standardize_missing(cleaned_data$w4ethnic2YP)
cleaned_data$W8DETHN15 <- standardize_missing(cleaned_data$W8DETHN15)
cleaned_data$W9DETHN15 <- standardize_missing(cleaned_data$W9DETHN15)

# Create consolidated eth variable using earliest valid response first
create_eth <- function(w1, w2, w4, w8, w9) {
  result <- rep(-3, length(w1))
  
  valid_w1 <- w1 >= 1 & w1 <= 16
  result[valid_w1] <- w1[valid_w1]
  
  missing_w1 <- is.na(result) | result == -3 | result == -2 | result == -1 | result == -8 | result == -9
  valid_w2 <- w2 >= 1 & w2 <= 16
  result[missing_w1 & valid_w2] <- w2[missing_w1 & valid_w2]
  
  missing_w12 <- is.na(result) | result == -3 | result == -2 | result == -1 | result == -8 | result == -9
  valid_w4 <- w4 >= 1 & w4 <= 16
  result[missing_w12 & valid_w4] <- w4[missing_w12 & valid_w4]
  
  missing_w124 <- is.na(result) | result == -3 | result == -2 | result == -1 | result == -8 | result == -9
  valid_w8 <- w8 >= 1 & w8 <= 16
  result[missing_w124 & valid_w8] <- w8[missing_w124 & valid_w8]
  
  missing_w1248 <- is.na(result) | result == -3 | result == -2 | result == -1 | result == -8 | result == -9
  valid_w9 <- w9 >= 1 & w9 <= 16
  result[missing_w1248 & valid_w9] <- w9[missing_w1248 & valid_w9]
  
  return(result)
}

# Create the eth variable
cleaned_data$eth <- create_eth(
  cleaned_data$W1ethnic2YP,
  cleaned_data$W2ethnicYP,
  cleaned_data$w4ethnic2YP,
  cleaned_data$W8DETHN15,
  cleaned_data$W9DETHN15
)

# Select only the ID and eth variables for output
output_data <- cleaned_data %>%
  select(NSID, eth)

# Write the output
cleaned_output_dir <- "data/output"
if (!dir.exists(cleaned_output_dir)) {
  dir.create(cleaned_output_dir, recursive = TRUE)
}

write_csv(output_data, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Total observations:", nrow(output_data), "\n")
cat("Unique ethnicity categories:", length(unique(output_data$eth)), "\n")