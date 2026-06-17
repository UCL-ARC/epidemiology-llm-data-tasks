library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Read all files
wave1_data <- read_delim(files[1], delim = "\t")
wave2_data <- read_delim(files[2], delim = "\t")
wave3_data <- read_delim(files[3], delim = "\t")
wave4_data <- read_delim(files[4], delim = "\t")

# Define value labels for substantive categories
substantive_labels <- c(
  "1" = "Doing paid work for 30 or more hours a week",
  "2" = "Doing paid work for fewer than 30 hours a week",
  "3" = "Unemployed/ Looking for a job",
  "4" = "On a training course or scheme",
  "5" = "In full-time education/ at school",
  "6" = "Looking after the family/ household",
  "7" = "Retired from work altogether",
  "8" = "Sick/ disabled",
  "9" = "Other"
)

missing_labels <- c(
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Function to process the variable
process_var <- function(var, source_name) {
  v <- as.numeric(var)
  # Map specific missing codes to -3
  v[v == -99] <- -3
  v[v == -98] <- -3
  v[v == -996] <- -3
  # Convert NA to -3 (not asked at fieldwork stage)
  v[is.na(v)] <- -3
  # Convert to character for labelled
  v <- as.character(v)
  # Create labelled variable with all labels
  all_labels <- c(substantive_labels, missing_labels)
  result <- labelled(v, labels = all_labels)
  return(result)
}

# Process all variables
ecoactma14 <- process_var(wave1_data$W1empsmum, "W1empsmum")
ecoactpa14 <- process_var(wave1_data$W1empsdad, "W1empsdad")
ecoactma15 <- process_var(wave2_data$W2empsmum, "W2empsmum")
ecoactpa15 <- process_var(wave2_data$W2empsdad, "W2empsdad")
ecoactma16 <- process_var(wave3_data$W3empsmum, "W3empsmum")
ecoactpa16 <- process_var(wave3_data$W3empsdad, "W3empsdad")
ecoactma17 <- process_var(wave4_data$w4empsmum, "w4empsmum")
ecoactpa17 <- process_var(wave4_data$w4empsdad, "w4empsdad")

# Create data frames
wave1_df <- data.frame(NSID = wave1_data$NSID, ecoactma14 = ecoactma14, ecoactpa14 = ecoactpa14)
wave2_df <- data.frame(NSID = wave2_data$NSID, ecoactma15 = ecoactma15, ecoactpa15 = ecoactpa15)
wave3_df <- data.frame(NSID = wave3_data$NSID, ecoactma16 = ecoactma16, ecoactpa16 = ecoactpa16)
wave4_df <- data.frame(NSID = wave4_data$NSID, ecoactma17 = ecoactma17, ecoactpa17 = ecoactpa17)

# Full join all waves
cleaned <- full_join(wave1_df, wave2_df, by = "NSID") %>%
  full_join(wave3_df, by = "NSID") %>%
  full_join(wave4_df, by = "NSID")

# Write output
write_csv(cleaned, "data/output/cleaned_data.csv")
cat("Script completed successfully\n")
