
library(haven)
library(dplyr)
library(readr)
library(forcats)

# Load all required files with proper column types
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Function to map missing values
map_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x %in% c(-999, -99, -98, -94, -996)] <- -3
  return(x)
}

# Define mapping for labels
label_mapping <- tibble(
  value = c(-3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  label = c(
    "Not asked at the fieldwork stage / not interviewed",
    "Doing paid work for 30 or more hours a week",
    "Doing paid work for fewer than 30 hours a week",
    "Unemployed/ Looking for a job",
    "On a training course or scheme",
    "In full-time education/ at school",
    "Looking after the family/ household",
    "Retired from work altogether",
    "Sick/ disabled",
    "Other"
  )
)

# Create a function to process variables
process_var <- function(data, source_var, output_var) {
  processed <- map_missing(data[[source_var]])

  # Create factor with custom labels
  data[[output_var]] <- factor(
    processed,
    levels = label_mapping$value,
    labels = label_mapping$label
  )

  return(data)
}

# Process each dataset
wave1 <- process_var(wave1, "W1empsmum", "ecoactma14")
wave1 <- process_var(wave1, "W1empsdad", "ecoactpa14")

wave2 <- process_var(wave2, "W2empsmum", "ecoactma15")
wave2 <- process_var(wave2, "W2empsdad", "ecoactpa15")

wave3 <- process_var(wave3, "W3empsmum", "ecoactma16")
wave3 <- process_var(wave3, "W3empsdad", "ecoactpa16")

wave4 <- process_var(wave4, "w4empsmum", "ecoactma17")
wave4 <- process_var(wave4, "w4empsdad", "ecoactpa17")

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Select only NSID and ecoact variables
output_data <- merged_data %>%
  select(NSID, contains("ecoact"))

# Create output directory if needed
if (!dir.exists("data/output")) {
  dir.create("data/output")
}

# Write output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print confirmation
cat("Processing complete. Output file written to data/output/cleaned_data.csv\n")
