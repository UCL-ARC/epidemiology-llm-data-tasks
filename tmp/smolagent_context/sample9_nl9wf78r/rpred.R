
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Suppress column specification messages
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
})

# Define file paths
wave_one <- "data/input/wave_one_lsype_family_background_2020.tab"
wave_two <- "data/input/wave_two_lsype_family_background_2020.tab"
wave_four <- "data/input/wave_four_lsype_family_background_2020.tab"

# Load datasets
cat("Loading wave one data...")
wave_one_data <- read_delim(wave_one, delim = "\t", col_types = cols(NSID = col_character()))
cat("Loading wave two data...")
wave_two_data <- read_delim(wave_two, delim = "\t", col_types = cols(NSID = col_character()))
cat("Loading wave four data...")
wave_four_data <- read_delim(wave_four, delim = "\t", col_types = cols(NSID = col_character()))

# Select relevant variables
cat("Selecting variables...")
wave_one_data <- wave_one_data %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave_two_data <- wave_two_data %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave_four_data <- wave_four_data %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge datasets
cat("Merging datasets...")
merged_data <- full_join(wave_one_data, wave_two_data, by = "NSID") %>%
  full_join(wave_four_data, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -3
  x[x == -99] <- -1
  x[x == -98] <- -1
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  return(x)
}

# Apply harmonization
cat("Harmonizing missing values...")
merged_data$W1hiqualmum <- harmonize_missing(merged_data$W1hiqualmum)
merged_data$W1hiqualdad <- harmonize_missing(merged_data$W1hiqualdad)
merged_data$W2hiqualmum <- harmonize_missing(merged_data$W2hiqualmum)
merged_data$W2hiqualdad <- harmonize_missing(merged_data$W2hiqualdad)
merged_data$w4hiqualmum <- harmonize_missing(merged_data$w4hiqualmum)
merged_data$w4hiqualdad <- harmonize_missing(merged_data$w4hiqualdad)

# Consolidate education variables
consolidate_education <- function(data, mother = TRUE) {
  if (mother) {
    waves <- c("W1hiqualmum", "W2hiqualmum", "w4hiqualmum")
  } else {
    waves <- c("W1hiqualdad", "W2hiqualdad", "w4hiqualdad")
  }

  for (wave in waves) {
    if (!all(is.na(data[[wave]]))) {
      return(data[[wave]])
    }
  }
  return(-3)  # Default to -3 for completely missing data
}

# Consolidate maternal and paternal education
cat("Consolidating education variables...")
merged_data$educdtlma <- consolidate_education(merged_data, mother = TRUE)
merged_data$educdtlpa <- consolidate_education(merged_data, mother = FALSE)

# Function to derive collapsed variables
derive_collapsed <- function(detailed_var) {
  if (is.na(detailed_var)) {
    return(NA)
  }

  detailed_var <- as.numeric(detailed_var)

  if (detailed_var >= 1 && detailed_var <= 4) {
    return(0)
  } else if (detailed_var >= 5 && detailed_var <= 17) {
    return(1)
  } else if (detailed_var == 18) {
    return(2)
  } else if (detailed_var == 19) {
    return(3)
  } else if (detailed_var == 20) {
    return(4)
  } else {
    return(NA)
  }
}

# Apply collapsed variable derivation
cat("Deriving collapsed variables...")
merged_data$educma <- sapply(merged_data$educdtlma, function(x) {
  if (is.na(x)) {
    return(-3)
  } else {
    return(derive_collapsed(x))
  }
})

merged_data$educpa <- sapply(merged_data$educdtlpa, function(x) {
  if (is.na(x)) {
    return(-3)
  } else {
    return(derive_collapsed(x))
  }
})

# Select final variables
cat("Selecting final variables...")
final_data <- merged_data %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

# Check dimensions of final data
cat("Final dataset dimensions:", nrow(final_data), "rows,", ncol(final_data), "columns\n")

# Write to CSV
cat("Writing output file...")
write_csv(final_data, "data/output/cleaned_data.csv")
cat("Output file created successfully at data/output/cleaned_data.csv\n")
