library(haven)
library(dplyr)
library(tidyr)
library(labelled)
library(readr)
library(purrr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_young_person_2020.tab",
  "data/input/wave_three_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_self_completion.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave1 <- read_delim(files[1], delim = "\t")
wave2 <- read_delim(files[2], delim = "\t")
wave3 <- read_delim(files[3], delim = "\t")
wave4 <- read_delim(files[4], delim = "\t")
wave6 <- read_delim(files[5], delim = "\t")
wave7 <- read_delim(files[6], delim = "\t")
wave8 <- read_delim(files[7], delim = "\t")
wave9 <- read_delim(files[8], delim = "\t")

# Combine all waves with full join
data <- full_join(wave1, wave2, by = "NSID")
data <- full_join(data, wave3, by = "NSID")
data <- full_join(data, wave4, by = "NSID")
data <- full_join(data, wave6, by = "NSID")
data <- full_join(data, wave7, by = "NSID")
data <- full_join(data, wave8, by = "NSID")
data <- full_join(data, wave9, by = "NSID")

# Derive drinking status for each wave with proper missing handling
data <- mutate(data, 
  d14 = ifelse(
    !is.na(W1alceverYP) & !is.na(W1alcmonYP) &
    W1alceverYP == 1 & W1alcmonYP == 1,
    1,  # drinking
    ifelse(
      !is.na(W1alceverYP) & !is.na(W1alcmonYP),
      0,  # not drinking
      NA  # missing
    )
  ),
  d15 = ifelse(
    !is.na(W2alceverYP) & W2alceverYP == 1,
    1,
    ifelse(
      !is.na(W2alceverYP) & W2alceverYP == 2,
      0,
      NA
    )
  ),
  d16 = ifelse(
    !is.na(W3alceverYP) & W3alceverYP == 1,
    1,
    ifelse(
      !is.na(W3alceverYP) & W3alceverYP == 2,
      0,
      NA
    )
  ),
  d17 = ifelse(
    !is.na(W4AlcEverYP) & W4AlcEverYP == 1,
    1,
    ifelse(
      !is.na(W4AlcEverYP) & W4AlcEverYP == 2,
      0,
      NA
    )
  ),
  d19 = ifelse(
    !is.na(W6AlcEverYP) & W6AlcEverYP == 1,
    1,
    ifelse(
      !is.na(W6AlcEverYP) & W6AlcEverYP == 2,
      0,
      NA
    )
  ),
  d20 = ifelse(
    !is.na(W7AlcEverYP) & W7AlcEverYP == 1,
    1,
    ifelse(
      !is.na(W7AlcEverYP) & W7AlcEverYP == 2,
      0,
      NA
    )
  ),
  d25 = ifelse(
    !is.na(W8AUDIT1) & W8AUDIT1 > 1,
    1,
    ifelse(
      !is.na(W8AUDIT1) & W8AUDIT1 == 1,
      0,
      NA
    )
  ),
  d32 = ifelse(
    !is.na(W9AUDIT1) & W9AUDIT1 > 1,
    1,
    ifelse(
      !is.na(W9AUDIT1) & W9AUDIT1 == 1,
      0,
      NA
    )
  )
)

# Function to calculate alcfst - fixed to handle NAs properly
calc_alcfst <- function(d14, d15, d16, d17, d19, d20, d25, d32) {
  ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
  drinking_vec <- c(d14, d15, d16, d17, d19, d20, d25, d32)
  
  # Check if any drinking observed (use sum with na.rm=TRUE)
  drinking_observed <- sum(drinking_vec == 1, na.rm = TRUE) > 0
  
  # Check if any missing
  has_missing <- any(is.na(drinking_vec))
  
  if (drinking_observed) {
    # Find earliest age with drinking
    for (i in seq_along(ages)) {
      if (!is.na(drinking_vec[i]) & drinking_vec[i] == 1) {
        return(ages[i])
      }
    }
  } else if (has_missing) {
    # No drinking observed but some missing - insufficient information
    return(-8)
  } else {
    # No drinking and no missing - never drank
    return(99)
  }
}

# Apply the function row-wise
data <- data %>%
  mutate(
    alcfst = pmap_dbl(
      list(d14, d15, d16, d17, d19, d20, d25, d32),
      calc_alcfst
    )
  )

# Create the factor variable with proper levels
alcfst <- factor(data$alcfst, 
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don\'t know/insufficient information")
)

# Replace the raw numeric with the factor
data$alcfst <- alcfst

# Remove all intermediate variables that we don\'t need
keep_vars <- c("NSID", "alcfst")
data <- data[, keep_vars]

# Write output
write_csv(data, "data/output/cleaned_data.csv")

cat("Script completed successfully.\n")