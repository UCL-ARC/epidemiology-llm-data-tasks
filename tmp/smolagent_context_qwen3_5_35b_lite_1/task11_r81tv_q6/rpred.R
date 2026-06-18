library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Function to convert missing value codes to standard codes
convert_missing_codes <- function(x) {
  # First, convert any negative codes based on their meaning
  x <- case_when(
    # Wave 4 specific codes
    x == -996 ~ -1,  # No parent in household -> Not applicable
    x == -99 ~ -3,   # Not interviewed -> Not asked at fieldwork
    x == -98 ~ -2,   # Not present -> Schedule not applicable
    x == -94 ~ -8,   # Insufficient information -> Don't know
    x == -92 ~ -9,   # Refusal -> Refusal
    x == -999 ~ -2,  # Missing household info -> Schedule not applicable
    TRUE ~ x
  )
  
  # Convert any remaining NA to -3
  x[is.na(x)] <- -3
  
  return(x)
}

# Load all wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W1empsmum, W1empsdad)

wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W2empsmum, W2empsdad)

wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W3empsmum, W3empsdad)

wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, w4empsmum, w4empsdad)

# Merge all waves by NSID
data <- full_join(wave1, wave2, by = "NSID")
data <- full_join(data, wave3, by = "NSID")
data <- full_join(data, wave4, by = "NSID")

# Create mother economic activity variables for each wave
data$ecoactma14 <- convert_missing_codes(data$W1empsmum)
data$ecoactpa14 <- convert_missing_codes(data$W1empsdad)
data$ecoactma15 <- convert_missing_codes(data$W2empsmum)
data$ecoactpa15 <- convert_missing_codes(data$W2empsdad)
data$ecoactma16 <- convert_missing_codes(data$W3empsmum)
data$ecoactpa16 <- convert_missing_codes(data$W3empsdad)
data$ecoactma17 <- convert_missing_codes(data$w4empsmum)
data$ecoactpa17 <- convert_missing_codes(data$w4empsdad)

# Keep only NSID and the new variables
output <- data %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, 
         ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
