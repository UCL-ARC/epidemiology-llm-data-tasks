# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(haven)

# Load all wave files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = "\t")
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = "\t")
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = "\t")
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = "\t")

# Merge all waves by NSID
cleaned_data <- full_join(wave1, wave2, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave3, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave4, by = "NSID")

# Define recode function for economic activity variables
# Map missing codes based on label meanings
recode_economic <- function(x) {
  case_when(
    # Standard missing codes
    x == -99 ~ -3,  # Not interviewed
    x == -98 ~ -3,  # Not present
    x == -996 ~ -3, # No parent in household
    x == -999 ~ -2, # Missing household information
    x == -94 ~ -8,  # Insufficient information
    x == -92 ~ -9,  # Refusal
    # Substantive categories (1-9)
    x %in% 1:9 ~ x,
    # Any other values (shouldn't happen but handle safely)
    TRUE ~ x
  )
}

# Create mother economic activity variables for all waves
cleaned_data <- cleaned_data %>%
  mutate(
    ecoactma14 = recode_economic(W1empsmum),
    ecoactma15 = recode_economic(W2empsmum),
    ecoactma16 = recode_economic(W3empsmum),
    ecoactma17 = recode_economic(w4empsmum)
  )

# Create father economic activity variables for all waves
cleaned_data <- cleaned_data %>%
  mutate(
    ecoactpa14 = recode_economic(W1empsdad),
    ecoactpa15 = recode_economic(W2empsdad),
    ecoactpa16 = recode_economic(W3empsdad),
    ecoactpa17 = recode_economic(w4empsdad)
  )

# Select only NSID and the 8 economic activity variables
final_data <- cleaned_data %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, 
         ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write to output file
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print summary
cat("Summary of cleaned data:\n")
str(final_data)
