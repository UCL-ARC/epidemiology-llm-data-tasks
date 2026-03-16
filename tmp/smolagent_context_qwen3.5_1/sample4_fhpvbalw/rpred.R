# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

cat("Loading datasets...\n")

# Load all datasets from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

cat("Datasets loaded successfully.\n")
cat("Wave 1 (Age 14):", nrow(wave1), "cases\n")
cat("Wave 4 (Age 17):", nrow(wave4), "cases\n")
cat("Wave 6 (Age 19):", nrow(wave6), "cases\n")
cat("Wave 7 (Age 20):", nrow(wave7), "cases\n")
cat("Wave 8:", nrow(wave8), "cases\n")
cat("Wave 9 (Age 32):", nrow(wave9), "cases\n")

# Merge all datasets by NSID using full_join
cat("\nMerging datasets...\n")
data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

cat("Merged dataset:", nrow(data), "cases\n")

# Function to harmonize sexual orientation missing values to standard codes
# Standard codes: -9 = Refusal, -8 = Don't know, -7 = Prefer not to say, 
#                 -1 = Not applicable, -3 = Not asked, -2 = Schedule not applicable
harmonize_sori <- function(x, wave) {
  # First replace NA with -3 (Not asked at fieldwork stage)
  x[is.na(x)] <- -3
  
  # Convert wave-specific missing codes to standard codes
  if (wave == 6) {
    # W6SexualityYP (Age 19): -97, -92, -91, -1
    x <- case_when(
      x == -97 ~ -3,  # Respondent declined self completion -> Not asked
      x == -92 ~ -9,  # Refused -> Refusal
      x == -91 ~ -1,  # Not applicable -> Not applicable
      x == -1 ~ -8,   # Don't know -> Don't know
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 7) {
    # W7SexualityYP (Age 20): -100, -97, -92, -91, -1
    x <- case_when(
      x == -100 ~ -3,  # Respondent declined sexual experience questions -> Not asked
      x == -97 ~ -3,   # Refused self completion -> Not asked
      x == -92 ~ -9,   # Refused -> Refusal
      x == -91 ~ -1,   # Not applicable -> Not applicable
      x == -1 ~ -8,    # Don't know -> Don't know
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 8) {
    # W8SEXUALITY (Wave 8, ~Age 25): -9, -8, -1
    x <- case_when(
      x == -9 ~ -9,  # Refused -> Refusal
      x == -8 ~ -8,  # Don't know -> Don't know
      x == -1 ~ -1,  # Not applicable -> Not applicable
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 9) {
    # W9SORI (Age 32): -9, -8, -3, -1, and 5 (Prefer not to say)
    x <- case_when(
      x == -9 ~ -9,  # Refused -> Refusal
      x == -8 ~ -8,  # Don't know -> Don't know
      x == -3 ~ -3,  # Not asked at fieldwork stage -> Not asked
      x == -1 ~ -1,  # Not applicable -> Not applicable
      x == 5 ~ -7,   # Prefer not to say -> Prefer not to say
      TRUE ~ as.numeric(x)
    )
  }
  return(x)
}

cat("\nHarmonizing sexual orientation variables...\n")

# Create harmonized sexual orientation variables for each wave
# Using age-based naming: sori19, sori20, sori25 (Wave 8 ~age 25), sori32
data <- data %>%
  mutate(
    sori19 = harmonize_sori(W6SexualityYP, 6),
    sori20 = harmonize_sori(W7SexualityYP, 7),
    sori25 = harmonize_sori(W8SEXUALITY, 8),
    sori32 = harmonize_sori(W9SORI, 9)
  )

# Create factor variables with explicit labels for all categories including missing codes
create_sori_factor <- function(x) {
  factor(x, 
         levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4),
         labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", 
                   "Schedule not applicable", "Not applicable",
                   "Heterosexual/Straight", "Gay/Lesbian", "Bisexual", "Other"))
}

data <- data %>%
  mutate(
    sori19 = create_sori_factor(sori19),
    sori20 = create_sori_factor(sori20),
    sori25 = create_sori_factor(sori25),
    sori32 = create_sori_factor(sori32)
  )

# Select only ID and derived variables for output
output_data <- data %>%
  select(NSID, sori19, sori20, sori25, sori32)

cat("\nWriting output to data/output/cleaned_data.csv...\n")

# Write to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Output dimensions:", nrow(output_data), "rows,", ncol(output_data), "columns\n")

# Display summary of output
cat("\nOutput variable summary:\n")
str(output_data)
