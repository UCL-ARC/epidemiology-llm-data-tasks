# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all datasets from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID using full_join
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to harmonize sexual orientation variable from Wave 6 (Age 19)
harmonize_sori_w6 <- function(x) {
  case_when(
    x %in% c(-97, -92) ~ -9,  # Refusal codes -> -9
    x == -91 ~ -1,  # Not applicable -> -1
    x == -1 ~ -8,  # Don't know -> -8
    x %in% 1:4 ~ x,  # Valid responses stay the same
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Function to harmonize sexual orientation variable from Wave 7 (Age 20)
harmonize_sori_w7 <- function(x) {
  case_when(
    x %in% c(-100, -97, -92) ~ -9,  # Refusal codes -> -9
    x == -91 ~ -1,  # Not applicable -> -1
    x == -1 ~ -8,  # Don't know -> -8
    x %in% 1:4 ~ x,  # Valid responses stay the same
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Function to harmonize sexual orientation variable from Wave 8 (Age ~25)
harmonize_sori_w8 <- function(x) {
  case_when(
    x == -9 ~ -9,  # Refused -> -9
    x == -8 ~ -8,  # Don't know -> -8
    x == -1 ~ -1,  # Not applicable -> -1
    x %in% 1:4 ~ x,  # Valid responses stay the same
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Function to harmonize sexual orientation variable from Wave 9 (Age 32)
harmonize_sori_w9 <- function(x) {
  case_when(
    x == -9 ~ -9,  # Refused -> -9
    x == -8 ~ -8,  # Don't know -> -8
    x == -3 ~ -3,  # Not asked -> -3
    x == -1 ~ -1,  # Not applicable -> -1
    x == 5 ~ -7,  # Prefer not to say -> -7
    x %in% 1:4 ~ x,  # Valid responses stay the same
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Create age-specific sexual orientation variables with harmonized codes
cleaned_data <- merged_data %>%
  mutate(
    sori19 = harmonize_sori_w6(W6SexualityYP),
    sori20 = harmonize_sori_w7(W7SexualityYP),
    sori25 = harmonize_sori_w8(W8SEXUALITY),
    sori32 = harmonize_sori_w9(W9SORI)
  )

# Convert sexual orientation variables to factors with explicit labels
cleaned_data <- cleaned_data %>%
  mutate(
    sori19 = factor(sori19, 
                    levels = c(-9, -8, -7, -3, -1, 1, 2, 3, 4),
                    labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Not applicable",
                               "Heterosexual/Straight", "Gay/Lesbian", "Bisexual", "Other")),
    sori20 = factor(sori20, 
                    levels = c(-9, -8, -7, -3, -1, 1, 2, 3, 4),
                    labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Not applicable",
                               "Heterosexual/Straight", "Gay/Lesbian", "Bisexual", "Other")),
    sori25 = factor(sori25, 
                    levels = c(-9, -8, -7, -3, -1, 1, 2, 3, 4),
                    labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Not applicable",
                               "Heterosexual/Straight", "Gay/Lesbian", "Bisexual", "Other")),
    sori32 = factor(sori32, 
                    levels = c(-9, -8, -7, -3, -1, 1, 2, 3, 4),
                    labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Not applicable",
                               "Heterosexual/Straight", "Gay/Lesbian", "Bisexual", "Other"))
  )

# Select final variables for output
# Include NSID and the derived sexual orientation variables
final_data <- cleaned_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write output to data/output/cleaned_data.csv
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(final_data), "\n")
cat("Number of columns:", ncol(final_data), "\n")