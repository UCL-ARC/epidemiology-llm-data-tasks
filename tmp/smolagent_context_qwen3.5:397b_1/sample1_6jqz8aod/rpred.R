# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files from data/input/
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
w5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Function to harmonize missing value codes for sex variables - Wave 1 (Age 14)
harmonize_sex_w1 <- function(x) {
  case_when(
    x == -99 ~ -3,  # YP not interviewed -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Not applicable
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 2 (Age 15)
harmonize_sex_w2 <- function(x) {
  case_when(
    x %in% c(-998, -997, -995, -99) ~ -3,  # Various not asked codes -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Not applicable
    x == -1 ~ -8,  # Don't know -> Don't know
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 3 (Age 16)
harmonize_sex_w3 <- function(x) {
  case_when(
    x == -99 ~ -3,  # YP not interviewed -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Not applicable
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 4 (Age 17)
harmonize_sex_w4 <- function(x) {
  case_when(
    x == -99 ~ -3,  # YP not interviewed -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Not applicable
    x == -1 ~ -8,  # Don't know -> Don't know
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 5 (Age 18)
harmonize_sex_w5 <- function(x) {
  case_when(
    x == -1 ~ -8,  # Don't know -> Don't know
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 6 (Age 19)
harmonize_sex_w6 <- function(x) {
  case_when(
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Not applicable
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 7 (Age 20)
harmonize_sex_w7 <- function(x) {
  case_when(
    x == -91 ~ -1,  # Not applicable -> Not applicable
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 8 (Age ~25-30)
harmonize_sex_w8 <- function(x) {
  case_when(
    x == -9 ~ -9,  # Refused -> Refusal
    x == -8 ~ -8,  # Don't know -> Don't know
    x == -1 ~ -1,  # Not applicable -> Not applicable
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Wave 9 (Age 32)
harmonize_sex_w9 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ x,  # Valid values
    is.na(x) ~ -3,  # NULL -> Not asked
    TRUE ~ x
  )
}

# Merge all waves by NSID using full_join
data <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w5, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Apply harmonization functions to sex variables
data <- data %>%
  mutate(
    W1sexYP_h = harmonize_sex_w1(W1sexYP),
    W2SexYP_h = harmonize_sex_w2(W2SexYP),
    W3sexYP_h = harmonize_sex_w3(W3sexYP),
    W4SexYP_h = harmonize_sex_w4(W4SexYP),
    W5SexYP_h = harmonize_sex_w5(W5SexYP),
    W6Sex_h = harmonize_sex_w6(W6Sex),
    W7Sex_h = harmonize_sex_w7(W7Sex),
    W8CMSEX_h = harmonize_sex_w8(W8CMSEX),
    W9DSEX_h = harmonize_sex_w9(W9DSEX)
  )

# Create consolidated sex variable (time-invariant)
# Prioritize most recent valid response (positive values only)
# Fall back to earlier waves when most recent data is missing
data <- data %>%
  mutate(
    sex = case_when(
      W9DSEX_h %in% c(1, 2) ~ W9DSEX_h,
      W8CMSEX_h %in% c(1, 2) ~ W8CMSEX_h,
      W7Sex_h %in% c(1, 2) ~ W7Sex_h,
      W6Sex_h %in% c(1, 2) ~ W6Sex_h,
      W5SexYP_h %in% c(1, 2) ~ W5SexYP_h,
      W4SexYP_h %in% c(1, 2) ~ W4SexYP_h,
      W3sexYP_h %in% c(1, 2) ~ W3sexYP_h,
      W2SexYP_h %in% c(1, 2) ~ W2SexYP_h,
      W1sexYP_h %in% c(1, 2) ~ W1sexYP_h,
      # If no valid response, use most recent missing code
      !is.na(W9DSEX_h) ~ W9DSEX_h,
      !is.na(W8CMSEX_h) ~ W8CMSEX_h,
      !is.na(W7Sex_h) ~ W7Sex_h,
      !is.na(W6Sex_h) ~ W6Sex_h,
      !is.na(W5SexYP_h) ~ W5SexYP_h,
      !is.na(W4SexYP_h) ~ W4SexYP_h,
      !is.na(W3sexYP_h) ~ W3sexYP_h,
      !is.na(W2SexYP_h) ~ W2SexYP_h,
      !is.na(W1sexYP_h) ~ W1sexYP_h,
      TRUE ~ -3
    )
  )

# Convert sex to factor with explicit labels for valid categories and missing value codes
data <- data %>%
  mutate(
    sex = factor(sex, 
                 levels = c(-9, -8, -1, -3, 1, 2), 
                 labels = c("Refusal", "Don't know", "Not applicable", "Not asked", "Male", "Female"))
  )

# Select final variables for output (ID and derived variables only)
output_data <- data %>%
  select(NSID, sex)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of observations:", nrow(output_data), "\n")
cat("Number of variables:", ncol(output_data), "\n")
cat("Variables:", paste(names(output_data), collapse = ", "), "\n")
cat("Sex distribution:\n")
print(table(output_data$sex, useNA = "ifany"))