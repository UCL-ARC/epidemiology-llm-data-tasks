# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all four wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Function to recode missing values to standard codes
# Standard codes: -9=Refusal, -8=Don't know/insufficient, -1=Not applicable, 
#                 -3=Not asked, -2=Schedule not applicable/info lost, -7=Prefer not to say
recode_missing_wave1_3 <- function(x) {
  case_when(
    x == -999 ~ -2,  # Missing household information - lost
    x == -99 ~ -1,   # Parent not interviewed
    x == -98 ~ -1,   # Parent not present
    x == -94 ~ -8,   # Insufficient information
    TRUE ~ as.numeric(x)
  )
}

recode_missing_wave4_mum <- function(x) {
  case_when(
    x == -999 ~ -2,  # Missing household information
    x == -99 ~ -1,   # Mother not interviewed
    x == -98 ~ -1,   # Mother not present
    x == -94 ~ -8,   # Insufficient information
    TRUE ~ as.numeric(x)
  )
}

recode_missing_wave4_dad <- function(x) {
  case_when(
    x == -999 ~ -2,  # Missing household information
    x == -996 ~ -1,  # No parent in household
    x == -99 ~ -1,   # Father not interviewed
    x == -98 ~ -1,   # Father not present
    x == -94 ~ -8,   # Insufficient information
    x == -92 ~ -9,   # Refusal
    TRUE ~ as.numeric(x)
  )
}

# Function to recode employment status to harmonized categories
# 1=Full-time work (30+ hrs), 2=Part-time work (<30 hrs), 3=Unemployed, 
# 4=Training, 5=Education, 6=Looking after home, 7=Retired, 8=Sick/disabled, 9=Other
recode_employment <- function(x) {
  case_when(
    x >= 1 & x <= 9 ~ x,  # Keep valid codes as-is (they already match our scheme)
    x < 0 ~ x,            # Keep missing codes
    TRUE ~ as.numeric(x)
  )
}

# Process Wave 1 (Age 14)
wave1_clean <- wave1 %>%
  select(NSID, W1empsmum, W1empsdad) %>%
  mutate(
    ecoactdtma14 = recode_employment(recode_missing_wave1_3(W1empsmum)),
    ecoactdtpa14 = recode_employment(recode_missing_wave1_3(W1empsdad))
  ) %>%
  select(NSID, ecoactdtma14, ecoactdtpa14)

# Process Wave 2 (Age 15)
wave2_clean <- wave2 %>%
  select(NSID, W2empsmum, W2empsdad) %>%
  mutate(
    ecoactdtma15 = recode_employment(recode_missing_wave1_3(W2empsmum)),
    ecoactdtpa15 = recode_employment(recode_missing_wave1_3(W2empsdad))
  ) %>%
  select(NSID, ecoactdtma15, ecoactdtpa15)

# Process Wave 3 (Age 16)
wave3_clean <- wave3 %>%
  select(NSID, W3empsmum, W3empsdad) %>%
  mutate(
    ecoactdtma16 = recode_employment(recode_missing_wave1_3(W3empsmum)),
    ecoactdtpa16 = recode_employment(recode_missing_wave1_3(W3empsdad))
  ) %>%
  select(NSID, ecoactdtma16, ecoactdtpa16)

# Process Wave 4 (Age 17)
wave4_clean <- wave4 %>%
  select(NSID, w4empsmum, w4empsdad) %>%
  mutate(
    ecoactdtma17 = recode_employment(recode_missing_wave4_mum(w4empsmum)),
    ecoactdtpa17 = recode_employment(recode_missing_wave4_dad(w4empsdad))
  ) %>%
  select(NSID, ecoactdtma17, ecoactdtpa17)

# Merge all waves using full_join by NSID
cleaned_data <- wave1_clean %>%
  full_join(wave2_clean, by = "NSID") %>%
  full_join(wave3_clean, by = "NSID") %>%
  full_join(wave4_clean, by = "NSID")

# Convert NULL values to -3 (Not asked at fieldwork stage)
cleaned_data <- cleaned_data %>%
  mutate(across(everything(), ~ifelse(is.na(.), -3, .)))

# Create labeled factors for parental economic activity variables
# Define value labels
emp_labels <- c(
  "1" = "Full-time work (30+ hrs)",
  "2" = "Part-time work (<30 hrs)",
  "3" = "Unemployed",
  "4" = "Training",
  "5" = "Education",
  "6" = "Looking after home",
  "7" = "Retired",
  "8" = "Sick/disabled",
  "9" = "Other",
  "-1" = "Not applicable",
  "-2" = "Information lost",
  "-3" = "Not asked",
  "-8" = "Don't know/insufficient",
  "-9" = "Refusal"
)

# Apply factor labels to all employment variables
for (var in names(cleaned_data)) {
  if (var != "NSID") {
    cleaned_data[[var]] <- factor(cleaned_data[[var]], 
                                   levels = c(-9, -8, -3, -2, -1, 1:9),
                                   labels = emp_labels[as.character(c(-9, -8, -3, -2, -1, 1:9))])
  }
}

# Write output CSV
write_csv(as.data.frame(cleaned_data), "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of observations:", nrow(cleaned_data), "\n")
cat("Number of variables:", ncol(cleaned_data), "\n")
cat("Variables:", paste(names(cleaned_data), collapse = ", "), "\n")