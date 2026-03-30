library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the four wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Function to recode missing values for waves 1 and 2 (continuous income)
recode_missing_w1w2 <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -999 ~ -2,
    x == -992 ~ -9,
    x == -99 ~ -3,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -3 ~ -1,  # Not yet paid -> Not applicable
    x == -1 ~ -8,
    TRUE ~ as.numeric(x)
  )
}

# Function to recode missing values for wave 3 (banded income)
recode_missing_w3 <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -99 ~ -3,
    x == -92 ~ -9,
    x == -1 ~ -8,
    TRUE ~ as.numeric(x)
  )
}

# Function to recode missing values for wave 4 (banded income)
recode_missing_w4 <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -996 ~ -3,  # No parent in household -> Not asked
    x == -99 ~ -3,
    x == -92 ~ -9,
    x == -1 ~ -8,
    TRUE ~ as.numeric(x)
  )
}

# Function to create income bands from continuous values
create_income_band <- function(x) {
  if (is.na(x) || x < 0) {
    return(x)
  } else if (x <= 49) {
    return(1)
  } else if (x <= 99) {
    return(2)
  } else if (x <= 199) {
    return(3)
  } else if (x <= 299) {
    return(4)
  } else if (x <= 399) {
    return(5)
  } else if (x <= 499) {
    return(6)
  } else if (x <= 599) {
    return(7)
  } else if (x <= 699) {
    return(8)
  } else if (x <= 799) {
    return(9)
  } else if (x <= 899) {
    return(10)
  } else if (x <= 999) {
    return(11)
  } else {
    return(12)
  }
}

# Process wave 1 (Age 14)
wave1_clean <- wave1 %>%
  select(NSID, W1GrsswkHH) %>%
  mutate(
    incwhhcnt14 = recode_missing_w1w2(W1GrsswkHH),
    incwhh14 = map_dbl(incwhhcnt14, create_income_band)
  ) %>%
  select(-W1GrsswkHH)

# Process wave 2 (Age 15)
wave2_clean <- wave2 %>%
  select(NSID, W2GrsswkHH) %>%
  mutate(
    incwhhcnt15 = recode_missing_w1w2(W2GrsswkHH),
    incwhh15 = map_dbl(incwhhcnt15, create_income_band)
  ) %>%
  select(-W2GrsswkHH)

# Process wave 3 (Age 16)
wave3_clean <- wave3 %>%
  select(NSID, W3incestw) %>%
  mutate(
    incwhh16 = recode_missing_w3(W3incestw)
  ) %>%
  select(-W3incestw)

# Process wave 4 (Age 17)
wave4_clean <- wave4 %>%
  select(NSID, w4IncEstW) %>%
  mutate(
    incwhh17 = recode_missing_w4(w4IncEstW)
  ) %>%
  select(-w4IncEstW)

# Merge all waves - fixed typo: wave2_clean instead of w2_clean
merged_data <- wave1_clean %>%
  full_join(wave2_clean, by = "NSID") %>%
  full_join(wave3_clean, by = "NSID") %>%
  full_join(wave4_clean, by = "NSID")

# Create factor levels for banded variables
income_band_levels <- c(-9, -8, -3, -2, -1, 1:12)
income_band_labels <- c("Refusal", "Don't know", "Not asked", "Script error", "Not applicable",
                        "Up to £49", "£50-£99", "£100-£199", "£200-£299", "£300-£399",
                        "£400-£499", "£500-£599", "£600-£699", "£700-£799", "£800-£899",
                        "£900-£999", "£1,000+")

# Convert banded variables to factors
merged_data <- merged_data %>%
  mutate(
    incwhh14 = factor(incwhh14, levels = income_band_levels, labels = income_band_labels),
    incwhh15 = factor(incwhh15, levels = income_band_levels, labels = income_band_labels)
  )

# For waves 3 and 4, the banding is slightly different (wave 3 has £900-£990 instead of £900-£999)
wave3_band_labels <- c("Refusal", "Don't know", "Not asked", "Not applicable",
                       "Up to £49", "£50-£99", "£100-£199", "£200-£299", "£300-£399",
                       "£400-£499", "£500-£599", "£600-£699", "£700-£799", "£800-£899",
                       "£900-£990", "£1,000+")

wave4_band_labels <- c("Refusal", "Don't know", "Not asked", "Not applicable",
                       "Up to £49", "£50-£99", "£100-£199", "£200-£299", "£300-£399",
                       "£400-£499", "£500-£599", "£600-£699", "£700-£799", "£800-£899",
                       "£900-£999", "£1,000+")

merged_data <- merged_data %>%
  mutate(
    incwhh16 = factor(incwhh16, levels = c(-9, -8, -3, -1, 1:12), labels = wave3_band_labels),
    incwhh17 = factor(incwhh17, levels = c(-9, -8, -3, -1, 1:12), labels = wave4_band_labels)
  )

# Select final output variables
final_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Ensure output directory exists
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Script completed successfully.\n")