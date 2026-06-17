library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Set working directory and output path
output_file <- "data/output/cleaned_data.csv"

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all datasets
wave1_data <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2_data <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3_data <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4_data <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Function to convert missing values according to metadata
# For W1GrsswkHH and W2GrsswkHH (ages 14 and 15)
convert_w1_w2_missing <- function(data, var_name) {
  data <- data %>%
    mutate(!!var_name := case_when(
      !!as.symbol(var_name) == -992 ~ -9,  # Work status refused
      !!as.symbol(var_name) == -99 ~ -3,   # HH not interviewed
      !!as.symbol(var_name) == -94 ~ -8,   # Insufficient information
      !!as.symbol(var_name) == -91 ~ -1,   # Not applicable
      !!as.symbol(var_name) == -3 ~ -3,    # Not yet paid
      !!as.symbol(var_name) == -1 ~ -8,    # Don't know
      TRUE ~ !!as.symbol(var_name)
    ))
  return(data)
}

# For W3incestw (age 16)
convert_w3_missing <- function(data, var_name) {
  data <- data %>%
    mutate(!!var_name := case_when(
      !!as.symbol(var_name) == -99 ~ -3,   # HH not interviewed
      !!as.symbol(var_name) == -92 ~ -9,   # Refused
      !!as.symbol(var_name) == -91 ~ -1,   # Not applicable
      !!as.symbol(var_name) == -1 ~ -8,    # Don't know
      TRUE ~ !!as.symbol(var_name)
    ))
  return(data)
}

# For w4IncEstW (age 17)
convert_w4_missing <- function(data, var_name) {
  data <- data %>%
    mutate(!!var_name := case_when(
      !!as.symbol(var_name) == -99 ~ -3,   # MP not interviewed
      !!as.symbol(var_name) == -92 ~ -9,   # Refused
      !!as.symbol(var_name) == -1 ~ -8,    # Don't know
      TRUE ~ !!as.symbol(var_name)
    ))
  return(data)
}

# Apply missing value conversion
wave1_data <- convert_w1_w2_missing(wave1_data, "W1GrsswkHH")
wave2_data <- convert_w1_w2_missing(wave2_data, "W2GrsswkHH")
wave3_data <- convert_w3_missing(wave3_data, "W3incestw")
wave4_data <- convert_w4_missing(wave4_data, "w4IncEstW")

# Define banded income categories
band_income <- function(x) {
  case_when(
    x <= 49 ~ 1,
    x >= 50 & x <= 99 ~ 2,
    x >= 100 & x <= 199 ~ 3,
    x >= 200 & x <= 299 ~ 4,
    x >= 300 & x <= 399 ~ 5,
    x >= 400 & x <= 499 ~ 6,
    x >= 500 & x <= 599 ~ 7,
    x >= 600 & x <= 699 ~ 8,
    x >= 700 & x <= 799 ~ 9,
    x >= 800 & x <= 899 ~ 10,
    x >= 900 & x <= 990 ~ 11,
    x >= 991 & x <= 999 ~ 11,
    x >= 1000 ~ 12,
    TRUE ~ NA_real_  # missing values
  )
}

# Process continuous variables for ages 14 and 15
# Keep original continuous values
wave1_inc <- wave1_data %>%
  mutate(
    inc_cont14 = W1GrsswkHH,
    inc_14 = band_income(W1GrsswkHH)
  ) %>%
  select(NSID, inc_cont14, inc_14)

wave2_inc <- wave2_data %>%
  mutate(
    inc_cont15 = W2GrsswkHH,
    inc_15 = band_income(W2GrsswkHH)
  ) %>%
  select(NSID, inc_cont15, inc_15)

# Process banded variable for ages 16 and 17
wave3_inc <- wave3_data %>%
  mutate(inc_16 = band_income(W3incestw)) %>%
  select(NSID, inc_16)

wave4_inc <- wave4_data %>%
  mutate(inc_17 = band_income(w4IncEstW)) %>%
  select(NSID, inc_17)

# Full join on NSID - use explicit join to handle missing IDs
cleaned_data <- full_join(
  wave1_inc,
  full_join(wave2_inc, wave3_inc, by = "NSID"),
  by = "NSID"
) %>%
  full_join(wave4_inc, by = "NSID")

# Handle any remaining NA values in banded variables - convert to -3
cleaned_data <- cleaned_data %>%
  mutate(
    inc_16 = case_when(is.na(inc_16) ~ -3, TRUE ~ inc_16),
    inc_17 = case_when(is.na(inc_17) ~ -3, TRUE ~ inc_17)
  )

# Convert numeric NAs in continuous variables to -3 as well
cleaned_data <- cleaned_data %>%
  mutate(
    inc_cont14 = case_when(is.na(inc_cont14) ~ -3, TRUE ~ inc_cont14),
    inc_cont15 = case_when(is.na(inc_cont15) ~ -3, TRUE ~ inc_cont15)
  )

# Write output
write_csv(cleaned_data, output_file)

cat("Successfully wrote cleaned data to", output_file, "\n")
cat("Output shape:", nrow(cleaned_data), "rows,", ncol(cleaned_data), "columns\n")
'}}]`}]}]`</arg_value>