# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load each dataset
wave1 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
wave2 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(files[5], delim = "\t", show_col_types = FALSE)

# Merge all datasets using full_join by NSID
df <- full_join(wave1, wave2, by = "NSID")
df <- full_join(df, wave4, by = "NSID")
df <- full_join(df, wave8, by = "NSID")
df <- full_join(df, wave9, by = "NSID")

# Function to convert wave-specific missing codes to standard codes
convert_missing_codes <- function(x) {
  x <- case_when(
    x %in% c(-999, -998, -997, -995, -99) ~ -3,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ x
  )
  return(x)
}

# Convert missing codes for ethnicity variables
df$W1ethnic2YP <- convert_missing_codes(df$W1ethnic2YP)
df$W2ethnicYP <- convert_missing_codes(df$W2ethnicYP)
df$w4ethnic2YP <- convert_missing_codes(df$w4ethnic2YP)
df$W8DETHN15 <- convert_missing_codes(df$W8DETHN15)
df$W9DETHN15 <- convert_missing_codes(df$W9DETHN15)

# Create age-specific ethnicity variables with harmonized codes
df$eth14 <- df$W1ethnic2YP
df$eth15 <- df$W2ethnicYP
df$eth17 <- df$w4ethnic2YP
df$eth23 <- df$W8DETHN15
df$eth32 <- df$W9DETHN15

# For time-invariant ethnicity, prioritize earliest valid response
df$eth <- case_when(
  df$eth14 %in% 1:16 ~ df$eth14,
  df$eth15 %in% 1:16 ~ df$eth15,
  df$eth17 %in% 1:16 ~ df$eth17,
  df$eth23 %in% 1:16 ~ df$eth23,
  df$eth32 %in% 1:16 ~ df$eth32,
  TRUE ~ df$eth14  # fallback to earliest
)

# Select only the variables to output
output_df <- df %>%
  select(NSID, eth14, eth15, eth17, eth23, eth32, eth)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write output CSV
write_csv(output_df, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
