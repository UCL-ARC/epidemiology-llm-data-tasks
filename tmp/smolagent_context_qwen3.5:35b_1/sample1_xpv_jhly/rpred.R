library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_young_person_2020.tab",
  "data/input/wave_three_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/wave_five_lsype_young_person_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_main_interview.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave_one <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
wave_two <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
wave_three <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
wave_four <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
wave_five <- read_delim(files[5], delim = "\t", show_col_types = FALSE)
wave_six <- read_delim(files[6], delim = "\t", show_col_types = FALSE)
wave_seven <- read_delim(files[7], delim = "\t", show_col_types = FALSE)
ns8 <- read_delim(files[8], delim = "\t", show_col_types = FALSE)
ns9 <- read_delim(files[9], delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(wave_one, wave_two, by = "NSID")
df <- full_join(df, wave_three, by = "NSID")
df <- full_join(df, wave_four, by = "NSID")
df <- full_join(df, wave_five, by = "NSID")
df <- full_join(df, wave_six, by = "NSID")
df <- full_join(df, wave_seven, by = "NSID")
df <- full_join(df, ns8, by = "NSID")
df <- full_join(df, ns9, by = "NSID")

# Function to standardize missing codes for a vector
standardize_missing <- function(x) {
  x <- as.numeric(x)
  # Map wave-specific missing codes to standard codes
  x <- case_when(
    # Refusal codes (-99, -92, -9)
    x %in% c(-99, -92, -9) ~ -9,
    # Don't know codes (-91, -1, -8)
    x %in% c(-91, -1, -8) ~ -8,
    # Not applicable codes (-91, -1)
    x %in% c(-91, -1) ~ -1,
    # Other wave-specific missing codes (-998, -997, -995)
    x %in% c(-998, -997, -995) ~ -3,
    # Null values
    is.na(x) ~ -3,
    # Keep valid values
    TRUE ~ x
  )
  return(x)
}

# Standardize sex variables across waves (after merge, all vectors have same length)
sex_w1 <- standardize_missing(df$W1sexYP)
sex_w2 <- standardize_missing(df$W2SexYP)
sex_w3 <- standardize_missing(df$W3sexYP)
sex_w4 <- standardize_missing(df$W4SexYP)
sex_w5 <- standardize_missing(df$W5SexYP)
sex_w6 <- standardize_missing(df$W6Sex)
sex_w7 <- standardize_missing(df$W7Sex)
sex_w8 <- standardize_missing(df$W8CMSEX)
sex_w9 <- standardize_missing(df$W9DSEX)

# Create time-invariant sex variable (prioritize most recent valid response)
df$sex <- case_when(
  sex_w9 %in% c(1, 2) ~ sex_w9,
  sex_w8 %in% c(1, 2) ~ sex_w8,
  sex_w7 %in% c(1, 2) ~ sex_w7,
  sex_w6 %in% c(1, 2) ~ sex_w6,
  sex_w5 %in% c(1, 2) ~ sex_w5,
  sex_w4 %in% c(1, 2) ~ sex_w4,
  sex_w3 %in% c(1, 2) ~ sex_w3,
  sex_w2 %in% c(1, 2) ~ sex_w2,
  sex_w1 %in% c(1, 2) ~ sex_w1,
  TRUE ~ NA_real_
)

# Convert to factor with labels
df$sex <- factor(df$sex, levels = c(1, 2, -9, -8, -1, -3),
                 labels = c("Male", "Female", "Refusal", "Don't know", "Not applicable", "Not asked"))

# Select only necessary variables for output
output_vars <- c("NSID", "sex")

# Create final output dataframe
cleaned_data <- df %>% select(all_of(output_vars))

# Write to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
