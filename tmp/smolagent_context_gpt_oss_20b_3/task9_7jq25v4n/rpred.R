# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Helper function to harmonise missing codes for a vector
harmonise_missing <- function(x){
  # Map specific negative codes to standard scheme
  x <- recode(x,
    `-999` = -2,
    `-99`  = -3,
    `-98`  = -3,
    `-94`  = -8,
    `-92`  = -9,
    `-91`  = -1,
    `-1`   = -8  # for father "Don\'t know"
  )
  # Replace NA with standard missing code -3
  x[is.na(x)] <- -3
  return(x)
}

# File paths
file_wave1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file_wave2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file_wave4 <- "data/input/wave_four_lsype_family_background_2020.tab"

# Read files (tab delimited)
wave1 <- read_delim(file_wave1, delim = "\t", col_types = cols(), na = c(""))
wave2 <- read_delim(file_wave2, delim = "\t", col_types = cols(), na = c(""))
wave4 <- read_delim(file_wave4, delim = "\t", col_types = cols(), na = c(""))

# Harmonise missing codes for each relevant variable
wave1 <- wave1 %>%
  mutate(
    W1hiqualmum = harmonise_missing(W1hiqualmum),
    W1hiqualdad = harmonise_missing(W1hiqualdad)
  )

wave2 <- wave2 %>%
  mutate(
    W2hiqualmum = harmonise_missing(W2hiqualmum),
    W2hiqualdad = harmonise_missing(W2hiqualdad)
  )

wave4 <- wave4 %>%
  mutate(
    w4hiqualmum = harmonise_missing(w4hiqualmum),
    w4hiqualdad = harmonise_missing(w4hiqualdad)
  )

# Merge all waves by NSID (full join to keep full cohort)
merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Consolidate parental education across waves
merged_consolidated <- merged %>%
  mutate(
    # Mother's detailed education
    educdtlma = case_when(
      !is.na(W1hiqualmum) & W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ W1hiqualmum,
      !is.na(W2hiqualmum) & W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ W2hiqualmum,
      !is.na(w4hiqualmum) & w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ w4hiqualmum,
      !is.na(W1hiqualmum) & W1hiqualmum < 0 ~ W1hiqualmum,
      !is.na(W2hiqualmum) & W2hiqualmum < 0 ~ W2hiqualmum,
      !is.na(w4hiqualmum) & w4hiqualmum < 0 ~ w4hiqualmum,
      TRUE ~ -3
    ),
    # Father's detailed education
    educdtlpa = case_when(
      !is.na(W1hiqualdad) & W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ W1hiqualdad,
      !is.na(W2hiqualdad) & W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ W2hiqualdad,
      !is.na(w4hiqualdad) & w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ w4hiqualdad,
      !is.na(W1hiqualdad) & W1hiqualdad < 0 ~ W1hiqualdad,
      !is.na(W2hiqualdad) & W2hiqualdad < 0 ~ W2hiqualdad,
      !is.na(w4hiqualdad) & w4hiqualdad < 0 ~ w4hiqualdad,
      TRUE ~ -3
    )
  )

# Create collapsed NVQ variables from detailed ones
merged_final <- merged_consolidated %>%
  mutate(
    educma = case_when(
      educdtlma %in% 1:4  ~ 0,
      educdtlma %in% 5:17 ~ 1,
      educdtlma == 18    ~ 2,
      educdtlma == 19    ~ 3,
      educdtlma == 20    ~ 4,
      TRUE                ~ educdtlma
    ),
    educpa = case_when(
      educdtlpa %in% 1:4  ~ 0,
      educdtlpa %in% 5:17 ~ 1,
      educdtlpa == 18    ~ 2,
      educdtlpa == 19    ~ 3,
      educdtlpa == 20    ~ 4,
      TRUE                ~ educdtlpa
    )
  )

# Select only required columns
output_df <- merged_final %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Ensure output directory exists
if(!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)

# Write to CSV
output_path <- "data/output/cleaned_data.csv"
write_csv(output_df, output_path, na = "")

cat("Cleaning complete. Output written to", output_path, "\n")
