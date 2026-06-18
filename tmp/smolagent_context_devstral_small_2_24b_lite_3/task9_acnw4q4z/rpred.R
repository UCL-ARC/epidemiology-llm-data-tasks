library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to map missing values
map_missing <- function(x) {
  case_when(
    x %in% c(-999, -99, -98, -94) ~ -2,  # Schedule not applicable / script error / information lost
    x == -92 ~ -9,  # Refused
    x == -91 ~ -1,  # Not applicable
    x == -1 ~ -7,   # Don't know / Prefer not to say
    TRUE ~ x
  )
}

# Map missing values for all relevant variables
merged_data <- merged_data %>%
  mutate(
    W1hiqualmum = map_missing(W1hiqualmum),
    W1hiqualdad = map_missing(W1hiqualdad),
    W2hiqualmum = map_missing(W2hiqualmum),
    W2hiqualdad = map_missing(W2hiqualdad),
    w4hiqualmum = map_missing(w4hiqualmum),
    w4hiqualdad = map_missing(w4hiqualdad)
  )

# Derive detailed education variables
merged_data <- merged_data %>%
  mutate(
    educdtlma = coalesce(W1hiqualmum, W2hiqualmum, w4hiqualmum),
    educdtlpa = coalesce(W1hiqualdad, W2hiqualdad, w4hiqualdad)
  )

# Function to collapse NVQ categories
collapse_nvq <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4) ~ 1,  # Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4
    x %in% c(5, 6) ~ 2,          # Teaching qualification, Nursing qualification
    x %in% c(7, 8, 9, 10, 11, 12) ~ 3,  # A Levels, OND/ONC, etc.
    x %in% c(13, 14, 15, 16, 17) ~ 4,  # Trade apprenticeship, GCSEs, etc.
    x %in% c(18, 19, 20) ~ 5,    # Youth training, No qualification
    TRUE ~ x
  )
}

# Derive NVQ education variables
merged_data <- merged_data %>%
  mutate(
    educma = collapse_nvq(educdtlma),
    educpa = collapse_nvq(educdtlpa)
  )

# Select only final derived variables and NSID
final_data <- merged_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"