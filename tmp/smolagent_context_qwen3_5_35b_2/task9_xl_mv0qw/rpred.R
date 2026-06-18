library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the data files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
all_data <- full_join(w1, w2, by = "NSID")
all_data <- full_join(all_data, w4, by = "NSID")

# Standardize missing value codes based on label meaning
standardize_missing <- function(x) {
  case_when(
    x == -999 ~ -2,    # Missing - household data lost
    x == -99 ~ -3,     # Not interviewed
    x == -98 ~ -2,     # Not present
    x == -94 ~ -8,     # Insufficient information
    x == -92 ~ -9,     # Refused
    x == -91 ~ -1,     # Not applicable
    x == -1 ~ -8,      # Don't know (only in W1/W2 for father)
    TRUE ~ as.numeric(x)
  )
}

# Map detailed codes to collapsed 5-level NVQ scheme
map_to_nvq5 <- function(x) {
  case_when(
    x >= 1 & x <= 4 ~ 0,       # NVQ 4-5: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4
    x >= 5 & x <= 17 ~ 1,      # NVQ 1-3: teaching/nursing through City & Guilds Part I / NVQ1
    x == 18 ~ 2,               # Youth training / skill seekers
    x == 19 ~ 3,               # Qualification, level unspecified
    x == 20 ~ 4,               # No qualification mentioned
    is.na(x) ~ NA_real_,
    TRUE ~ x                   # Preserve other values (missing codes)
  )
}

# Derive detailed education variables for mother and father
# Priority: first positive value (1-20), then first negative code, then -3 if no data
all_data <- all_data %>%
  mutate(
    educdtlma = case_when(
      !is.na(W1hiqualmum) & W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ W1hiqualmum,
      !is.na(W2hiqualmum) & W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ W2hiqualmum,
      !is.na(w4hiqualmum) & w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ w4hiqualmum,
      !is.na(W1hiqualmum) ~ standardize_missing(W1hiqualmum),
      !is.na(W2hiqualmum) ~ standardize_missing(W2hiqualmum),
      !is.na(w4hiqualmum) ~ standardize_missing(w4hiqualmum),
      TRUE ~ -3
    ),
    educdtlpa = case_when(
      !is.na(W1hiqualdad) & W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ W1hiqualdad,
      !is.na(W2hiqualdad) & W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ W2hiqualdad,
      !is.na(w4hiqualdad) & w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ w4hiqualdad,
      !is.na(W1hiqualdad) ~ standardize_missing(W1hiqualdad),
      !is.na(W2hiqualdad) ~ standardize_missing(W2hiqualdad),
      !is.na(w4hiqualdad) ~ standardize_missing(w4hiqualdad),
      TRUE ~ -3
    )
  )

# Derive collapsed NVQ variables from consolidated detailed variables
all_data <- all_data %>%
  mutate(
    educma = map_to_nvq5(educdtlma),
    educpa = map_to_nvq5(educdtlpa)
  )

# Select only final derived variables
final_data <- all_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write output CSV
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
