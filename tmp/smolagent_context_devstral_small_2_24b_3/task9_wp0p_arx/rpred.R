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

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define a function to harmonize missing values
harmonize_missing <- function(var) {
  case_when(
    var == -999 ~ -2,  # Missing - household data lost
    var == -99 ~ -3,   # Not interviewed
    var == -98 ~ -3,   # Not present
    var == -94 ~ -8,   # Insufficient information
    var == -92 ~ -9,   # Refused
    var == -91 ~ -1,   # Not applicable
    var == -1 ~ -7,    # Don't know
    TRUE ~ var
  )
}

# Apply harmonization to each wave's variables
merged_data <- merged_data %>%
  mutate(
    W1hiqualmum_harm = harmonize_missing(W1hiqualmum),
    W1hiqualdad_harm = harmonize_missing(W1hiqualdad),
    W2hiqualmum_harm = harmonize_missing(W2hiqualmum),
    W2hiqualdad_harm = harmonize_missing(W2hiqualdad),
    w4hiqualmum_harm = harmonize_missing(w4hiqualmum),
    w4hiqualdad_harm = harmonize_missing(w4hiqualdad)
  )

# Define a function to consolidate variables across waves
derive_consolidated <- function(var_w1, var_w2, var_w4) {
  # Find the first positive value across waves
  first_positive <- case_when(
    !is.na(var_w1) & var_w1 > 0 ~ var_w1,
    !is.na(var_w2) & var_w2 > 0 ~ var_w2,
    !is.na(var_w4) & var_w4 > 0 ~ var_w4,
    TRUE ~ NA_real_
  )

  # If no positive value, find the first negative code
  first_negative <- case_when(
    !is.na(var_w1) & var_w1 < 0 ~ var_w1,
    !is.na(var_w2) & var_w2 < 0 ~ var_w2,
    !is.na(var_w4) & var_w4 < 0 ~ var_w4,
    TRUE ~ NA_real_
  )

  # Combine: use first positive if available, else first negative, else -3
  coalesce(first_positive, first_negative, -3)
}

# Derive consolidated detailed variables
merged_data <- merged_data %>%
  mutate(
    educdtlma = derive_consolidated(W1hiqualmum_harm, W2hiqualmum_harm, w4hiqualmum_harm),
    educdtlpa = derive_consolidated(W1hiqualdad_harm, W2hiqualdad_harm, w4hiqualdad_harm)
  )

# Define the mapping for the collapsed NVQ scheme
nvq_mapping <- c(
  # NVQ 4–5
  "1.0" = 0, "2.0" = 0, "3.0" = 0, "4.0" = 0,
  # NVQ 1–3
  "5.0" = 1, "6.0" = 1, "7.0" = 1, "8.0" = 1, "9.0" = 1, "10.0" = 1, "11.0" = 1, "12.0" = 1, "13.0" = 1, "14.0" = 1, "15.0" = 1, "16.0" = 1, "17.0" = 1,
  # Youth training / skill seekers
  "18.0" = 2,
  # Qualification, level unspecified
  "19.0" = 3,
  # No qualification mentioned
  "20.0" = 4
)

# Derive collapsed NVQ variables
merged_data <- merged_data %>%
  mutate(
    educma = case_when(
      educdtlma %in% as.numeric(names(nvq_mapping)) ~ nvq_mapping[as.character(educdtlma)],
      educdtlma < 0 ~ educdtlma,  # Preserve missing codes
      TRUE ~ -3
    ),
    educpa = case_when(
      educdtlpa %in% as.numeric(names(nvq_mapping)) ~ nvq_mapping[as.character(educdtlpa)],
      educdtlpa < 0 ~ educdtlpa,  # Preserve missing codes
      TRUE ~ -3
    )
  )

# Select only the final derived variables and NSID
final_data <- merged_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write the output
write_csv(final_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"