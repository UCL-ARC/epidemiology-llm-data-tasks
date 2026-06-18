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

# Merge datasets using full_join
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Harmonize missing values for mother's education variables
harmonize_missing <- function(var) {
  case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refused
    var %in% c(-94, -94.0) ~ -8,  # Don't know / insufficient information
    var %in% c(-91, -91.0) ~ -1,  # Not applicable
    var %in% c(-99, -99.0) ~ -3,  # Not interviewed
    var %in% c(-98, -98.0) ~ -2,  # Not present
    var %in% c(-999, -999.0) ~ -2,  # Missing - household data lost
    var %in% c(-1, -1.0) ~ -7,  # Prefer not to say
    TRUE ~ var
  )
}

# Apply harmonization to each wave's mother and father education variables
merged_data <- merged_data %>%
  mutate(
    W1hiqualmum_harm = harmonize_missing(W1hiqualmum),
    W1hiqualdad_harm = harmonize_missing(W1hiqualdad),
    W2hiqualmum_harm = harmonize_missing(W2hiqualmum),
    W2hiqualdad_harm = harmonize_missing(W2hiqualdad),
    w4hiqualmum_harm = harmonize_missing(w4hiqualmum),
    w4hiqualdad_harm = harmonize_missing(w4hiqualdad)
  )

# Function to consolidate education variables
consolidate_education <- function(w1_var, w2_var, w4_var) {
  # Find the first positive value across waves 1, 2, and 4
  first_positive <- pmap_dbl(
    list(w1_var, w2_var, w4_var),
    function(w1, w2, w4) {
      if (!is.na(w1) && w1 > 0) return(w1)
      if (!is.na(w2) && w2 > 0) return(w2)
      if (!is.na(w4) && w4 > 0) return(w4)
      # If no positive value, find the first negative code
      if (!is.na(w1) && w1 < 0) return(w1)
      if (!is.na(w2) && w2 < 0) return(w2)
      if (!is.na(w4) && w4 < 0) return(w4)
      return(-3)  # No value at all
    }
  )
  return(first_positive)
}

# Consolidate mother's and father's education
merged_data <- merged_data %>%
  mutate(
    educdtlma = consolidate_education(W1hiqualmum_harm, W2hiqualmum_harm, w4hiqualmum_harm),
    educdtlpa = consolidate_education(W1hiqualdad_harm, W2hiqualdad_harm, w4hiqualdad_harm)
  )

# Map detailed education variables to collapsed NVQ scheme
map_to_nvq <- function(detailed_var) {
  case_when(
    detailed_var %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4–5
    detailed_var %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14) ~ 1,  # NVQ 1–3
    detailed_var %in% c(18) ~ 2,  # Youth training
    detailed_var %in% c(19) ~ 3,  # Qualification, level unspecified
    detailed_var %in% c(20) ~ 4,  # No qualification
    TRUE ~ NA_real_  # Missing values remain as is
  )
}

# Create collapsed NVQ variables
merged_data <- merged_data %>%
  mutate(
    educma = map_to_nvq(educdtlma),
    educpa = map_to_nvq(educdtlpa)
  )

# Select only the final derived variables and NSID
final_data <- merged_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write the output
write_csv(final_data, "data/output/cleaned_data.csv")
