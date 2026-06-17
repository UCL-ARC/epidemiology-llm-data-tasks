library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Set seed for reproducibility
set.seed(123)

# Define missing value mapping
map_missing <- function(x) {
  x[x %in% c(-999, -99, -98, -94, -92, -91, -1)] <- -3
  return(x)
}

# Map detailed 20-category variables
map_detailed <- function(x) {
  x <- map_missing(x)
  return(x)
}

# Map to 5-level NVQ
map_nvq <- function(x) {
  x <- map_missing(x)
  # Level 5: Higher Degree, First Degree, HE Diploma
  x[x %in% c(1, 2, 3)] <- 5
  # Level 4: NVQ4 (HNC/HND/NVQ4)
  x[x %in% 4] <- 4
  # Level 3: NVQ3 (City and guilds part III)
  x[x %in% 9] <- 3
  # Level 2: NVQ2 (City and guilds part II)
  x[x %in% 14] <- 2
  # Level 1: NVQ1 (City and guilds part I)
  x[x %in% 17] <- 1
  # Default: below NVQ or no qualification (5, 6, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 20)
  x[x %in% c(5, 6, 7, 8, 10, 11, 12, 13, 15, 16, 18, 19, 20)] <- 1
  return(x)
}

# Load the three waves
data_wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
data_wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
data_wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Create all derived variables for each wave
wave1_derived <- data_wave1 %>%
  mutate(
    educdtlma = map_detailed(W1hiqualmum),
    educdtlpa = map_detailed(W1hiqualdad),
    educma = map_nvq(W1hiqualmum),
    educpa = map_nvq(W1hiqualdad)
  )

wave2_derived <- data_wave2 %>%
  mutate(
    educdtlma = map_detailed(W2hiqualmum),
    educdtlpa = map_detailed(W2hiqualdad),
    educma = map_nvq(W2hiqualmum),
    educpa = map_nvq(W2hiqualdad)
  )

wave4_derived <- data_wave4 %>%
  mutate(
    educdtlma = map_detailed(w4hiqualmum),
    educdtlpa = map_detailed(w4hiqualdad),
    educma = map_nvq(w4hiqualmum),
    educpa = map_nvq(w4hiqualdad)
  )

# Merge all waves using full_join
final_data <- full_join(wave1_derived, wave2_derived, by = 'NSID') %>%
  full_join(wave4_derived, by = 'NSID')

# Create final consolidated variables using earliest-valid-first
final_data <- final_data %>%
  mutate(
    # Detailed mother education - 20 categories
    educdtlma = case_when(
      !is.na(W1hiqualmum) ~ W1hiqualmum,
      !is.na(W2hiqualmum) ~ W2hiqualmum,
      !is.na(w4hiqualmum) ~ w4hiqualmum,
      TRUE ~ -3
    ),
    # Detailed father education - 20 categories
    educdtlpa = case_when(
      !is.na(W1hiqualdad) ~ W1hiqualdad,
      !is.na(W2hiqualdad) ~ W2hiqualdad,
      !is.na(w4hiqualdad) ~ w4hiqualdad,
      TRUE ~ -3
    ),
    # 5-level NVQ mother education
    educma = case_when(
      !is.na(W1hiqualmum) ~ map_nvq(W1hiqualmum),
      !is.na(W2hiqualmum) ~ map_nvq(W2hiqualmum),
      !is.na(w4hiqualmum) ~ map_nvq(w4hiqualmum),
      TRUE ~ -3
    ),
    # 5-level NVQ father education
    educpa = case_when(
      !is.na(W1hiqualdad) ~ map_nvq(W1hiqualdad),
      !is.na(W2hiqualdad) ~ map_nvq(W2hiqualdad),
      !is.na(w4hiqualdad) ~ map_nvq(w4hiqualdad),
      TRUE ~ -3
    )
  ) %>%
  # Remove all source variables and intermediate variables, keeping only NSID and final derived variables
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Dataset written successfully. Variables:', paste(names(final_data), collapse=', '), '\n')
cat('Number of cases:', nrow(final_data), '\n')
