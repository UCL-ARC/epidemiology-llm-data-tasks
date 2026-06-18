
# Load required libraries
library(dplyr)
library(readr)

# Load data files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge datasets
merged <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Function to map missing values (vectorized)
map_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3

  mapping <- list(
    '-999' = -3, '-99' = -3, '-98' = -2,
    '-94' = -8, '-92' = -9, '-91' = -1, '-1' = -8
  )

  for (code in names(mapping)) {
    x[x == as.numeric(code)] <- mapping[[code]]
  }
  return(x)
}

# Function to convert detailed education to NVQ levels (vectorized)
get_nvq_level <- function(x) {
  x <- as.numeric(x)
  x[x == 1 | x == 2] <- 1    # Higher Degree, First Degree
  x[x %in% c(3, 4, 5, 6)] <- 2  # HE Diploma, HNC/HND/NVQ4, Teaching/Nursing
  x[x %in% c(7, 8, 9, 10, 11)] <- 3  # A Levels, OND/ONC, NVQ3, CSYS, Scottish Higher
  x[x %in% c(12, 13, 14)] <- 4  # AS Level, Trade apprenticeship, NVQ2
  x[x %in% c(15:20)] <- 5  # Lower qualifications
  x[!x %in% 1:5] <- -3  # Set all others to missing
  return(x)
}

# Create education variables using vectorized operations
merged <- merged %>%
  mutate(
    educdtlma = coalesce(
      map_missing(w4hiqualmum),
      map_missing(W2hiqualmum),
      map_missing(W1hiqualmum)
    ),
    educdtlpa = coalesce(
      map_missing(w4hiqualdad),
      map_missing(W2hiqualdad),
      map_missing(W1hiqualdad)
    ),
    educma = get_nvq_level(coalesce(
      map_missing(w4hiqualmum),
      map_missing(W2hiqualmum),
      map_missing(W1hiqualmum)
    )),
    educpa = get_nvq_level(coalesce(
      map_missing(w4hiqualdad),
      map_missing(W2hiqualdad),
      map_missing(W1hiqualdad)
    ))
  )

# Select final variables
final_data <- merged %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')
