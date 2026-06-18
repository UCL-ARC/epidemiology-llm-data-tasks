library(dplyr)
library(readr)
library(haven)
library(tidyr)
library(purrr)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Merge all files by NSID
full <- full_join(w1, w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

# Function to map missing values based on label meaning
map_missing <- function(x) {
  x[x == -999] <- -8
  x[x == -992] <- -8
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -3] <- -2
  x[x == -1] <- -8
  x[x == -996] <- -2
  x
}

# Function to create banded variable from continuous
create_band <- function(x) {
  bands <- numeric(length(x))
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val) || val < -10) {
      bands[i] <- NA
    } else if (val < 50) {
      bands[i] <- 1
    } else if (val < 100) {
      bands[i] <- 2
    } else if (val < 200) {
      bands[i] <- 3
    } else if (val < 300) {
      bands[i] <- 4
    } else if (val < 400) {
      bands[i] <- 5
    } else if (val < 500) {
      bands[i] <- 6
    } else if (val < 600) {
      bands[i] <- 7
    } else if (val < 700) {
      bands[i] <- 8
    } else if (val < 800) {
      bands[i] <- 9
    } else if (val < 900) {
      bands[i] <- 10
    } else if (val < 1000) {
      bands[i] <- 11
    } else {
      bands[i] <- 12
    }
  }
  bands
}

# Map missing values in the full dataframe
full$W1GrsswkHH <- map_missing(full$W1GrsswkHH)
full$W2GrsswkHH <- map_missing(full$W2GrsswkHH)
full$W3incestw <- map_missing(full$W3incestw)
full$w4IncEstW <- map_missing(full$w4IncEstW)

# Create banded variables for waves 1 and 2 from continuous
full$incband14 <- create_band(full$W1GrsswkHH)
full$incband15 <- create_band(full$W2GrsswkHH)

# Wave 3 and 4 are already banded
full$incband16 <- full$W3incestw
full$incband17 <- full$w4IncEstW

# Create final dataframe with NSID and derived variables
result <- full %>%
  select(NSID, inccont14 = W1GrsswkHH, incband14, inccont15 = W2GrsswkHH, incband15, incband16, incband17)

# Check summary
print(summary(result))

# Write to CSV
dir.create('data/output', showWarnings = FALSE)
write_csv(result, 'data/output/cleaned_data.csv')

cat('Successfully wrote cleaned_data.csv\n')
