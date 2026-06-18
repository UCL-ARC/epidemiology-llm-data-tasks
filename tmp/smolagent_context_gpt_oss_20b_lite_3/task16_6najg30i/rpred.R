# Load required packages
library(readr)
library(dplyr)
library(labelled)

# Helper function to map missing values for wave 1 & 2
map_missing_w1w2 <- function(x) {
  # Vectorized mapping
  x[x == -999.0 | x == -992.0] <- -2
  x[x == -99.0] <- -3
  x[x == -94.0] <- -8
  x[x == -92.0] <- -9
  x[x == -91.0] <- -2
  x[x == -3.0] <- -3
  x[x == -1.0] <- -8
  return(x)
}

# Helper function to map missing values for wave 3 & 4
map_missing_w3w4 <- function(x) {
  x[x == -996.0] <- -2
  x[x == -99.0] <- -3
  x[x == -92.0] <- -9
  x[x == -1.0] <- -8
  return(x)
}

# Function to convert continuous income to banded categories
band_income <- function(x) {
  # Define missing codes that should be kept as is
  missing_codes <- c(-9, -8, -2, -3)
  res <- rep(NA_real_, length(x))
  # For valid numeric entries (not missing codes and not NA)
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val)) {
      res[i] <- NA_real_
    } else if (val %in% missing_codes) {
      res[i] <- val
    } else {
      # Bin according to thresholds
      if (val < 50) res[i] <- 1
      else if (val < 100) res[i] <- 2
      else if (val < 200) res[i] <- 3
      else if (val < 300) res[i] <- 4
      else if (val < 400) res[i] <- 5
      else if (val < 500) res[i] <- 6
      else if (val < 600) res[i] <- 7
      else if (val < 700) res[i] <- 8
      else if (val < 800) res[i] <- 9
      else if (val < 900) res[i] <- 10
      else if (val < 1000) res[i] <- 11
      else res[i] <- 12
    }
  }
  return(res)
}

# Load wave 1 data (Age 14)
wave1 <- read_delim(file.path("data/input/", "wave_one_lsype_family_background_2020.tab"),
                     delim = "\t", col_types = cols(.default = col_double(), NSID = col_character()))
# Map missing and create continuous and banded variables
wave1 <- wave1 %>%
  mutate(inc_cont14 = map_missing_w1w2(as.numeric(W1GrsswkHH)),
         incband14 = band_income(inc_cont14))

# Load wave 2 data (Age 15)
wave2 <- read_delim(file.path("data/input/", "wave_two_lsype_family_background_2020.tab"),
                     delim = "\t", col_types = cols(.default = col_double(), NSID = col_character()))
wave2 <- wave2 %>%
  mutate(inc_cont15 = map_missing_w1w2(as.numeric(W2GrsswkHH)),
         incband15 = band_income(inc_cont15))

# Load wave 3 data (Age 16)
wave3 <- read_delim(file.path("data/input/", "wave_three_lsype_family_background_2020.tab"),
                     delim = "\t", col_types = cols(.default = col_double(), NSID = col_character()))
wave3 <- wave3 %>%
  mutate(incband16 = map_missing_w3w4(as.numeric(W3incestw)))

# Load wave 4 data (Age 17)
wave4 <- read_delim(file.path("data/input/", "wave_four_lsype_family_background_2020.tab"),
                     delim = "\t", col_types = cols(.default = col_double(), NSID = col_character()))
wave4 <- wave4 %>%
  mutate(incband17 = map_missing_w3w4(as.numeric(w4IncEstW)))

# Merge all waves by NSID
merged_all <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Keep only required columns: NSID and derived income variables
final_df <- merged_all %>%
  select(NSID, inc_cont14, incband14, inc_cont15, incband15, incband16, incband17)

# Write to CSV
write_csv(final_df, file.path("data/output/", "cleaned_data.csv"))
