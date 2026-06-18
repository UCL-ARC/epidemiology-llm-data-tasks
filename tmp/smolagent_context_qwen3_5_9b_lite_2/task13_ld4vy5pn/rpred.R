library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all waves
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Function to collapse NS-SEC to major category (first digit)
nssec_major <- function(x) {
  if (is.na(x)) return(NA)
  if (x < 0) return(NA)  # Negative values are missing
  floor(x)
}

# Function to convert to standard missing codes
convert_missing <- function(x) {
  x <- replace(x, x == -999, -2)  # Missing - household data lost
  x <- replace(x, x == -99, -3)   # Mother/Father not interviewed
  x <- replace(x, x == -98, -1)   # Mother/Father not present
  x <- replace(x, x == -94, -8)   # Insufficient information
  x <- replace(x, is.na(x), -3)   # R NA to -3
  x
}

# Process each wave and create a data frame with NSID and NS-SEC variables
wave1_df <- wave1 %>%
  transmute(NSID, nssecma14 = convert_missing(nssec_major(W1nsseccatmum)), nssecpa14 = convert_missing(nssec_major(W1nsseccatdad)))

wave2_df <- wave2 %>%
  transmute(NSID, nssecma15 = convert_missing(nssec_major(W2nsseccatmum)), nssecpa15 = convert_missing(nssec_major(W2nsseccatdad)))

wave3_df <- wave3 %>%
  transmute(NSID, nssecma16 = convert_missing(nssec_major(W3cnsseccatmum)), nssecpa16 = convert_missing(nssec_major(W3cnsseccatdad)))

wave4_df <- wave4 %>%
  transmute(NSID, nssecma17 = convert_missing(nssec_major(w4cnsseccatmum)), nssecpa17 = convert_missing(nssec_major(w4cnsseccatdad)))

wave5_df <- wave5 %>%
  transmute(NSID, nssecma18 = convert_missing(nssec_major(w5Cnsseccatmum)), nssecpa18 = convert_missing(nssec_major(w5Cnsseccatdad)))

# Combine all waves with full_join by NSID
result <- full_join(wave1_df, wave2_df, by = "NSID")
result <- full_join(result, wave3_df, by = "NSID")
result <- full_join(result, wave4_df, by = "NSID")
result <- full_join(result, wave5_df, by = "NSID")

# Write to CSV
write_csv(result, "data/output/cleaned_data.csv")