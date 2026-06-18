library(dplyr)
library(readr)
library(tidyr)
library(labelled)
library(haven)

# Load all input files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID")

# Define the banded income labels - names are label text, values are codes
income_bands <- c(
  "Up to \u00a349" = 1,
  "\u00a350 up to \u00a399" = 2,
  "\u00a3100 up to \u00a3199" = 3,
  "\u00a3200 up to \u00a3299" = 4,
  "\u00a3300 up to \u00a3399" = 5,
  "\u00a3400 up to \u00a3499" = 6,
  "\u00a3500 up to \u00a3599" = 7,
  "\u00a3600 up to \u00a3699" = 8,
  "\u00a3700 up to \u00a3799" = 9,
  "\u00a3800 up to \u00a3899" = 10,
  "\u00a3900 up to \u00a3999" = 11,
  "\u00a31,000 or more" = 12
)

# Function to map missing values for age 14 and 15 (continuous source variables)
map_missing_14_15 <- function(x) {
  x[x == -3] <- -1       # "Not yet paid" -> -1
  x[x == -1] <- -8       # "Don't know" -> -8
  x[x == -992] <- -9     # "No information - work status questions refused" -> -9
  x[x == -999] <- -2     # "Missing in error" -> -2
  x[x == -94] <- -8      # "Insufficient information" -> -8
  x[x == -92] <- -9      # "Refused" -> -9
  x[x == -91] <- -1      # "Not applicable" -> -1
  x[x == -99] <- -3      # "HH not interviewed" -> -3
  x[is.na(x)] <- -3
  return(x)
}

# Function to map missing values for age 16 (W3incestw - banded source)
map_missing_16 <- function(x) {
  x[x == -92] <- -9      # "Refused" -> -9
  x[x == -1] <- -8       # "Don't know" -> -8
  x[x == -99] <- -3      # "MP not interviewed" -> -3
  x[is.na(x)] <- -3
  return(x)
}

# Function to map missing values for age 17 (w4IncEstW - banded source)
map_missing_17 <- function(x) {
  x[x == -996] <- -3     # "No parent in household" -> -3
  x[x == -99] <- -3      # "MP not interviewed" -> -3
  x[x == -92] <- -9      # "Refused" -> -9
  x[x == -1] <- -8       # "Don't know" -> -8
  x[is.na(x)] <- -3
  return(x)
}

# Function to band continuous values into categories
band_income <- function(x) {
  result <- rep(-3, length(x))  # default to -3 for all
  valid_mask <- x > 0
  result[valid_mask] <- as.integer(x[valid_mask])
  return(result)
}

# Process Age 14 (W1GrsswkHH - continuous)
df$incwhhcnt14 <- map_missing_14_15(df$W1GrsswkHH)
df$incwhh14 <- band_income(df$incwhhcnt14)

# Process Age 15 (W2GrsswkHH - continuous)
df$incwhhcnt15 <- map_missing_14_15(df$W2GrsswkHH)
df$incwhh15 <- band_income(df$incwhhcnt15)

# Process Age 16 (W3incestw - already banded)
df$incwhh16 <- map_missing_16(df$W3incestw)

# Process Age 17 (w4IncEstW - already banded)
df$incwhh17 <- map_missing_17(df$w4IncEstW)

# Set value labels using haven::labelled with correct format (names=labels, values=codes)
for (var_name in c("incwhh14", "incwhh15", "incwhh16", "incwhh17")) {
  df[[var_name]] <- haven::labelled(df[[var_name]], labels = income_bands)
}

# Convert to labelled factors
df$incwhh14 <- haven::as_factor(df$incwhh14)
df$incwhh15 <- haven::as_factor(df$incwhh15)
df$incwhh16 <- haven::as_factor(df$incwhh16)
df$incwhh17 <- haven::as_factor(df$incwhh17)

# Select only NSID and derived variables
output_df <- df %>%
  select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

# Write output
write_csv(output_df, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(output_df), "\n")
cat("Columns:", paste(names(output_df), collapse = ", "), "\n")

# Quick summary
summary(output_df)
