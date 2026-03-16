library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(wave1, wave2, by = "NSID")
df <- full_join(df, wave3, by = "NSID")
df <- full_join(df, wave4, by = "NSID")

# Function to standardize missing codes
standardize_missing <- function(x) {
  x <- recode(x, 
              `-999` = -3,
              `-992` = -3,
              `-99` = -3,
              `-94` = -8,
              `-92` = -9,
              `-91` = -1,
              `-3` = -3,
              `-1` = -8,
              `-996` = -1,
              `-999.0` = -3,
              `-992.0` = -3,
              `-99.0` = -3,
              `-94.0` = -8,
              `-92.0` = -9,
              `-91.0` = -1,
              `-3.0` = -3,
              `-1.0` = -8,
              `-999` = -3,
              `-99.0` = -3,
              `-92.0` = -9,
              `-1.0` = -8,
              `-996.0` = -1,
              `-99.0` = -3,
              `-92.0` = -9,
              `-1.0` = -8
  )
  x[is.na(x)] <- -3
  return(x)
}

# Standardize missing codes for continuous income variables
df$W1GrsswkHH <- standardize_missing(df$W1GrsswkHH)
df$W2GrsswkHH <- standardize_missing(df$W2GrsswkHH)

# Create continuous income variables for ages 14 and 15
df$incwhhcnt14 <- df$W1GrsswkHH
df$incwhhcnt15 <- df$W2GrsswkHH

# Derive banded income for ages 14 and 15
df$incwhh14 <- df$incwhhcnt14
df$incwhh15 <- df$incwhhcnt15

# Apply banding logic only to valid positive values
band_income <- function(x) {
  result <- x
  valid <- x > 0
  result[valid] <- cut(x[valid], 
                       breaks = c(0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, Inf),
                       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                       include.lowest = TRUE)
  return(result)
}

df$incwhh14 <- band_income(df$incwhhcnt14)
df$incwhh15 <- band_income(df$incwhhcnt15)

# Convert to factors
df$incwhh14 <- as.factor(df$incwhh14)
df$incwhh15 <- as.factor(df$incwhh15)

# Standardize missing codes for categorical variables (ages 16 and 17)
df$W3incestw <- standardize_missing(df$W3incestw)
df$w4IncEstW <- standardize_missing(df$w4IncEstW)

# Create banded income variables for ages 16 and 17
df$incwhh16 <- df$W3incestw
df$incwhh17 <- df$w4IncEstW

# Convert to factors
df$incwhh16 <- as.factor(df$incwhh16)
df$incwhh17 <- as.factor(df$incwhh17)

# Select only required variables
output_df <- df %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
