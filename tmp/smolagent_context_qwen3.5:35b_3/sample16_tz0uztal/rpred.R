# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load each dataset
age14_data <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
age15_data <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
age16_data <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
age17_data <- read_delim(files[4], delim = "\t", show_col_types = FALSE)

# Function to harmonize missing codes
harmonize_missing <- function(x, mapping) {
  for (code in names(mapping)) {
    x[x == as.numeric(code)] <- as.numeric(mapping[[code]])
  }
  x[is.na(x)] <- -3
  return(x)
}

# Age 14 harmonization
age14_data <- age14_data %>%
  mutate(
    W1GrsswkHH = harmonize_missing(W1GrsswkHH, list(
      "-999" = -2,
      "-992" = -9,
      "-99" = -3,
      "-94" = -8,
      "-92" = -9,
      "-91" = -1,
      "-3" = -1,
      "-1" = -8
    ))
  )

# Age 15 harmonization
age15_data <- age15_data %>%
  mutate(
    W2GrsswkHH = harmonize_missing(W2GrsswkHH, list(
      "-999" = -2,
      "-992" = -9,
      "-99" = -3,
      "-94" = -8,
      "-92" = -9,
      "-91" = -1,
      "-3" = -1,
      "-1" = -8
    ))
  )

# Age 16 harmonization
age16_data <- age16_data %>%
  mutate(
    W3incestw = harmonize_missing(W3incestw, list(
      "-99" = -3,
      "-92" = -9,
      "-1" = -8
    ))
  )

# Age 17 harmonization
age17_data <- age17_data %>%
  mutate(
    w4IncEstW = harmonize_missing(w4IncEstW, list(
      "-996" = -3,
      "-99" = -3,
      "-92" = -9,
      "-1" = -8
    ))
  )

# Re-merge after harmonization
merged_data <- full_join(age14_data, age15_data, by = "NSID") %>%
  full_join(age16_data, by = "NSID") %>%
  full_join(age17_data, by = "NSID")

# Create continuous variables for ages 14 and 15
merged_data <- merged_data %>%
  mutate(
    incwhhcnt14 = W1GrsswkHH,
    incwhhcnt15 = W2GrsswkHH
  )

# Create banded variables for ages 14 and 15
create_bands_14_15 <- function(x) {
  case_when(
    x >= 0 & x < 50 ~ 1L,
    x >= 50 & x < 100 ~ 2L,
    x >= 100 & x < 200 ~ 3L,
    x >= 200 & x < 300 ~ 4L,
    x >= 300 & x < 400 ~ 5L,
    x >= 400 & x < 500 ~ 6L,
    x >= 500 & x < 600 ~ 7L,
    x >= 600 & x < 700 ~ 8L,
    x >= 700 & x < 800 ~ 9L,
    x >= 800 & x < 900 ~ 10L,
    x >= 900 & x < 1000 ~ 11L,
    x >= 1000 ~ 12L,
    TRUE ~ as.integer(x)
  )
}

merged_data <- merged_data %>%
  mutate(
    incwhh14 = create_bands_14_15(W1GrsswkHH),
    incwhh15 = create_bands_14_15(W2GrsswkHH),
    incwhh16 = W3incestw,
    incwhh17 = w4IncEstW
  )

# Convert any remaining NAs to -3 for all income variables
output_data <- merged_data %>%
  mutate(
    incwhh14 = ifelse(is.na(incwhh14), -3L, incwhh14),
    incwhh15 = ifelse(is.na(incwhh15), -3L, incwhh15),
    incwhhcnt14 = ifelse(is.na(incwhhcnt14), -3, incwhhcnt14),
    incwhhcnt15 = ifelse(is.na(incwhhcnt15), -3, incwhhcnt15),
    incwhh16 = ifelse(is.na(incwhh16), -3, incwhh16),
    incwhh17 = ifelse(is.na(incwhh17), -3, incwhh17)
  ) %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Output summary:\n")
cat("Number of rows:", nrow(output_data), "\n")
cat("Number of columns:", ncol(output_data), "\n")
cat("Variables:", names(output_data), "\n")

head(output_data)

# Check for any remaining NA values
cat("\nNA counts per column:\n")
colSums(is.na(output_data))
