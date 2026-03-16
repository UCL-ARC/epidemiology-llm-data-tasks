library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define input and output paths
input_dir <- "data/input"
output_dir <- "data/output"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define file paths
files <- c(
  "wave_one_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab"
)

file_paths <- paste0(input_dir, "/", files)

# Load each file with character columns to preserve values
wave14 <- read_delim(file_paths[1], delim = "\t", col_types = cols(.default = "c"))
wave15 <- read_delim(file_paths[2], delim = "\t", col_types = cols(.default = "c"))
wave16 <- read_delim(file_paths[3], delim = "\t", col_types = cols(.default = "c"))
wave17 <- read_delim(file_paths[4], delim = "\t", col_types = cols(.default = "c"))

print("Files loaded successfully")

# Function to map missing codes for age 14-15
map_missing_14_15 <- function(x) {
  x <- as.integer(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -9
  x[x == -992] <- -9
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -3] <- -3
  x[x == -1] <- -8
  return(x)
}

# Function to map missing codes for age 16-17
map_missing_16_17 <- function(x) {
  x <- as.integer(x)
  x[is.na(x)] <- -3
  x[x == -996] <- -1
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -1] <- -8
  return(x)
}

# Banding labels
banding_labels <- c(
  "Refusal" = "Refusal",
  "Don't know/insufficient" = "Don't know/insufficient information",
  "Not applicable" = "Not applicable",
  "Not asked" = "Not asked/interviewed",
  "Script error" = "Script error/lost",
  "1" = "Up to \u00A349",
  "2" = "\u00A350 up to \u00A399",
  "3" = "\u00A3100 up to \u00A3199",
  "4" = "\u00A3200 up to \u00A3299",
  "5" = "\u00A3300 up to \u00A3399",
  "6" = "\u00A3400 up to \u00A3499",
  "7" = "\u00A3500 up to \u00A3599",
  "8" = "\u00A3600 up to \u00A3699",
  "9" = "\u00A3700 up to \u00A3799",
  "10" = "\u00A3800 up to \u00A3899",
  "11" = "\u00A3900 up to \u00A3999",
  "12" = "\u00A31,000 or more"
)

# Process Age 14 - create both banded and continuous versions
wave14$incwhh14 <- map_missing_14_15(wave14$W1GrsswkHH)
wave14$incwhh14 <- factor(wave14$incwhh14, 
  levels = c(-9, -8, -1, -3, -2, 1:12),
  labels = banding_labels
)

# For continuous version, we need numeric with missing codes
wave14$incwhhcnt14 <- map_missing_14_15(wave14$W1GrsswkHH)

# Process Age 15
wave15$incwhh15 <- map_missing_14_15(wave15$W2GrsswkHH)
wave15$incwhh15 <- factor(wave15$incwhh15, 
  levels = c(-9, -8, -1, -3, -2, 1:12),
  labels = banding_labels
)

# Continuous version
wave15$incwhhcnt15 <- map_missing_14_15(wave15$W2GrsswkHH)

# Process Age 16
wave16$incwhh16 <- map_missing_16_17(wave16$W3incestw)
wave16$incwhh16 <- factor(wave16$incwhh16, 
  levels = c(-9, -8, -1, -3, -2, 1:12),
  labels = banding_labels
)

# Process Age 17
wave17$incwhh17 <- map_missing_16_17(wave17$w4IncEstW)
wave17$incwhh17 <- factor(wave17$incwhh17, 
  levels = c(-9, -8, -1, -3, -2, 1:12),
  labels = banding_labels
)

# For continuous versions of 16 and 17, we also create them
wave16$incwhhcnt16 <- map_missing_16_17(wave16$W3incestw)
wave17$incwhhcnt17 <- map_missing_16_17(wave17$w4IncEstW)

print("Missing value mapping complete")

# Merge all datasets by NSID, but first select only needed columns from each
# We need: NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17
df <- wave14 %>%
  select(NSID, incwhh14, incwhhcnt14)

df <- df %>%
  full_join(wave15 %>% select(NSID, incwhh15, incwhhcnt15), by = "NSID")

df <- df %>%
  full_join(wave16 %>% select(NSID, incwhh16, incwhhcnt16), by = "NSID")

df <- df %>%
  full_join(wave17 %>% select(NSID, incwhh17, incwhhcnt17), by = "NSID")

# Keep only required variables - we don't need incwhhcnt16 and incwhhcnt17 per output requirements
df <- df %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

print(paste("Total cases:", nrow(df)))
print(paste("Variables:", paste(names(df), collapse = ", ")))

# Write to CSV
write_csv(df, paste0(output_dir, "/cleaned_data.csv"))

print("Output written successfully!")
print(paste("Output file:", paste0(output_dir, "/cleaned_data.csv")))
