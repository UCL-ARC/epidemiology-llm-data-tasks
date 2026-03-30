# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets from data/input/
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Select only NSID and income variables
wave8_sel <- wave8 %>% select(NSID, W8DINCB)
wave9_sel <- wave9 %>% select(NSID, W9DINCB)

# Merge using full_join by NSID
merged_data <- full_join(wave8_sel, wave9_sel, by = "NSID")

# Rename variables to inc25 and inc32
merged_data <- merged_data %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

# Handle missing values - convert NA to -3 (Not asked/interviewed)
# This handles cases where respondents were absent from a wave
merged_data <- merged_data %>%
  mutate(
    inc25 = ifelse(is.na(inc25), -3, inc25),
    inc32 = ifelse(is.na(inc32), -3, inc32)
  )

# Define value labels for income bands and missing codes
income_labels <- c(
  "less than 25" = 1,
  "25 to 50" = 2,
  "50 to 90" = 3,
  "90 to 140" = 4,
  "140 to 240" = 5,
  "240 to 300" = 6,
  "300 to 350" = 7,
  "350 to 400" = 8,
  "400 to 500" = 9,
  "500 to 600" = 10,
  "600 to 700" = 11,
  "700 to 800" = 12,
  "800 to 900" = 13,
  "900 to 1200" = 14,
  "1200 to 1400" = 15,
  "more than 1400" = 16,
  "Item not applicable" = -1,
  "Script error/lost" = -2,
  "Not asked/interviewed" = -3,
  "Don't know/insufficient information" = -8,
  "Refusal" = -9
)

# Convert to labelled vectors
merged_data <- merged_data %>%
  mutate(
    inc25 = labelled(inc25, labels = income_labels),
    inc32 = labelled(inc32, labels = income_labels)
  )

# Select only required variables
output_data <- merged_data %>% select(NSID, inc25, inc32)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of rows:", nrow(output_data), "\n")
cat("Variables:", paste(names(output_data), collapse = ", "), "\n")