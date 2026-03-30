# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the data files from input directory
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge the datasets by NSID using full_join
merged <- full_join(wave8, wave9, by = "NSID")

# Select only NSID and income variables (W8DINCB and W9DINCB)
selected <- merged %>%
  select(NSID, W8DINCB, W9DINCB)

# Rename income variables to inc25 and inc32
selected <- selected %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

# Define standard missing codes
standard_missing <- c(-9, -8, -1, -3, -2)

# Function to recode missing values according to standard scheme
recode_missing <- function(x) {
  # Replace NA with -3 (not asked/interviewed)
  x[is.na(x)] <- -3
  return(x)
}

# Apply recoding to income variables
selected$inc25 <- recode_missing(selected$inc25)
selected$inc32 <- recode_missing(selected$inc32)

# Create value labels using named numeric vector approach
# Create a named vector where names are the labels and values are the codes
inc_labels <- c(
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
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not applicable" = -1,
  "Not asked/interviewed" = -3,
  "Script error/lost" = -2
)

# Apply value labels to income variables using labelled::set_value_labels
# The function expects a named list or named vector
selected$inc25 <- labelled::set_value_labels(selected$inc25, inc_labels)
selected$inc32 <- labelled::set_value_labels(selected$inc32, inc_labels)

# Convert to labelled factors
selected$inc25 <- as_factor(selected$inc25)
selected$inc32 <- as_factor(selected$inc32)

# Write output CSV file
csv_path <- "data/output/cleaned_data.csv"
write_csv(selected, csv_path)

cat("Cleaned data written to:", csv_path, "\n")
cat("Number of rows:", nrow(selected), "\n")
cat("Number of columns:", ncol(selected), "\n")