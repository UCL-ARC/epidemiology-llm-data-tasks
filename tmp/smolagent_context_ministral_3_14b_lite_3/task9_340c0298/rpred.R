
# Load required libraries
library(haven)
library(dplyr)
library(readr)
library(labelled)

# Load data files
wave_one <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_df <- full_join(wave_one, wave_two, by = "NSID")
merged_df <- full_join(merged_df, wave_four, by = "NSID")

# Function to map missing values
map_missing <- function(x) {
  if (!is.numeric(x)) x <- as.numeric(x)
  x[x == -999] <- -2
  x[x == -99] <- -3
  x[x == -98] <- -1
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  return(x)
}

# Process mother's education using correct column names
mother_cols <- c("W1hiqualmum", "W2hiqualmum", "w4hiqualmum")
merged_df$educdtlma <- coalesce(
  map_missing(merged_df[[mother_cols[3]]]),
  map_missing(merged_df[[mother_cols[2]]]),
  map_missing(merged_df[[mother_cols[1]]]),
  -3
)

# Process father's education using correct column names
father_cols <- c("W1hiqualdad", "W2hiqualdad", "w4hiqualdad")
merged_df$educdtlpa <- coalesce(
  map_missing(merged_df[[father_cols[3]]]),
  map_missing(merged_df[[father_cols[2]]]),
  map_missing(merged_df[[father_cols[1]]]),
  -3
)

# Create NVQ5-level variables
create_nvq5 <- function(x) {
  if (!is.numeric(x)) x <- as.numeric(x)
  x[x == 1 | x == 2 | x == 3 | x == 4] <- 1
  x[x == 5 | x == 6 | x == 7 | x == 8 | x == 9 | x == 10 | x == 11 | x == 12 | x == 13 | x == 14] <- 2
  x[x == 15 | x == 16 | x == 17 | x == 18] <- 3
  x[x == 19 | x == 20] <- 4
  x[is.na(x)] <- -3
  return(x)
}

merged_df$educma <- create_nvq5(merged_df$educdtlma)
merged_df$educpa <- create_nvq5(merged_df$educdtlpa)

# Create labeled factors manually
educdtlma_levels <- c(-9, -8, -7, -3, -2, -1, 1:20)
educdtlma_labels <- c(
  "Refusal", "Don't know / insufficient information", "Prefer not to say",
  "Not asked / not interviewed", "Schedule not applicable", "Item not applicable",
  "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4",
  "Teaching qualification", "Nursing qualification", "A Levels", "OND/ONC",
  "City and Guilds part III, NVQ3", "CSYS", "Scottish Higher Grade", "AS Level",
  "Trade apprenticeship", "City and Guilds part II, NVQ2", "GCSE grade A-C and equivalent",
  "GCSE grade D-E and equivalent", "City and Guilds part I, NVQ1", "Youth training, skill seekers",
  "Qualification, level unspecified", "No qualification mentioned"
)

merged_df$educdtlma <- factor(
  merged_df$educdtlma,
  levels = educdtlma_levels,
  labels = educdtlma_labels
)

# NVQ levels
nvq_levels <- c(-3, 1, 2, 3, 4)
nvq_labels <- c(
  "Not asked / not interviewed", "NVQ5", "NVQ4", "NVQ3 or lower", "No NVQ/No qualification"
)

merged_df$educma <- factor(
  merged_df$educma,
  levels = nvq_levels,
  labels = nvq_labels
)

merged_df$educdtlpa <- factor(
  merged_df$educdtlpa,
  levels = educdtlma_levels,
  labels = educdtlma_labels
)

merged_df$educpa <- factor(
  merged_df$educpa,
  levels = nvq_levels,
  labels = nvq_labels
)

# Select final variables
final_df <- merged_df %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")
