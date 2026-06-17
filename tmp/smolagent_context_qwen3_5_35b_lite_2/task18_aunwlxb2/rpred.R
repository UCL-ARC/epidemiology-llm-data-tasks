library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave2 = "data/input/wave_two_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave8_self = "data/input/ns8_2015_self_completion.tab",
  wave8_derived = "data/input/ns8_2015_derived.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab",
  wave9_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Load each file
wave1 <- read_delim(files$wave1, delim = "\t", show_col_types = FALSE)
wave2 <- read_delim(files$wave2, delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files$wave4, delim = "\t", show_col_types = FALSE)
wave8_self <- read_delim(files$wave8_self, delim = "\t", show_col_types = FALSE)
wave8_derived <- read_delim(files$wave8_derived, delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(files$wave9, delim = "\t", show_col_types = FALSE)
wave9_derived <- read_delim(files$wave9_derived, delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")
merged <- full_join(merged, wave8_self, by = "NSID")
merged <- full_join(merged, wave8_derived, by = "NSID")
merged <- full_join(merged, wave9, by = "NSID")
merged <- full_join(merged, wave9_derived, by = "NSID")

# Define GHQ-12 item names for each wave
ghq_items_wave2 <- c(
  "W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP",
  "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP"
)

ghq_items_wave4 <- c(
  "W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP",
  "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP"
)

ghq_items_wave8 <- c(
  "W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6",
  "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12"
)

ghq_items_wave9 <- c(
  "W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6",
  "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12"
)

# Function to convert negative codes to standard missing values
convert_missing <- function(x) {
  x[x %in% c(-999, -998, -997, -995)] <- -2
  x[x == -99] <- -3
  x[x == -97] <- -1
  x[x == -96] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -3] <- -3
  x[x == -1] <- -1
  return(x)
}

# Function to calculate item-summed score (ghqtl)
sum_ghq_items_vec <- function(item_values) {
  if (any(is.na(item_values))) {
    return(NA)
  }
  if (any(item_values < 0)) {
    return(NA)
  }
  return(sum(item_values))
}

# Function to calculate GHQ caseness (0-12)
calc_caseness_vec <- function(item_values) {
  if (any(is.na(item_values))) {
    return(NA)
  }
  if (any(item_values < 0)) {
    return(NA)
  }
  
  scores <- numeric(12)
  for (i in 1:12) {
    val <- item_values[i]
    if (val <= 6) {
      scores[i] <- ifelse(val >= 3, 1, 0)
    } else {
      scores[i] <- ifelse(val <= 2, 1, 0)
    }
  }
  return(sum(scores))
}

# Function to process a wave and return ghqtl and ghq values using rowwise approach
process_wave_items_vec <- function(item_df) {
  # Convert each column
  converted_items <- lapply(item_df, convert_missing)
  
  # Convert to matrix (ensure numeric)
  mat <- as.matrix(sapply(converted_items, as.numeric))
  
  # Calculate ghqtl (item-summed score) for each row
  ghqtl <- apply(mat, 1, function(row) {
    sum_ghq_items_vec(row)
  })
  
  # Calculate ghq (caseness) for each row
  ghq <- apply(mat, 1, function(row) {
    calc_caseness_vec(row)
  })
  
  list(ghqtl = ghqtl, ghq = ghq)
}

# Process each wave
# Age 15 (wave 2)
wave2_vars <- merged[, ghq_items_wave2, drop = FALSE]
wave2_scores <- process_wave_items_vec(wave2_vars)

# Age 17 (wave 4)
wave4_vars <- merged[, ghq_items_wave4, drop = FALSE]
wave4_scores <- process_wave_items_vec(wave4_vars)

# Age 25 (wave 8)
wave8_vars <- merged[, ghq_items_wave8, drop = FALSE]
wave8_scores <- process_wave_items_vec(wave8_vars)

# Age 32 (wave 9)
wave9_vars <- merged[, ghq_items_wave9, drop = FALSE]
wave9_scores <- process_wave_items_vec(wave9_vars)

# Create final output with NSID and GHQ variables
output <- merged[, "NSID", drop = FALSE]
output$ghqtl15 <- wave2_scores$ghqtl
output$ghq15 <- wave2_scores$ghq
output$ghqtl17 <- wave4_scores$ghqtl
output$ghq17 <- wave4_scores$ghq
output$ghqtl25 <- wave8_scores$ghqtl
output$ghq25 <- wave8_scores$ghq
output$ghqtl32 <- wave9_scores$ghqtl
output$ghq32 <- wave9_scores$ghq

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Rows:", nrow(output), "\n")
cat("Columns:", ncol(output), "\n")
cat("\nVariable summary:\n")
str(output)
