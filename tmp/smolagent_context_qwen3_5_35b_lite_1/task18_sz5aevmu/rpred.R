library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8_self <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
w8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")
merged <- full_join(merged, w8_self, by = "NSID")
merged <- full_join(merged, w8_derived, by = "NSID")
merged <- full_join(merged, w9, by = "NSID")
merged <- full_join(merged, w9_derived, by = "NSID")

cat("Merged dataset dimensions:", nrow(merged), "rows,", ncol(merged), "columns\n")
cat("Number of NSIDs:", length(unique(merged$NSID)), "\n")

# Define GHQ-12 items for each wave
# Wave 2 (Age 15)
ghq_items_w2 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", 
                  "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", 
                  "W2wthlessYP", "W2happyYP")

# Wave 4 (Age 17)
ghq_items_w4 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", 
                  "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", 
                  "W4WthlessYP", "W4HappyYP")

# Wave 8 (Age 25)
ghq_items_w8 <- paste0("W8GHQ12_", 1:12)

# Wave 9 (Age 32)
ghq_items_w9 <- paste0("W9GHQ12_", 1:12)

# Function to standardize missing values for waves 2 and 4
standardize_missing_w2w4 <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,
    x == -99 ~ -3,
    x == -97 ~ -1,
    x == -96 ~ -3,
    x == -92 ~ -9,
    x == -1 ~ -8,
    is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Function to standardize missing values for waves 8 and 9
standardize_missing_w8w9 <- function(x) {
  case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Standardize missing values for all GHQ items
for (item in ghq_items_w2) {
  if (item %in% names(merged)) {
    merged[[item]] <- standardize_missing_w2w4(merged[[item]])
  }
}

for (item in ghq_items_w4) {
  if (item %in% names(merged)) {
    merged[[item]] <- standardize_missing_w2w4(merged[[item]])
  }
}

for (item in ghq_items_w8) {
  if (item %in% names(merged)) {
    merged[[item]] <- standardize_missing_w8w9(merged[[item]])
  }
}

for (item in ghq_items_w9) {
  if (item %in% names(merged)) {
    merged[[item]] <- standardize_missing_w8w9(merged[[item]])
  }
}

# Function to check if all values are non-negative
all_valid <- function(df, items) {
  valid <- TRUE
  for (item in items) {
    valid <- valid & all(df[[item]] >= 0, na.rm = TRUE)
  }
  return(valid)
}

# Check if all items are valid for each row
ghq_all_valid_15 <- apply(merged[ghq_items_w2], 1, function(x) all(x >= 0, na.rm = TRUE))
ghq_all_valid_17 <- apply(merged[ghq_items_w4], 1, function(x) all(x >= 0, na.rm = TRUE))
ghq_all_valid_25 <- apply(merged[ghq_items_w8], 1, function(x) all(x >= 0, na.rm = TRUE))
ghq_all_valid_32 <- apply(merged[ghq_items_w9], 1, function(x) all(x >= 0, na.rm = TRUE))

# Calculate Likert scores (sum of items, only if all valid)
merged$ghqtl15 <- rowSums(merged[ghq_items_w2], na.rm = TRUE)
merged$ghqtl17 <- rowSums(merged[ghq_items_w4], na.rm = TRUE)
merged$ghqtl25 <- rowSums(merged[ghq_items_w8], na.rm = TRUE)
merged$ghqtl32 <- rowSums(merged[ghq_items_w9], na.rm = TRUE)

# Set scores to -3 (not asked) if not all valid
merged$ghqtl15[!ghq_all_valid_15] <- -3
merged$ghqtl17[!ghq_all_valid_17] <- -3
merged$ghqtl25[!ghq_all_valid_25] <- -3
merged$ghqtl32[!ghq_all_valid_32] <- -3

cat("\nLikert score summary:\n")
print(summary(merged$ghqtl15))
print(summary(merged$ghqtl17))
print(summary(merged$ghqtl25))
print(summary(merged$ghqtl32))

# GHQ caseness: 0/3 scoring
# Pos items (1,3,4,6,7,9,11,12): 1,2=0; 3,4=1
# Neg items (2,5,8,10): 1,2=1; 3,4=0
ghq_pos_items <- c(1, 3, 4, 6, 7, 9, 11, 12)
ghq_neg_items <- c(2, 5, 8, 10)

# Function to compute GHQ caseness from Likert items for a single row
ghq_caseness_row <- function(items, pos_items, neg_items) {
  score <- 0
  for (i in pos_items) {
    val <- items[i]
    if (!is.na(val) && val >= 3 && val <= 4) score <- score + 1
  }
  for (i in neg_items) {
    val <- items[i]
    if (!is.na(val) && val >= 1 && val <= 2) score <- score + 1
  }
  return(score)
}

# Compute caseness for wave 2 (age 15)
merged$ghq15 <- apply(merged[ghq_items_w2], 1, function(x) ghq_caseness_row(x, ghq_pos_items, ghq_neg_items))

# Compute caseness for wave 4 (age 17)
merged$ghq17 <- apply(merged[ghq_items_w4], 1, function(x) ghq_caseness_row(x, ghq_pos_items, ghq_neg_items))

# Set caseness to -3 if not all valid
merged$ghq15[!ghq_all_valid_15] <- -3
merged$ghq17[!ghq_all_valid_17] <- -3

# Use pre-derived caseness scores for wave 8 (age 25) and wave 9 (age 32)
# Standardize W8DGHQSC and W9DGHQSC missing codes
merged$ghq25 <- case_when(
  merged$W8DGHQSC == -9 ~ -9,
  merged$W8DGHQSC == -8 ~ -8,
  merged$W8DGHQSC == -1 ~ -1,
  is.na(merged$W8DGHQSC) ~ -3,
  TRUE ~ merged$W8DGHQSC
)

merged$ghq32 <- case_when(
  merged$W9DGHQSC == -9 ~ -9,
  merged$W9DGHQSC == -8 ~ -8,
  merged$W9DGHQSC == -1 ~ -1,
  is.na(merged$W9DGHQSC) ~ -3,
  TRUE ~ merged$W9DGHQSC
)

# Clean up temporary variables
merged$ghq_all_valid_15 <- NULL
merged$ghq_all_valid_17 <- NULL
merged$ghq_all_valid_25 <- NULL
merged$ghq_all_valid_32 <- NULL

# Select only NSID and final variables
final_vars <- c("NSID", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32", "ghq15", "ghq17", "ghq25", "ghq32")
output <- merged[, final_vars]

cat("\nFinal output dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("Final variables:", paste(names(output), collapse = ", "), "\n")

# Write output
cat("\nWriting to data/output/cleaned_data.csv\n")
readr::write_csv(output, "data/output/cleaned_data.csv")

cat("\nDone!\n")

# Show sample
print(head(output))

# Show distribution of caseness scores
cat("\nCaspess score distribution:\n")
print(table(output$ghq15, useNA = "ifany"))
print(table(output$ghq17, useNA = "ifany"))
print(table(output$ghq25, useNA = "ifany"))
print(table(output$ghq32, useNA = "ifany"))
