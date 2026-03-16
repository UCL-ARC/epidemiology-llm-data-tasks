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
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load each file
w1 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
w2 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
w4 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
merged <- full_join(w1, w2, by = "NSID")
merged <- full_join(merged, w4, by = "NSID")

# Consolidate maternal education (educdtlma)
# Priority: W2 > W1 > W4
# First try to find positive values, then fall back to negative codes
w2_pos <- !is.na(merged$W2hiqualmum) & merged$W2hiqualmum > 0
w1_pos <- !is.na(merged$W1hiqualmum) & merged$W1hiqualmum > 0
w4_pos <- !is.na(merged$w4hiqualmum) & merged$w4hiqualmum > 0
w2_neg <- !is.na(merged$W2hiqualmum) & merged$W2hiqualmum <= 0
w1_neg <- !is.na(merged$W1hiqualmum) & merged$W1hiqualmum <= 0
w4_neg <- !is.na(merged$w4hiqualmum) & merged$w4hiqualmum <= 0

merged$educdtlma <- case_when(
  w2_pos ~ merged$W2hiqualmum,
  w1_pos ~ merged$W1hiqualmum,
  w4_pos ~ merged$w4hiqualmum,
  w2_neg ~ merged$W2hiqualmum,
  w1_neg ~ merged$W1hiqualmum,
  w4_neg ~ merged$w4hiqualmum,
  TRUE ~ NA_real_
)

# Consolidate paternal education (educdtlpa)
w2_pos <- !is.na(merged$W2hiqualdad) & merged$W2hiqualdad > 0
w1_pos <- !is.na(merged$W1hiqualdad) & merged$W1hiqualdad > 0
w4_pos <- !is.na(merged$w4hiqualdad) & merged$w4hiqualdad > 0
w2_neg <- !is.na(merged$W2hiqualdad) & merged$W2hiqualdad <= 0
w1_neg <- !is.na(merged$W1hiqualdad) & merged$W1hiqualdad <= 0
w4_neg <- !is.na(merged$w4hiqualdad) & merged$w4hiqualdad <= 0

merged$educdtlpa <- case_when(
  w2_pos ~ merged$W2hiqualdad,
  w1_pos ~ merged$W1hiqualdad,
  w4_pos ~ merged$w4hiqualdad,
  w2_neg ~ merged$W2hiqualdad,
  w1_neg ~ merged$W1hiqualdad,
  w4_neg ~ merged$w4hiqualdad,
  TRUE ~ NA_real_
)

# Create collapsed maternal education (educma)
# Map detailed codes to 5-level NVQ scheme
merged$educma <- case_when(
  merged$educdtlma %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4-5
  merged$educdtlma %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,  # NVQ 1-3
  merged$educdtlma == 18 ~ 2,  # Youth training
  merged$educdtlma == 19 ~ 3,  # Level unspecified
  merged$educdtlma == 20 ~ 4,  # No qualification
  merged$educdtlma < 0 ~ merged$educdtlma,  # Preserve negative codes
  TRUE ~ NA_real_
)

# Create collapsed paternal education (educpa)
merged$educpa <- case_when(
  merged$educdtlpa %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4-5
  merged$educdtlpa %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,  # NVQ 1-3
  merged$educdtlpa == 18 ~ 2,  # Youth training
  merged$educdtlpa == 19 ~ 3,  # Level unspecified
  merged$educdtlpa == 20 ~ 4,  # No qualification
  merged$educdtlpa < 0 ~ merged$educdtlpa,  # Preserve negative codes
  TRUE ~ NA_real_
)

# Harmonize missing codes to standard scheme
harmonize_missing <- function(x) {
  # Map various missing codes to standard scheme
  x <- ifelse(x == -999, -9, x)
  x <- ifelse(x == -99, -9, x)
  x <- ifelse(x == -98, -9, x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(x == -91, -1, x)
  x <- ifelse(x == -1, -8, x)
  x <- ifelse(is.na(x), -3, x)  # NA -> -3 (Not asked)
  return(x)
}

# Apply harmonization to consolidated detailed variables
merged$educdtlma <- harmonize_missing(merged$educdtlma)
merged$educdtlpa <- harmonize_missing(merged$educdtlpa)

# Apply harmonization to collapsed variables
merged$educma <- harmonize_missing(merged$educma)
merged$educpa <- harmonize_missing(merged$educpa)

# Select only required variables
result <- merged %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Create value labels for detailed variables using character keys
detailed_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "1" = "Higher Degree",
  "2" = "First Degree",
  "3" = "HE Diploma",
  "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree",
  "6" = "Nursing qualification, non-degree",
  "7" = "A Levels",
  "8" = "OND/ONC",
  "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS",
  "11" = "Scottish Higher Grade",
  "12" = "AS Level",
  "13" = "Trade apprenticeship",
  "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent",
  "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified",
  "20" = "No qualification mentioned"
)

# Create value labels for collapsed variables
collapsed_labels <- c(
  "0" = "NVQ 4-5: degree-level qualifications and above",
  "1" = "NVQ 1-3: sub-degree qualifications",
  "2" = "None/entry: training programmes below NVQ level",
  "3" = "Other: qualifications where level is unspecified or cannot be determined",
  "4" = "No qualifications mentioned",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable"
)

# Convert numeric columns to character to apply labels with character keys
result$educdtlma <- as.character(result$educdtlma)
result$educdtlpa <- as.character(result$educdtlpa)
result$educma <- as.character(result$educma)
result$educpa <- as.character(result$educpa)

# Apply value labels using labelled::set_value_labels
result$educdtlma <- labelled::set_value_labels(result$educdtlma, detailed_labels)
result$educdtlpa <- labelled::set_value_labels(result$educdtlpa, detailed_labels)
result$educma <- labelled::set_value_labels(result$educma, collapsed_labels)
result$educpa <- labelled::set_value_labels(result$educpa, collapsed_labels)

# Convert to labelled factor variables
result$educdtlma <- as_factor(result$educdtlma)
result$educdtlpa <- as_factor(result$educdtlpa)
result$educma <- as_factor(result$educma)
result$educpa <- as_factor(result$educpa)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output CSV
write_csv(result, "data/output/cleaned_data.csv")

# Print summary
print(paste("Output saved to data/output/cleaned_data.csv"))
print(paste("Number of observations:", nrow(result)))
print(paste("Number of variables:", ncol(result)))
print("Variable names:")
print(names(result))
