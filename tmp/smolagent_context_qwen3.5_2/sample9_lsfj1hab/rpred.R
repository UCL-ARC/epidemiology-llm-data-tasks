library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load the three wave files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge by NSID using full_join
merged <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w4, by = "NSID")

# Select only parental education variables plus NSID
selected <- merged %>%
  select(NSID, W1hiqualmum, W1hiqualdad, W2hiqualmum, W2hiqualdad, w4hiqualmum, w4hiqualdad)

# Function to harmonize missing value codes to standard scheme
harmonize_missing <- function(x) {
  x <- case_when(
    x == -999 ~ -2,  # Missing - household data lost -> Schedule not applicable
    x == -99 ~ -3,   # Parent not interviewed -> Not asked
    x == -98 ~ -3,   # Parent not present -> Not asked
    x == -94 ~ -8,   # Insufficient information -> Don't know
    x == -92 ~ -9,   # Refused -> Refusal
    x == -91 ~ -1,   # Not applicable -> Item not applicable
    x == -1 ~ -8,    # Don't know -> Don't know
    TRUE ~ as.numeric(x)
  )
  return(x)
}

# Apply harmonization to all education variables
selected <- selected %>%
  mutate(
    W1hiqualmum = harmonize_missing(W1hiqualmum),
    W1hiqualdad = harmonize_missing(W1hiqualdad),
    W2hiqualmum = harmonize_missing(W2hiqualmum),
    W2hiqualdad = harmonize_missing(W2hiqualdad),
    w4hiqualmum = harmonize_missing(w4hiqualmum),
    w4hiqualdad = harmonize_missing(w4hiqualdad)
  )

# Replace NULL/NA values with -3 (Not asked)
selected <- selected %>%
  mutate(
    W1hiqualmum = ifelse(is.na(W1hiqualmum), -3, W1hiqualmum),
    W1hiqualdad = ifelse(is.na(W1hiqualdad), -3, W1hiqualdad),
    W2hiqualmum = ifelse(is.na(W2hiqualmum), -3, W2hiqualmum),
    W2hiqualdad = ifelse(is.na(W2hiqualdad), -3, W2hiqualdad),
    w4hiqualmum = ifelse(is.na(w4hiqualmum), -3, w4hiqualmum),
    w4hiqualdad = ifelse(is.na(w4hiqualdad), -3, w4hiqualdad)
  )

# Function to consolidate education across waves (vectorized)
consolidate_educ_row <- function(w1_val, w2_val, w4_val) {
  # Prioritize positive values (1-20) in wave order
  if (!is.na(w1_val) && w1_val >= 1 && w1_val <= 20) {
    return(w1_val)
  }
  if (!is.na(w2_val) && w2_val >= 1 && w2_val <= 20) {
    return(w2_val)
  }
  if (!is.na(w4_val) && w4_val >= 1 && w4_val <= 20) {
    return(w4_val)
  }
  
  # Fall back to negative codes in wave order
  if (!is.na(w1_val) && w1_val < 0) {
    return(w1_val)
  }
  if (!is.na(w2_val) && w2_val < 0) {
    return(w2_val)
  }
  if (!is.na(w4_val) && w4_val < 0) {
    return(w4_val)
  }
  
  return(NA_real_)
}

# Create consolidated detailed variables using rowwise operation
selected <- selected %>%
  rowwise() %>%
  mutate(
    educdtlma = consolidate_educ_row(W1hiqualmum, W2hiqualmum, w4hiqualmum),
    educdtlpa = consolidate_educ_row(W1hiqualdad, W2hiqualdad, w4hiqualdad)
  ) %>%
  ungroup()

# Create collapsed variables (5-level NVQ scheme) - vectorized version
# 0 = NVQ 4-5 (degree-level: 1-4)
# 1 = NVQ 1-3 (sub-degree: 5-17)
# 2 = None/entry (18)
# 3 = Other (19)
# 4 = No qualifications (20)
# Negative codes preserved

selected <- selected %>%
  mutate(
    educma = case_when(
      is.na(educdtlma) ~ NA_real_,
      educdtlma < 0 ~ educdtlma,
      educdtlma >= 1 & educdtlma <= 4 ~ 0,
      educdtlma >= 5 & educdtlma <= 17 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3,
      educdtlma == 20 ~ 4,
      TRUE ~ NA_real_
    ),
    educpa = case_when(
      is.na(educdtlpa) ~ NA_real_,
      educdtlpa < 0 ~ educdtlpa,
      educdtlpa >= 1 & educdtlpa <= 4 ~ 0,
      educdtlpa >= 5 & educdtlpa <= 17 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ NA_real_
    )
  )

# Apply value labels to detailed variables
selected <- selected %>%
  mutate(
    educdtlma = set_value_labels(educdtlma, c(
      "Higher Degree" = 1,
      "First Degree" = 2,
      "HE Diploma" = 3,
      "HNC/HND/NVQ4" = 4,
      "Teaching qualification, non-degree" = 5,
      "Nursing qualification, non-degree" = 6,
      "A Levels" = 7,
      "OND/ONC" = 8,
      "City and guilds part III, NVQ3" = 9,
      "CSYS" = 10,
      "Scottish Higher Grade" = 11,
      "AS Level" = 12,
      "Trade apprenticeship" = 13,
      "City and guilds part II, NVQ2" = 14,
      "GCSE grade A-C and equivalent" = 15,
      "GCSE grade D-E and equivalent" = 16,
      "City and guilds part I, NVQ1" = 17,
      "Youth training, skill seekers" = 18,
      "Qualification, level unspecified" = 19,
      "No qualification mentioned" = 20,
      "Item not applicable" = -1,
      "Schedule not applicable" = -2,
      "Not asked" = -3,
      "Don't know/insufficient information" = -8,
      "Refusal" = -9
    )),
    educdtlpa = set_value_labels(educdtlpa, c(
      "Higher Degree" = 1,
      "First Degree" = 2,
      "HE Diploma" = 3,
      "HNC/HND/NVQ4" = 4,
      "Teaching qualification, non-degree" = 5,
      "Nursing qualification, non-degree" = 6,
      "A Levels" = 7,
      "OND/ONC" = 8,
      "City and guilds part III, NVQ3" = 9,
      "CSYS" = 10,
      "Scottish Higher Grade" = 11,
      "AS Level" = 12,
      "Trade apprenticeship" = 13,
      "City and guilds part II, NVQ2" = 14,
      "GCSE grade A-C and equivalent" = 15,
      "GCSE grade D-E and equivalent" = 16,
      "City and guilds part I, NVQ1" = 17,
      "Youth training, skill seekers" = 18,
      "Qualification, level unspecified" = 19,
      "No qualification mentioned" = 20,
      "Item not applicable" = -1,
      "Schedule not applicable" = -2,
      "Not asked" = -3,
      "Don't know/insufficient information" = -8,
      "Refusal" = -9
    ))
  )

# Apply value labels to collapsed variables
selected <- selected %>%
  mutate(
    educma = set_value_labels(educma, c(
      "NVQ 4-5 (degree-level)" = 0,
      "NVQ 1-3 (sub-degree)" = 1,
      "None/entry" = 2,
      "Other" = 3,
      "No qualifications mentioned" = 4,
      "Item not applicable" = -1,
      "Schedule not applicable" = -2,
      "Not asked" = -3,
      "Don't know/insufficient information" = -8,
      "Refusal" = -9
    )),
    educpa = set_value_labels(educpa, c(
      "NVQ 4-5 (degree-level)" = 0,
      "NVQ 1-3 (sub-degree)" = 1,
      "None/entry" = 2,
      "Other" = 3,
      "No qualifications mentioned" = 4,
      "Item not applicable" = -1,
      "Schedule not applicable" = -2,
      "Not asked" = -3,
      "Don't know/insufficient information" = -8,
      "Refusal" = -9
    ))
  )

# Select final variables
final <- selected %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Convert to factors with labels
final <- final %>%
  mutate(
    educma = as_factor(educma),
    educpa = as_factor(educpa),
    educdtlma = as_factor(educdtlma),
    educdtlpa = as_factor(educdtlpa)
  )

# Write to CSV
write_csv(final, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(final), "\n")
cat("Variables:", paste(names(final), collapse = ", "), "\n")