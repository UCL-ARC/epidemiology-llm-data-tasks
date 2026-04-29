library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing codes
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked/interviewed/Null

# Helper function to harmonize missing values based on metadata
# This function maps specific wave codes to the standard codes
harmonize_missing <- function(val, wave_mappings) {
  # First, handle Nulls
  val[is.na(val)] <- -3
  
  for (wave_code in names(wave_mappings)) {
    val[val == as.numeric(wave_code)] <- wave_mappings[[wave_code]]
  }
  
  # Any remaining negative values not in mapping but in the -999 to -1 range 
  # should be handled carefully. The prompt says follow exact mappings.
  return(val)
}

# Load files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))

# Merge datasets
merged_data <- data_list %>% reduce(full_join, by = 'NSID')

# Sex Harmonization
# We need to create age-specific sex variables first, then a consolidated one.
# Wave mappings for sex variables based on metadata
# Standard: -9 Refusal, -8 DK, -1 NA, -3 Not asked

# W1sexYP: -99 -> -3, -92 -> -9, -91 -> -1
# W2SexYP: -998, -997, -995, -99 -> -3, -92 -> -9, -91 -> -1, -1 -> -8
# W3sexYP: -99 -> -3, -92 -> -9, -91 -> -1
# W4SexYP: -99 -> -3, -92 -> -9, -91 -> -1, -1 -> -8
# W5SexYP: -1 -> -8
# W6Sex: -92 -> -9, -91 -> -1
# W7Sex: -91 -> -1
# W8CMSEX: -9 -> -9, -8 -> -8, -1 -> -1
# W9DSEX: (only positive values provided)

process_sex <- function(vec, mappings) {
  res <- vec
  res[is.na(res)] <- -3
  for (code in names(mappings)) {
    res[res == as.numeric(code)] <- mappings[[code]]
  }
  return(res)
}

# Define mappings per wave
m1 <- c('-99' = -3, '-92' = -9, '-91' = -1)
m2 <- c('-998' = -3, '-997' = -3, '-995' = -3, '-99' = -3, '-92' = -9, '-91' = -1, '-1' = -8)
m3 <- c('-99' = -3, '-92' = -9, '-91' = -1)
m4 <- c('-99' = -3, '-92' = -9, '-91' = -1, '-1' = -8)
m5 <- c('-1' = -8)
m6 <- c('-92' = -9, '-91' = -1)
m7 <- c('-91' = -1)
m8 <- c('-9' = -9, '-8' = -8, '-1' = -1)
m9 <- c()

merged_data <- merged_data %>%
  mutate(
    sex14 = process_sex(W1sexYP, m1),
    sex15 = process_sex(W2SexYP, m2),
    sex16 = process_sex(W3sexYP, m3),
    sex17 = process_sex(W4SexYP, m4),
    sex18 = process_sex(W5SexYP, m5),
    sex19 = process_sex(W6Sex, m6),
    sex20 = process_sex(W7Sex, m7),
    sex21 = process_sex(W8CMSEX, m8),
    sex32 = process_sex(W9DSEX, m9)
  )

# Consolidated Sex (Time-Invariant)
# Prioritize most recent valid (positive), then fall back to earliest
# Waves order: sex32, sex21, sex20, sex19, sex18, sex17, sex16, sex15, sex14

consolidate_stable <- function(df, cols) {
  # We want the most recent positive value
  # Create a matrix of values
  mat <- as.matrix(df[, cols])
  
  # Find indices of positive values
  pos_idx <- which(mat > 0, arr.ind = TRUE)
  
  # For each row, find the last column index that has a positive value
  final_vals <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    row_pos <- which(mat[i, ] > 0)
    if (length(row_pos) > 0) {
      final_vals[i] <- mat[i, max(row_pos)]
    } else {
      # If no positive value, we check for the most recent missing code
      # The logic says: prioritse most recent valid response, fall back to earliest valid wave
      # Actually, the instruction says "Prioritise the most recent valid response... and fall back to earliest valid wave"
      # Let's interpret "valid" as any response including missing codes, but "positive" for the prioritisation
      # Since sex is stable, we just need the most recent non-NA/non-null value if no positive exists
      
      # Fallback to earliest wave if most recent is missing
      row_vals <- mat[i, ]
      # Find first non-NA (which we coded as -3)
      first_val_idx <- which(row_vals != -3)[1]
      if (!is.na(first_val_idx)) {
        final_vals[i] <- row_vals[first_val_idx]
      } else {
        final_vals[i] <- -3
      }
    }
  }
  return(final_vals)
}

sex_cols <- c("sex14", "sex15", "sex16", "sex17", "sex18", "sex19", "sex20", "sex21", "sex32")
merged_data$sex <- consolidate_stable(merged_data, sex_cols)

# Convert to factor with labels
sex_labels <- c("-9" = "Refusal", "-8" = "Don't know", "-1" = "Not applicable", "-3" = "Not asked", "1" = "Male", "2" = "Female")
merged_data$sex <- factor(merged_data$sex, levels = as.numeric(names(sex_labels)), labels = sex_labels)

# Select only final variables
final_data <- merged_data %>%
  select(NSID, sex)

# Write to CSV
dir.create("data/output", recursive = TRUE)
write_csv(final_data, "data/output/cleaned_data.csv")