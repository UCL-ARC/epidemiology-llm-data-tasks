# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  wave1 = "data/input/wave_one_lsype_family_background_2020.tab",
  wave2 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load each file
w1 <- read_delim(files["wave1"], delim = "\t", show_col_types = FALSE)
w2 <- read_delim(files["wave2"], delim = "\t", show_col_types = FALSE)
w4 <- read_delim(files["wave4"], delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w4, by = "NSID")

print(paste("Merged dataframe dimensions:", nrow(df), "x", ncol(df)))

# Function to map source values to standard missing-value codes and keep valid values
code_values <- function(x) {
  result <- rep(-3, length(x))  # default: not asked / NA
  
  # Handle valid substantive codes (1-20)
  for (i in seq_along(x)) {
    val <- x[i]
    if (!is.na(val) && val >= 1 && val <= 20) {
      result[i] <- val
    } else if (!is.na(val)) {
      # Map missing codes by their label meaning
      if (val == -999) result[i] <- -2    # Missing - household data lost
      else if (val == -99) result[i] <- -3     # Not interviewed
      else if (val == -98) result[i] <- -3     # Not present
      else if (val == -94) result[i] <- -8     # Insufficient information
      else if (val == -92) result[i] <- -9     # Refused
      else if (val == -91) result[i] <- -1     # Not applicable
      else if (val == -1) result[i] <- -8      # Don't know
    }
    # else: NA stays as -3 (not asked)
  }
  
  return(result)
}

# Function to map to 5-level NVQ categories from coded values
map_to_5level <- function(x) {
  result <- rep(-3, length(x))  # default
  
  # For missing codes, keep them as-is
  for (i in seq_along(x)) {
    val <- x[i]
    if (val %in% c(-1, -2, -3, -8, -9)) {
      result[i] <- val
    } else if (!is.na(val) && val >= 1 && val <= 20) {
      # Map valid detailed codes to 5-level NVQ
      if (val == 20) result[i] <- 1  # No qualification
      else if (val %in% c(17, 16, 18)) result[i] <- 2  # NVQ1 or equivalent
      else if (val %in% c(14, 15, 8, 10)) result[i] <- 3  # NVQ2 or equivalent
      else if (val %in% c(9, 7, 12, 13, 11, 5, 6)) result[i] <- 4  # NVQ3 or equivalent
      else if (val %in% c(4, 3, 2, 1)) result[i] <- 5  # NVQ4 or higher
      else if (val == 19) result[i] <- 0  # Qualification, level unspecified
    }
  }
  
  return(result)
}

# Function to create consolidated variable (earliest valid first)
consolidate <- function(var1, var2, var4) {
  result <- var1  # Start with wave1 (earliest)
  
  # Where wave1 is missing (-3, -8, -9, -1, -2), use wave2
  mask_w1_missing <- result %in% c(-3, -8, -9, -1, -2)
  result[mask_w1_missing] <- var2[mask_w1_missing]
  
  # Where both wave1 and wave2 are missing, use wave4
  mask_w2_missing <- mask_w1_missing & (result %in% c(-3, -8, -9, -1, -2))
  result[mask_w2_missing] <- var4[mask_w2_missing]
  
  return(result)
}

# Step 1: Code all source variables
# Mother's education
df$w1_edu_m <- code_values(df$W1hiqualmum)
df$w2_edu_m <- code_values(df$W2hiqualmum)
df$w4_edu_m <- code_values(df$w4hiqualmum)

# Father's education
df$w1_edu_p <- code_values(df$W1hiqualdad)
df$w2_edu_p <- code_values(df$W2hiqualdad)
df$w4_edu_p <- code_values(df$w4hiqualdad)

# Step 2: Create detailed 20-category consolidated variables
df$educdtlma <- consolidate(df$w1_edu_m, df$w2_edu_m, df$w4_edu_m)
df$educdtlpa <- consolidate(df$w1_edu_p, df$w2_edu_p, df$w4_edu_p)

# Step 3: Create 5-level NVQ variables (first code to detailed, then map to 5-level)
df$w1_5level_m <- map_to_5level(df$w1_edu_m)
df$w2_5level_m <- map_to_5level(df$w2_edu_m)
df$w4_5level_m <- map_to_5level(df$w4_edu_m)

df$w1_5level_p <- map_to_5level(df$w1_edu_p)
df$w2_5level_p <- map_to_5level(df$w2_edu_p)
df$w4_5level_p <- map_to_5level(df$w4_edu_p)

# Consolidate 5-level variables
df$educma <- consolidate(df$w1_5level_m, df$w2_5level_m, df$w4_5level_m)
df$educpa <- consolidate(df$w1_5level_p, df$w2_5level_p, df$w4_5level_p)

# Define labels for detailed 20-category variables (including missing codes)
detailed_labels <- c(
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
  "Not applicable" = -1,
  "Missing - household data lost" = -2,
  "Not asked" = -3,
  "Insufficient information" = -8,
  "Refused" = -9
)

# Labels for 5-level NVQ variables (including missing codes)
fivelevel_labels <- c(
  "Qualification, level unspecified" = 0,
  "No qualification" = 1,
  "NVQ1 or equivalent" = 2,
  "NVQ2 or equivalent" = 3,
  "NVQ3 or equivalent" = 4,
  "NVQ4 or higher" = 5,
  "Not applicable" = -1,
  "Missing - household data lost" = -2,
  "Not asked" = -3,
  "Insufficient information" = -8,
  "Refused" = -9
)

# Apply labels using haven::labelled()
df$educdtlma <- haven::labelled(df$educdtlma, labels = detailed_labels)
df$educdtlpa <- haven::labelled(df$educdtlpa, labels = detailed_labels)
df$educma <- haven::labelled(df$educma, labels = fivelevel_labels)
df$educpa <- haven::labelled(df$educpa, labels = fivelevel_labels)

# Remove intermediate working variables
df_out <- df %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

# Ensure output directory exists
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output
write_csv(df_out, "data/output/cleaned_data.csv")

print("Output saved successfully!")
print(paste("Output dimensions:", nrow(df_out), "x", ncol(df_out)))
print("\neducdtlma distribution:")
print(table(df_out$educdtlma, useNA = "always"))
print("\neducma distribution:")
print(table(df_out$educma, useNA = "always"))
