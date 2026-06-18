library(dplyr)
library(readr)
library(tidyr)
library(labelled)
library(haven)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

cat("W1 dimensions:", dim(w1), "\n")
cat("W2 dimensions:", dim(w2), "\n")
cat("W4 dimensions:", dim(w4), "\n")

# Function to convert missing value codes to standard codes
convert_missing <- function(x) {
  recode <- function(val) {
    if (is.na(val)) return(-3)  # R NA -> -3 (not asked)
    if (val > 0) return(val)    # Valid substantive response
    if (val == -1) return(-8)   # "Don't know" -> -8
    if (val == -91) return(-1)  # Not applicable -> -1
    if (val == -92) return(-9)  # Refused -> -9
    if (val == -94) return(-8)  # Insufficient information -> -8
    if (val == -98) return(-2)  # Mother/Father not present -> -2
    if (val == -99) return(-3)  # Not interviewed -> -3
    if (val == -999) return(-2) # Data lost -> -2
    return(-3)  # Default fallback
  }
  result <- sapply(x, recode)
  # Strip names to ensure plain vector
  result <- as.numeric(result)
  result
}

# Convert education variables from each source
# Mother's education
w1$W1hiqualmum_clean <- convert_missing(w1$W1hiqualmum)
w2$W2hiqualmum_clean <- convert_missing(w2$W2hiqualmum)
w4$w4hiqualmum_clean <- convert_missing(w4$w4hiqualmum)

# Father's education
w1$W1hiqualdad_clean <- convert_missing(w1$W1hiqualdad)
w2$W2hiqualdad_clean <- convert_missing(w2$W2hiqualdad)
w4$w4hiqualdad_clean <- convert_missing(w4$w4hiqualdad)

# Merge all files by NSID using full_join
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w4, by = "NSID")

cat("Merged dimensions:", dim(df), "\n")

# Consolidation function: earliest-valid-first
df$educdtlma <- NA_real_
df$educdtlpa <- NA_real_

for (i in seq_len(nrow(df))) {
  # Mother's education
  v1 <- df$W1hiqualmum_clean[i]
  v2 <- df$W2hiqualmum_clean[i]
  v4 <- df$w4hiqualmum_clean[i]
  
  if (!is.na(v1) && v1 > 0) {
    df$educdtlma[i] <- v1
  } else if (!is.na(v2) && v2 > 0) {
    df$educdtlma[i] <- v2
  } else if (!is.na(v4) && v4 > 0) {
    df$educdtlma[i] <- v4
  } else {
    # Use first available missing code
    miss <- c(v1, v2, v4)
    miss <- miss[!is.na(miss)]
    if (length(miss) > 0) {
      df$educdtlma[i] <- miss[1]
    } else {
      df$educdtlma[i] <- -3
    }
  }
  
  # Father's education
  v1 <- df$W1hiqualdad_clean[i]
  v2 <- df$W2hiqualdad_clean[i]
  v4 <- df$w4hiqualdad_clean[i]
  
  if (!is.na(v1) && v1 > 0) {
    df$educdtlpa[i] <- v1
  } else if (!is.na(v2) && v2 > 0) {
    df$educdtlpa[i] <- v2
  } else if (!is.na(v4) && v4 > 0) {
    df$educdtlpa[i] <- v4
  } else {
    miss <- c(v1, v2, v4)
    miss <- miss[!is.na(miss)]
    if (length(miss) > 0) {
      df$educdtlpa[i] <- miss[1]
    } else {
      df$educdtlpa[i] <- -3
    }
  }
}

cat("educdtlma unique values:", sort(unique(df$educdtlma)), "\n")
cat("educdtlpa unique values:", sort(unique(df$educdtlpa)), "\n")

# Map 20 categories to 5 NVQ levels
map_to_nvq5 <- function(x) {
  result <- rep(-3, length(x))
  for (i in seq_len(length(x))) {
    val <- x[i]
    if (val > 0) {
      if (val %in% c(1, 2)) {
        result[i] <- 5
      } else if (val %in% c(3, 4)) {
        result[i] <- 4
      } else if (val %in% c(5, 6, 7, 9, 10, 11, 12, 13, 19)) {
        result[i] <- 3
      } else if (val %in% c(8, 14, 15, 18)) {
        result[i] <- 2
      } else if (val %in% c(16, 17)) {
        result[i] <- 1
      } else if (val == 20) {
        result[i] <- 1
      } else {
        result[i] <- 1
      }
    } else {
      result[i] <- val
    }
  }
  result
}

df$educma <- as.integer(map_to_nvq5(df$educdtlma))
df$educpa <- as.integer(map_to_nvq5(df$educdtlpa))

cat("educma unique values:", sort(unique(df$educma)), "\n")
cat("educpa unique values:", sort(unique(df$educpa)), "\n")

# Create labelled factors for the output

# Detailed 20-category labels
dtl_labels <- c(
  "Higher Degree" = 1, "First Degree" = 2, "HE Diploma" = 3,
  "HNC/HND/NVQ4" = 4, "Teaching qualification, non-degree" = 5,
  "Nursing qualification, non-degree" = 6, "A Levels" = 7,
  "OND/ONC" = 8, "City and guilds part III, NVQ3" = 9,
  "CSYS" = 10, "Scottish Higher Grade" = 11, "AS Level" = 12,
  "Trade apprenticeship" = 13, "City and guilds part II, NVQ2" = 14,
  "GCSE grade A-C and equivalent" = 15, "GCSE grade D-E and equivalent" = 16,
  "City and guilds part I, NVQ1" = 17, "Youth training, skill seekers" = 18,
  "Qualification, level unspecified" = 19, "No qualification mentioned" = 20
)

# Add missing value labels
dtl_labels <- c(dtl_labels, "Refusal" = -9, "Don't know" = -8, "Prefer not to say" = -7,
                "Not asked / not interviewed" = -3, "Schedule not applicable" = -2,
                "Item not applicable" = -1)

# 5-level NVQ labels
nvq5_labels <- c(
  "Level 5: Higher Degree" = 5, "Level 4: HNC/HND/NVQ4" = 4,
  "Level 3: A-levels / NVQ3" = 3, "Level 2: GCSE A-C / NVQ2" = 2,
  "Level 1: GCSE D-E / NVQ1 / no qualification" = 1
)
nvq5_labels <- c(nvq5_labels, "Refusal" = -9, "Don't know" = -8, "Prefer not to say" = -7,
                 "Not asked / not interviewed" = -3, "Schedule not applicable" = -2,
                 "Item not applicable" = -1)

# Create final output dataframe
out <- df %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

# Add labelled attributes
out$educdtlma <- haven::labelled(out$educdtlma, labels = dtl_labels)
out$educdtlpa <- haven::labelled(out$educdtlpa, labels = dtl_labels)
out$educma <- haven::labelled(out$educma, labels = nvq5_labels)
out$educpa <- haven::labelled(out$educpa, labels = nvq5_labels)

cat("Output dimensions:", dim(out), "\n")
cat("Unique NSIDs:", length(unique(out$NSID)), "\n")

# Summary statistics
cat("\neducdtlma distribution:\n")
print(table(out$educdtlma, useNA = "ifany"))

cat("\neducma distribution:\n")
print(table(out$educma, useNA = "ifany"))

# Write output
write_csv(out, "data/output/cleaned_data.csv")
cat("\nOutput written to data/output/cleaned_data.csv\n")
