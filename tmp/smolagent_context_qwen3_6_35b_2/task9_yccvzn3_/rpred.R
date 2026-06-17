library(dplyr)
library(readr)
library(haven)
library(labelled)
library(tidyr)
library(purrr)

# Load all three files from data/input/
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

cat("W1 dimensions:", dim(w1), "\n")
cat("W2 dimensions:", dim(w2), "\n")
cat("W4 dimensions:", dim(w4), "\n")

# Function to harmonise missing codes to standard scheme
harmonise_missing <- function(x) {
  x[x == -999] <- -2  # Missing - household data lost
  x[x == -99] <- -3   # Not interviewed
  x[x == -98] <- -3   # Not present
  x[x == -94] <- -8   # Insufficient information
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -1] <- -8    # Don't know (only in father vars)
  x[x == -100] <- -2  # Script error / not applicable
  x[x == -97] <- -2   # Depending on label
  return(x)
}

# Harmonise missing codes for mother's education variables
w1$W1hiqualmum <- harmonise_missing(w1$W1hiqualmum)
w2$W2hiqualmum <- harmonise_missing(w2$W2hiqualmum)
w4$w4hiqualmum <- harmonise_missing(w4$w4hiqualmum)

# Harmonise missing codes for father's education variables
w1$W1hiqualdad <- harmonise_missing(w1$W1hiqualdad)
w2$W2hiqualdad <- harmonise_missing(w2$W2hiqualdad)
w4$w4hiqualdad <- harmonise_missing(w4$w4hiqualdad)

# Merge all three files by NSID
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w4, by = "NSID")

cat("Merged dataset dimensions:", dim(df), "\n")

# Function to consolidate from earliest-valid-first (waves 1, 2, 4)
consolidate_earliest <- function(w1var, w2var, w4var) {
  result <- rep(-3, length(w1var))  # Default: not asked
  
  for (i in seq_along(w1var)) {
    v1 <- w1var[i]
    v2 <- w2var[i]
    v4 <- w4var[i]
    
    # Skip if all are NA
    if (is.na(v1) && is.na(v2) && is.na(v4)) {
      result[i] <- -3
      next
    }
    
    # First priority: valid positive codes (1-20)
    if (!is.na(v1) && v1 >= 1 && v1 <= 20) {
      result[i] <- v1
    } else if (!is.na(v2) && v2 >= 1 && v2 <= 20) {
      result[i] <- v2
    } else if (!is.na(v4) && v4 >= 1 && v4 <= 20) {
      result[i] <- v4
    } else {
      # No valid positive code; take first non-NA (which is a negative/missing code)
      if (!is.na(v1) && v1 != -3) {
        result[i] <- v1
      } else if (!is.na(v2) && v2 != -3) {
        result[i] <- v2
      } else if (!is.na(v4) && v4 != -3) {
        result[i] <- v4
      } else {
        result[i] <- -3
      }
    }
  }
  return(result)
}

# Derive detailed education variables (20 categories)
educdtlma <- consolidate_earliest(df$W1hiqualmum, df$W2hiqualmum, df$w4hiqualmum)
educdtlpa <- consolidate_earliest(df$W1hiqualdad, df$W2hiqualdad, df$w4hiqualdad)

# Derive collapsed 5-level NVQ scheme from detailed variables
collapse_nvq <- function(detailed) {
  result <- rep(-3, length(detailed))
  
  for (i in seq_along(detailed)) {
    v <- detailed[i]
    
    # If it's a valid detailed code
    if (!is.na(v) && v >= 1 && v <= 20) {
      if (v %in% c(1, 2, 3, 4)) {
        result[i] <- 0  # NVQ 4-5
      } else if (v %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)) {
        result[i] <- 1  # NVQ 1-3
      } else if (v == 18) {
        result[i] <- 2  # Youth training
      } else if (v == 19) {
        result[i] <- 3  # Level unspecified
      } else if (v == 20) {
        result[i] <- 4  # No qualification
      }
    } else {
      # Preserve missing codes
      result[i] <- v
    }
  }
  return(result)
}

educma <- collapse_nvq(educdtlma)
educpa <- collapse_nvq(educdtlpa)

# Create final output dataframe
cleaned <- df %>%
  select(NSID) %>%
  mutate(
    educdtlma = educdtlma,
    educdtlpa = educdtlpa,
    educma = educma,
    educpa = educpa
  )

# Check for any remaining NAs
cat("NAs in educdtlma:", sum(is.na(cleaned$educdtlma)), "\n")
cat("NAs in educdtlpa:", sum(is.na(cleaned$educdtlpa)), "\n")
cat("NAs in educma:", sum(is.na(cleaned$educma)), "\n")
cat("NAs in educpa:", sum(is.na(cleaned$educpa)), "\n")

# Write output
write_csv(cleaned, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Final dimensions:", dim(cleaned), "\n")

# Show some summary statistics
cat("\neducdtlma distribution:\n")
print(table(as.numeric(as.character(cleaned$educdtlma)), useNA = "ifany"))
cat("\neducma distribution:\n")
print(table(as.numeric(as.character(cleaned$educma)), useNA = "ifany"))
