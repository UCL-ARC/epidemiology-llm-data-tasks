library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

cat("Wave1 rows:", nrow(wave1), "\n")
cat("Wave2 rows:", nrow(wave2), "\n")
cat("Wave4 rows:", nrow(wave4), "\n")

# Merge all files by NSID using full_join
cleaned <- full_join(wave1, wave2, by = "NSID")
cleaned <- full_join(cleaned, wave4, by = "NSID")

cat("Cleaned rows:", nrow(cleaned), "\n")

# Define missing value mapping function
map_missing_values <- function(x) {
  x_clean <- x
  x_clean[x_clean == -999] <- -2
  x_clean[x_clean == -99] <- -3
  x_clean[x_clean == -98] <- -2
  x_clean[x_clean == -94] <- -8
  x_clean[x_clean == -92] <- -9
  x_clean[x_clean == -91] <- -1
  x_clean[x_clean == -1 & !is.na(x_clean)] <- -8
  return(x_clean)
}

# Create consolidated detailed variables using a proper loop
get_earliest_valid <- function(v1, v2, v4, n_rows) {
  result <- rep(-3, n_rows)
  
  for (i in 1:n_rows) {
    x1 <- if (i <= length(v1)) v1[i] else NA
    x2 <- if (i <= length(v2)) v2[i] else NA
    x4 <- if (i <= length(v4)) v4[i] else NA
    
    if (!is.na(x1) && x1 > 0) {
      result[i] <- x1
    } else if (!is.na(x2) && x2 > 0) {
      result[i] <- x2
    } else if (!is.na(x4) && x4 > 0) {
      result[i] <- x4
    } else {
      missing_codes <- c(x1, x2, x4)
      missing_codes <- missing_codes[!is.na(missing_codes) & missing_codes < 0]
      if (length(missing_codes) > 0) {
        result[i] <- min(missing_codes)
      }
    }
  }
  return(result)
}

# Create consolidated detailed variables (20-category)
educdtlma <- get_earliest_valid(wave1$W1hiqualmum, wave2$W2hiqualmum, wave4$w4hiqualmum, nrow(cleaned))
educdtlpa <- get_earliest_valid(wave1$W1hiqualdad, wave2$W2hiqualdad, wave4$w4hiqualdad, nrow(cleaned))

# Create 5-level NVQ categories
map_to_nvq5 <- function(x) {
  n <- length(x)
  result <- rep(-3, n)
  
  for (i in 1:n) {
    val <- x[i]
    if (is.na(val)) {
      result[i] <- -3
    } else if (val < 0) {
      result[i] <- val
    } else if (val == 1 || val == 2 || val == 3) {
      result[i] <- 5
    } else if (val == 4) {
      result[i] <- 4
    } else if (val == 7 || val == 9) {
      result[i] <- 3
    } else if (val == 14 || val == 15) {
      result[i] <- 2
    } else if (val == 16 || val == 17 || val == 20) {
      result[i] <- 1
    } else if (val == 5 || val == 6 || val == 8 || val == 10) {
      result[i] <- 4
    } else if (val == 11 || val == 12 || val == 13) {
      result[i] <- 3
    } else if (val == 18 || val == 19) {
      result[i] <- 1
    }
  }
  return(result)
}

# Apply NVQ mapping
educma <- map_to_nvq5(educdtlma)
educpa <- map_to_nvq5(educdtlpa)

# Create final output dataframe
output <- tibble(
  NSID = cleaned$NSID,
  educdtlma = educdtlma,
  educdtlpa = educdtlpa,
  educma = educma,
  educpa = educpa
)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Cleaning complete.\n")
cat("Records:", nrow(output), "\n")
cat("Columns:", paste(names(output), collapse = ", "), "\n")