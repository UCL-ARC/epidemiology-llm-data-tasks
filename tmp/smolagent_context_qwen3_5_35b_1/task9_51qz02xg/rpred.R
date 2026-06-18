library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the three data files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Merge all files
full_data <- full_join(wave1, wave2, by = 'NSID')
full_data <- full_join(full_data, wave4, by = 'NSID')

cat('Data loaded successfully. Dimensions:', dim(full_data), '\n')

# Function to harmonize missing values based on label meaning
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  
  x <- case_when(
    x == -999 ~ -2,
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
  
  return(x)
}

# Extract the parental education variables from each wave
w1_mum <- harmonize_missing(full_data$W1hiqualmum)
w1_dad <- harmonize_missing(full_data$W1hiqualdad)
w2_mum <- harmonize_missing(full_data$W2hiqualmum)
w2_dad <- harmonize_missing(full_data$W2hiqualdad)
w4_mum <- harmonize_missing(full_data$w4hiqualmum)
w4_dad <- harmonize_missing(full_data$w4hiqualdad)

# Function to consolidate parental education
consolidate_education <- function(w1, w2, w4) {
  result <- rep(-3, length(w1))
  
  for (i in seq_along(w1)) {
    vals <- c(w1[i], w2[i], w4[i])
    vals <- vals[!is.na(vals)]
    
    if (length(vals) == 0) {
      result[i] <- -3
    } else {
      positive_vals <- vals[vals >= 1 & vals <= 20]
      if (length(positive_vals) > 0) {
        result[i] <- positive_vals[1]
      } else {
        result[i] <- vals[1]
      }
    }
  }
  
  return(result)
}

# Consolidate detailed education variables
educdtlma <- consolidate_education(w1_mum, w2_mum, w4_mum)
educdtlpa <- consolidate_education(w1_dad, w2_dad, w4_dad)

# Function to collapse detailed education to 5-level NVQ scheme
collapse_nvq <- function(x) {
  result <- rep(NA_real_, length(x))
  
  for (i in seq_along(x)) {
    if (x[i] >= 1 && x[i] <= 20) {
      if (x[i] %in% c(1, 2, 3, 4)) {
        result[i] <- 0
      } else if (x[i] %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)) {
        result[i] <- 1
      } else if (x[i] == 18) {
        result[i] <- 2
      } else if (x[i] == 19) {
        result[i] <- 3
      } else if (x[i] == 20) {
        result[i] <- 4
      }
    } else {
      result[i] <- x[i]
    }
  }
  
  return(result)
}

# Derive collapsed NVQ variables
educma <- collapse_nvq(educdtlma)
educpa <- collapse_nvq(educdtlpa)

# Create output dataframe
output <- tibble(
  NSID = full_data$NSID,
  educdtlma = educdtlma,
  educdtlpa = educdtlpa,
  educma = educma,
  educpa = educpa
)

# Write output to CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Number of records:', nrow(output), '\n')
cat('Variables:', names(output), '\n')

# Show summary statistics
cat('\nSummary statistics for educdtlma:\n')
print(table(educdtlma, useNA = 'ifany'))
cat('\nSummary statistics for educdtlpa:\n')
print(table(educdtlpa, useNA = 'ifany'))
cat('\nSummary statistics for educma:\n')
print(table(educma, useNA = 'ifany'))
cat('\nSummary statistics for educpa:\n')
print(table(educpa, useNA = 'ifany'))
