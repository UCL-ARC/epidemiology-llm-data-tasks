library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the three files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab",
                     delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab",
                     delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab",
                     delim = "\t", show_col_types = FALSE)

# Merge using full_join by NSID
data <- full_join(wave1, wave2, by = "NSID")
data <- full_join(data, wave4, by = "NSID")

# Function to consolidate across waves with proper NA handling
consolidate_education <- function(w1, w2, w4) {
  result <- w1
  # Replace NAs with wave 2 values
  na_idx <- is.na(result)
  result[na_idx] <- w2[na_idx]
  # Replace remaining NAs with wave 4 values
  na_idx <- is.na(result)
  result[na_idx] <- w4[na_idx]
  return(result)
}

# Consolidate detailed maternal education
educdtlma <- consolidate_education(data$W1hiqualmum, data$W2hiqualmum, data$w4hiqualmum)

# Consolidate detailed paternal education
educdtlpa <- consolidate_education(data$W1hiqualdad, data$W2hiqualdad, data$w4hiqualdad)

# Function to harmonize missing values to standard scheme
harmonize_missing <- function(x) {
  # Map wave-specific missing codes to standard scheme
  x[x == -999] <- -2  # data lost
  x[x == -99] <- -2   # not interviewed
  x[x == -98] <- -2   # not present
  x[x == -94] <- -8   # insufficient information
  x[x == -92] <- -9   # refused
  x[x == -91] <- -1   # not applicable
  x[x == -1] <- -8    # don't know
  
  # Convert NA to -3 (not asked)
  x[is.na(x)] <- -3
  
  return(x)
}

# Harmonize missing codes for detailed variables
educdtlma <- harmonize_missing(educdtlma)
educdtlpa <- harmonize_missing(educdtlpa)

# Collapse detailed variables to 5-level scheme
collapse_education <- function(x) {
  result <- x
  
  # NVQ 4-5: codes 1-4 (Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4)
  result[x %in% 1:4] <- 0
  
  # NVQ 1-3: codes 5-9, 13-17 (Teaching, Nursing, A Levels, OND, NVQ3, CSYS, Scottish Higher, AS, Apprenticeship, NVQ2, GCSE A-C)
  result[x %in% c(5,6,7,8,9,13,14,15,17)] <- 1
  
  # None/entry: code 18 (Youth training)
  result[x == 18] <- 2
  
  # Other: code 19 (level unspecified)
  result[x == 19] <- 3
  
  # No qualifications: code 20
  result[x == 20] <- 4
  
  # Preserve negative codes
  result[result < 0] <- x[result < 0]
  
  return(result)
}

educma <- collapse_education(educdtlma)
educpa <- collapse_education(educdtlpa)

# Create labelled variables using labelled() with named numeric vector
# The key is to use numeric names (as numbers, not strings)
val_labels_num <- c(
  `0` = 0,
  `1` = 1,
  `2` = 2,
  `3` = 3,
  `4` = 4,
  `-3` = -3,
  `-2` = -2,
  `-1` = -1
)

# Use labelled::val_labels with labelled::set_labels
# Actually, let's use labelled::val_labels with a named character vector but specify the values as numeric
val_labels_char <- c(
  "0" = "NVQ 4-5: degree-level qualifications",
  "1" = "NVQ 1-3: sub-degree qualifications",
  "2" = "None/entry: training below NVQ level",
  "3" = "Other: unspecified level",
  "4" = "No qualifications mentioned",
  "-3" = "Not asked at fieldwork stage",
  "-2" = "Schedule not applicable/Script error",
  "-1" = "Item not applicable"
)

# Create labelled variables using labelled() function
# The labels should be numeric values that match the data
educma <- labelled(educma, labels = val_labels_num)
educpa <- labelled(educpa, labels = val_labels_num)
educdtlma <- labelled(educdtlma, labels = val_labels_num)
educdtlpa <- labelled(educdtlpa, labels = val_labels_num)

# Create final dataset with exactly 5 variables
cleaned_data <- data.frame(
  NSID = data$NSID,
  educma = educma,
  educpa = educpa,
  educdtlma = educdtlma,
  educdtlpa = educdtlpa
)

# Convert NSID to factor
cleaned_data$NSID <- as.factor(cleaned_data$NSID)

# Write output file
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(cleaned_data), "\n")
cat("Number of variables:", ncol(cleaned_data), "\n")