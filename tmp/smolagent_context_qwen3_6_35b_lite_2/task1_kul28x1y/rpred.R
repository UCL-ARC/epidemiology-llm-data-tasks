# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns9_2022_main_interview.tab"
)

# Load all files
loaded_data <- list()
for (i in seq_along(files)) {
  fname <- files[i]
  fpath <- file.path("data/input", fname)
  loaded_data[[fname]] <- read_delim(fpath, delim = "\t", show_col_types = FALSE)
  cat("Loaded", fname, "with", nrow(loaded_data[[fname]]), "rows\n")
}

# Merge all datasets by NSID using full_join
cleaned <- loaded_data[[files[1]]]
for (i in 2:length(files)) {
  cleaned <- full_join(cleaned, loaded_data[[files[i]]], by = "NSID")
}

cat("Merged dataset has", nrow(cleaned), "rows and", ncol(cleaned), "columns\n")

# Harmonization functions for each wave
harmonize_w1 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -99] <- -3    # YP not interviewed -> -3
  x[x == -92] <- -9    # Refused -> -9
  x[x == -91] <- -1    # Not applicable -> -1
  return(x)
}

harmonize_w2 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -998] <- -2   # Interviewer missed question -> -2
  x[x == -997] <- -2   # Script error -> -2
  x[x == -995] <- -2   # Missing history section data -> -2
  x[x == -99] <- -3    # YP not interviewed -> -3
  x[x == -92] <- -9    # Refused -> -9
  x[x == -91] <- -1    # Not applicable -> -1
  x[x == -1] <- -8     # Don't Know -> -8
  return(x)
}

harmonize_w3 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -99] <- -3    # YP not interviewed -> -3
  x[x == -92] <- -9    # Refused -> -9
  x[x == -91] <- -1    # Not applicable -> -1
  return(x)
}

harmonize_w4 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -99] <- -3    # YP not interviewed -> -3
  x[x == -92] <- -9    # Refused -> -9
  x[x == -91] <- -1    # Not applicable -> -1
  x[x == -1] <- -8     # Don't know -> -8
  return(x)
}

harmonize_w5 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -1] <- -8     # Don't know -> -8
  return(x)
}

harmonize_w6 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -92] <- -9    # Refused -> -9
  x[x == -91] <- -1    # Not applicable -> -1
  return(x)
}

harmonize_w7 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -91] <- -1    # Not applicable -> -1
  return(x)
}

harmonize_w8 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -9] <- -9     # Refused -> -9
  x[x == -8] <- -8     # Don't know -> -8
  x[x == -1] <- -1     # Not applicable -> -1
  return(x)
}

harmonize_w9 <- function(x) {
  x[is.na(x)] <- -3    # No user missing values defined, default to -3
  return(x)
}

# Apply harmonization to each wave's sex variable
cleaned$W1sexYP_h <- harmonize_w1(cleaned$W1sexYP)
cleaned$W2SexYP_h <- harmonize_w2(cleaned$W2SexYP)
cleaned$W3sexYP_h <- harmonize_w3(cleaned$W3sexYP)
cleaned$W4SexYP_h <- harmonize_w4(cleaned$W4SexYP)
cleaned$W5SexYP_h <- harmonize_w5(cleaned$W5SexYP)
cleaned$W6Sex_h <- harmonize_w6(cleaned$W6Sex)
cleaned$W7Sex_h <- harmonize_w7(cleaned$W7Sex)
cleaned$W8CMSEX_h <- harmonize_w8(cleaned$W8CMSEX)
cleaned$W9DSEX_h <- harmonize_w9(cleaned$W9DSEX)

# Create consolidated sex variable using most-recent-valid-first
consolidate_sex <- function(w9, w8, w7, w6, w5, w4, w3, w2, w1) {
  result <- rep(NA_real_, length(w9))
  
  for (i in seq_along(result)) {
    # Check waves in most-recent-first order
    wave_values <- c(w9[i], w8[i], w7[i], w6[i], w5[i], w4[i], w3[i], w2[i], w1[i])
    
    for (v in wave_values) {
      if (!is.na(v) && v %in% c(1, 2)) {
        result[i] <- v
        break
      }
    }
    
    # If no valid response found, use -3 (not available)
    if (is.na(result[i])) {
      result[i] <- -3
    }
  }
  
  return(result)
}

cleaned$sex <- consolidate_sex(
  cleaned$W9DSEX_h, cleaned$W8CMSEX_h, cleaned$W7Sex_h,
  cleaned$W6Sex_h, cleaned$W5SexYP_h, cleaned$W4SexYP_h,
  cleaned$W3sexYP_h, cleaned$W2SexYP_h, cleaned$W1sexYP_h
)

# Create labelled numeric vector for sex
# Valid labels: 1 = Male, 2 = Female
cleaned$sex <- haven::labelled(cleaned$sex, labels = c(Male = 1, Female = 2))

# Keep only NSID and the sex variable
final_data <- cleaned %>% select(NSID, sex)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Final dataset has", nrow(final_data), "rows and", ncol(final_data), "columns\n")

# Print summary of sex variable
cat("\nDistribution of sex variable:\n")
print(table(factor(final_data$sex, levels = c(1, 2, -1, -2, -3, -7, -8, -9), 
                  labels = c("Male", "Female", "Not applicable", 
                             "Schedule not applicable", "Not asked / not interviewed",
                             "Prefer not to say", "Don't know", "Refused"))))
