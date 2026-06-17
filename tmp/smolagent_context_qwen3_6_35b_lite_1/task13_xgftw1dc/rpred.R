library(dplyr)
library(readr)
library(tidyr)
library(labelled)

# Load all wave files
data_dir <- "data/input"
wave1 <- read_delim(file.path(data_dir, "wave_one_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)
wave2 <- read_delim(file.path(data_dir, "wave_two_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)
wave3 <- read_delim(file.path(data_dir, "wave_three_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(file.path(data_dir, "wave_four_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)
wave5 <- read_delim(file.path(data_dir, "wave_five_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)

# Merge all waves
data_full <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Function to recode NS-SEC detailed categories to major categories
recode_nssec_major <- function(x) {
  # Handle missing values first
  out <- rep(-1, length(x))
  
  # Identify valid numeric codes (positive values representing occupation categories)
  valid_mask <- !is.na(x) & x > 0 & x < 100
  
  out[valid_mask] <- sapply(x[valid_mask], function(v) {
    # Extract the integer part
    int_part <- floor(v)
    
    # Map to major categories based on NS-SEC structure
    # Major categories:
    # 1 = Employers/large orgs, Higher managerial/professional
    # 2 = Lower managerial, Higher supervisory
    # 3 = Intermediate, Lower supervisory, Lower technical
    # 4 = Employers small orgs, Own account
    # 5 = Semi-routine
    # 6 = Routine
    # 7 = Not in labour force (never worked, unemployed, students, etc.)
    
    if (int_part == 1 || int_part == 2 || (int_part >= 3 && int_part <= 4)) {
      return(1L)
    } else if (int_part == 5 || int_part == 6) {
      return(2L)
    } else if (int_part == 7 || int_part == 10 || int_part == 11) {
      return(3L)
    } else if (int_part == 8 || int_part == 9) {
      return(4L)
    } else if (int_part == 12) {
      return(5L)
    } else if (int_part == 13) {
      return(6L)
    } else if (int_part >= 14) {
      return(7L)
    } else {
      return(-1L)
    }
  })
  
  # Map specific missing codes
  missing_mask <- !is.na(x) & x <= 0
  out[missing_mask & x == -999] <- -3
  out[missing_mask & x == -99] <- -2
  out[missing_mask & x == -98] <- -2
  out[missing_mask & x == -94] <- -8
  
  return(out)
}

# Apply recoding for each wave
data_full$nssecma14 <- recode_nssec_major(data_full$W1nsseccatmum)
data_full$nssecpa14 <- recode_nssec_major(data_full$W1nsseccatdad)
data_full$nssecma15 <- recode_nssec_major(data_full$W2nsseccatmum)
data_full$nssecpa15 <- recode_nssec_major(data_full$W2nsseccatdad)
data_full$nssecma16 <- recode_nssec_major(data_full$W3cnsseccatmum)
data_full$nssecpa16 <- recode_nssec_major(data_full$W3cnsseccatdad)
data_full$nssecma17 <- recode_nssec_major(data_full$w4cnsseccatmum)
data_full$nssecpa17 <- recode_nssec_major(data_full$w4cnsseccatdad)
data_full$nssecma18 <- recode_nssec_major(data_full$w5Cnsseccatmum)
data_full$nssecpa18 <- recode_nssec_major(data_full$w5Cnsseccatdad)

# Keep only NSID and derived variables
output_df <- data_full %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Ensure output directory exists
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output
write_csv(output_df, "data/output/cleaned_data.csv")

# Print summary
cat("Output dimensions:", dim(output_df), "\n")
cat("Variables:", names(output_df), "\n")
cat("\nSample of output:\n")
print(head(output_df))
