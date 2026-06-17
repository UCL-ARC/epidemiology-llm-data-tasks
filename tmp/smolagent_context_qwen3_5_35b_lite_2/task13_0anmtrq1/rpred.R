library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files from the metadata
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

cat("Loaded files successfully\n")

# Merge all files by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave3, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")
merged <- full_join(merged, wave5, by = "NSID")

cat("Merged dataset:", nrow(merged), "rows,", ncol(merged), "cols\n")

# Function to collapse NS-SEC to major categories
collapse_nssec <- function(x) {
  result <- x
  
  # Map substantive categories to major codes
  result[result %in% c(1.0, 2.0, 3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4)] <- 1
  result[result %in% c(5.0, 6.0, 7.1, 7.2, 7.3, 7.4, 8.1, 8.2, 9.1, 9.2)] <- 2
  result[result %in% c(10.0, 11.1, 11.2, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7, 13.1, 13.2, 13.3, 13.4, 13.5)] <- 3
  
  # Map non-working categories
  result[result %in% c(14.1, 14.2, 14.3)] <- 4
  result[result == 15.0] <- 5
  
  result
}

# Function to convert missing codes to standard codes
convert_missing_codes <- function(x) {
  result <- x
  # -999: Missing data -> -2 (Not applicable)
  # -99: Not interviewed -> -9 (Refusal)
  # -98: Not present -> -1 (Not applicable)
  # -94: Insufficient information -> -8 (Don't know)
  result[result == -999] <- -2
  result[result == -99] <- -9
  result[result == -98] <- -1
  result[result == -94] <- -8
  
  # Convert NA to -3 (Not asked/not interviewed)
  result[is.na(result)] <- -3
  
  result
}

# Create functions for each wave's mother/father variables
process_parent_nssec <- function(mom_var, dad_var, age) {
  # Collapse to major categories first
  mom_collapsed <- collapse_nssec(mom_var)
  dad_collapsed <- collapse_nssec(dad_var)
  
  # Convert missing codes
  mom_converted <- convert_missing_codes(mom_collapsed)
  dad_converted <- convert_missing_codes(dad_collapsed)
  
  # Create factors with proper labels
  mom_out <- factor(mom_converted, levels = c(1, 2, 3, 4, 5, -1, -2, -3, -8, -9), 
                    labels = c("1", "2", "3", "4", "5", "-1", "-2", "-3", "-8", "-9"))
  dad_out <- factor(dad_converted, levels = c(1, 2, 3, 4, 5, -1, -2, -3, -8, -9), 
                    labels = c("1", "2", "3", "4", "5", "-1", "-2", "-3", "-8", "-9"))
  
  # Add value labels using attr
  labels_vec <- c("1" = "Managers and professionals", 
                 "2" = "Intermediate", 
                 "3" = "Routine and manual", 
                 "4" = "Never worked/long-term unemployed", 
                 "5" = "Full-time students",
                 "-1" = "Not applicable",
                 "-2" = "Not applicable",
                 "-3" = "Not asked",
                 "-8" = "Don't know",
                 "-9" = "Refused")
  
  attr(mom_out, "labels") <- labels_vec
  attr(dad_out, "labels") <- labels_vec
  
  mom_var_name <- paste0("nssecma", age)
  dad_var_name <- paste0("nssecpa", age)
  
  list(mom = mom_out, dad = dad_out, mom_name = mom_var_name, dad_name = dad_var_name)
}

# Process each wave
wave1_vars <- process_parent_nssec(wave1$W1nsseccatmum, wave1$W1nsseccatdad, 14)
wave2_vars <- process_parent_nssec(wave2$W2nsseccatmum, wave2$W2nsseccatdad, 15)
wave3_vars <- process_parent_nssec(wave3$W3cnsseccatmum, wave3$W3cnsseccatdad, 16)
wave4_vars <- process_parent_nssec(wave4$w4cnsseccatmum, wave4$w4cnsseccatdad, 17)
wave5_vars <- process_parent_nssec(wave5$w5Cnsseccatmum, wave5$w5Cnsseccatdad, 18)

# Select only the necessary columns from each wave
wave1_clean <- select(wave1, NSID)
wave2_clean <- select(wave2, NSID)
wave3_clean <- select(wave3, NSID)
wave4_clean <- select(wave4, NSID)
wave5_clean <- select(wave5, NSID)

# Add the processed variables
wave1_clean <- mutate(wave1_clean, 
                      !!wave1_vars$mom_name := wave1_vars$mom,
                      !!wave1_vars$dad_name := wave1_vars$dad)

wave2_clean <- mutate(wave2_clean,
                      !!wave2_vars$mom_name := wave2_vars$mom,
                      !!wave2_vars$dad_name := wave2_vars$dad)

wave3_clean <- mutate(wave3_clean,
                      !!wave3_vars$mom_name := wave3_vars$mom,
                      !!wave3_vars$dad_name := wave3_vars$dad)

wave4_clean <- mutate(wave4_clean,
                      !!wave4_vars$mom_name := wave4_vars$mom,
                      !!wave4_vars$dad_name := wave4_vars$dad)

wave5_clean <- mutate(wave5_clean,
                      !!wave5_vars$mom_name := wave5_vars$mom,
                      !!wave5_vars$dad_name := wave5_vars$dad)

# Merge all cleaned waves
final_data <- full_join(wave1_clean, wave2_clean, by = "NSID")
final_data <- full_join(final_data, wave3_clean, by = "NSID")
final_data <- full_join(final_data, wave4_clean, by = "NSID")
final_data <- full_join(final_data, wave5_clean, by = "NSID")

# Ensure NSID is first column
final_data <- select(final_data, NSID, everything())

cat("Final dataset:", nrow(final_data), "rows,", ncol(final_data), "cols\n")
cat("Variable names:", paste(names(final_data), collapse = ", "), "\n")

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")
cat("Output written to data/output/cleaned_data.csv\n")
