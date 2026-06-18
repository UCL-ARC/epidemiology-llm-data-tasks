library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
cleaned <- full_join(wave1, wave4, by = "NSID")
cleaned <- full_join(cleaned, ns8, by = "NSID")
cleaned <- full_join(cleaned, ns9, by = "NSID")

# Function to harmonise BMI variables
harmonise_bmi <- function(x, name) {
  # Keep positive values as-is (continuous BMI)
  # Map negative user-missing values to standard codes
  # -9 = Refusal, -8 = Insufficient information, -1 = Not applicable
  # Map R NA to -3 (Not asked/not interviewed)
  
  result <- as.numeric(x)
  
  # Map NAs to -3 (not asked)
  result[is.na(result)] <- -3
  
  # Set variable label
  var_label(result) <- name
  
  return(result)
}

# Create bmi25 from W8DBMI
cleaned$bmi25 <- harmonise_bmi(cleaned$W8DBMI, "Body mass index at age 25")

# Create bmi32 from W9DBMI
cleaned$bmi32 <- harmonise_bmi(cleaned$W9DBMI, "Body mass index at age 32")

# Keep only NSID and the final derived variables
output <- cleaned %>% select(NSID, bmi25, bmi32)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("Columns:", paste(names(output), collapse = ", "), "\n")

# Show summary
print(summary(output))
