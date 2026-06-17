library(readr)
library(dplyr)
library(haven)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = "\t", show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = "\t", show_col_types = FALSE)
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = "\t", show_col_types = FALSE)
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
clean_data <- full_join(wave1, wave4, by = "NSID")
clean_data <- full_join(clean_data, ns8, by = "NSID")
clean_data <- full_join(clean_data, ns9, by = "NSID")

# Function to clean BMI values
clean_bmi <- function(bmi_var) {
  # Replace negative values with -3 (not applicable)
  bmi_var[bmi_var < 0] <- -3
  # Replace NA with -3
  bmi_var[is.na(bmi_var)] <- -3
  return(bmi_var)
}

# Create bmi25 (from W8DBMI at age 25)
clean_data$bmi25 <- clean_bmi(clean_data$W8DBMI)

# Create bmi32 (from W9DBMI at age 32)
clean_data$bmi32 <- clean_bmi(clean_data$W9DBMI)

# Select only ID and final derived variables
output_data <- clean_data %>%
  select(NSID, bmi25, bmi32)

# Write output
write_csv(output_data, 'data/output/cleaned_data.csv')

cat("Output written successfully\n")
