library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave5 = "data/input/wave_five_lsype_young_person_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_derived.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab"
)

# Load all files
data_wave1 <- read_delim(files$wave1, delim = "\t", show_col_types = FALSE)
data_wave4 <- read_delim(files$wave4, delim = "\t", show_col_types = FALSE)
data_wave5 <- read_delim(files$wave5, delim = "\t", show_col_types = FALSE)
data_wave6 <- read_delim(files$wave6, delim = "\t", show_col_types = FALSE)
data_wave7 <- read_delim(files$wave7, delim = "\t", show_col_types = FALSE)
data_wave8 <- read_delim(files$wave8, delim = "\t", show_col_types = FALSE)
data_wave9 <- read_delim(files$wave9, delim = "\t", show_col_types = FALSE)

# Merge all data by NSID
cleaned_data <- full_join(data_wave1, data_wave4, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_wave5, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_wave6, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_wave7, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_wave8, by = "NSID")
cleaned_data <- full_join(cleaned_data, data_wave9, by = "NSID")

# Function to extract major category from fractional codes
extract_major_category <- function(x) {
  # Handle NA/missing values
  is_missing <- is.na(x) | x %in% c(-9, -8, -7, -3, -2, -1, -999, -998, -997, -995, -94, -92, -91, -99, -100, -97)
  
  # For non-missing values, extract integer part
  result <- rep(NA_real_, length(x))
  result[!is_missing] <- floor(x[!is_missing])
  result
}

# Map missing codes to standard scheme
map_missing_codes <- function(x) {
  # Map various missing codes to standard scheme
  result <- x
  
  # Refusal (-9)
  result[result == -9] <- -9
  result[result == -999] <- -9
  result[result == -998] <- -9
  result[result == -997] <- -9
  result[result == -92] <- -9
  
  # Don't know/insufficient information (-8)
  result[result == -8] <- -8
  result[result == -995] <- -8
  
  # Prefer not to say (-7)
  result[result == -7] <- -7
  
  # Not asked/not interviewed (-3)
  result[result == -3] <- -3
  result[result == -99] <- -3
  
  # Schedule not applicable/script error (-2)
  result[result == -2] <- -2
  result[result == -999] <- -2
  
  # Item not applicable (-1)
  result[result == -1] <- -1
  result[result == -91] <- -1
  result[result == -100] <- -1
  result[result == -97] <- -1
  
  # Convert remaining NA to -3 (not asked)
  result[is.na(result)] <- -3
  
  result
}

# Create nssec17 from W4nsseccatYP
cleaned_data$nssec17 <- extract_major_category(cleaned_data$W4nsseccatYP)
cleaned_data$nssec17 <- map_missing_codes(cleaned_data$nssec17)

# Create nssec18 from W5nsseccatYP
cleaned_data$nssec18 <- extract_major_category(cleaned_data$W5nsseccatYP)
cleaned_data$nssec18 <- map_missing_codes(cleaned_data$nssec18)

# Create nssec19 from w6nsseccatYP
cleaned_data$nssec19 <- extract_major_category(cleaned_data$w6nsseccatYP)
cleaned_data$nssec19 <- map_missing_codes(cleaned_data$nssec19)

# Create nssec20 from W7NSSECCat
cleaned_data$nssec20 <- extract_major_category(cleaned_data$W7NSSECCat)
cleaned_data$nssec20 <- map_missing_codes(cleaned_data$nssec20)

# Create nssec25 from W8DNSSEC17 with special handling for full-time students
cleaned_data$nssec25 <- extract_major_category(cleaned_data$W8DNSSEC17)

# Special rule for nssec25: if W8DACTIVITYC == 5 (Full-time education), assign 15
is_fulltime_edu <- cleaned_data$W8DACTIVITYC == 5
is_nssec_missing <- is.na(cleaned_data$nssec25) | cleaned_data$nssec25 %in% c(-9, -8, -7, -3, -2, -1)

# Apply the special rule
cleaned_data$nssec25[is_fulltime_edu] <- 15

# Map missing codes for nssec25
cleaned_data$nssec25 <- map_missing_codes(cleaned_data$nssec25)

# Create nssec32 from W9NSSEC
cleaned_data$nssec32 <- extract_major_category(cleaned_data$W9NSSEC)
cleaned_data$nssec32 <- map_missing_codes(cleaned_data$nssec32)

# Remove raw source variables
cleaned_data <- cleaned_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Cleaned data written to data/output/cleaned_data.csv\n")
cat("Dimensions:", dim(cleaned_data), "\n")
