library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the 17 major NS-SEC category labels as a named vector
major_labels <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "-3"
)

major_values <- c(
  "Employers in large organisations",
  "Higher managerial occupations",
  "Higher professional occupations",
  "Lower professional occupations",
  "Lower managerial occupations",
  "Higher supervisory occupations",
  "Intermediate occupations",
  "Employers in small orgs",
  "Own account workers",
  "Lower supervisory occupations",
  "Lower technical occupations",
  "Semi routine occupations",
  "Routine occupations",
  "Never worked/Long-term unemployed",
  "Full-time students",
  "Not classified or inadequately stated",
  "Not classifiable for other reasons",
  "Not asked at fieldwork"
)

nssec_labels <- setNames(major_values, major_labels)

# Function to extract major category from fractional code
extract_nssec_major <- function(x) {
  result <- as.numeric(x)
  
  # Convert -98 (Parent not present) to -3 (Not asked at fieldwork)
  result[result == -98] <- -3
  
  # For valid values (positive), extract integer part
  valid_mask <- result > 0 & !is.na(result)
  result[valid_mask] <- floor(result[valid_mask])
  
  # Convert to character for labelled function
  result <- as.character(result)
  result <- labelled(result, nssec_labels)
  
  return(result)
}

# Load all 5 wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID
cleaned <- full_join(wave1, wave2, by = "NSID")
cleaned <- full_join(cleaned, wave3, by = "NSID")
cleaned <- full_join(cleaned, wave4, by = "NSID")
cleaned <- full_join(cleaned, wave5, by = "NSID")

# Derive NS-SEC variables for each wave by transforming the original columns in the merged dataframe
cleaned$nssecma14 <- extract_nssec_major(cleaned$W1nsseccatmum)
cleaned$nssecpa14 <- extract_nssec_major(cleaned$W1nsseccatdad)
cleaned$nssecma15 <- extract_nssec_major(cleaned$W2nsseccatmum)
cleaned$nssecpa15 <- extract_nssec_major(cleaned$W2nsseccatdad)
cleaned$nssecma16 <- extract_nssec_major(cleaned$W3cnsseccatmum)
cleaned$nssecpa16 <- extract_nssec_major(cleaned$W3cnsseccatdad)
cleaned$nssecma17 <- extract_nssec_major(cleaned$w4cnsseccatmum)
cleaned$nssecpa17 <- extract_nssec_major(cleaned$w4cnsseccatdad)
cleaned$nssecma18 <- extract_nssec_major(cleaned$w5Cnsseccatmum)
cleaned$nssecpa18 <- extract_nssec_major(cleaned$w5Cnsseccatdad)

# Select only ID and derived variables
output <- cleaned %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Successfully created cleaned_data.csv with", nrow(output), "records and", ncol(output), "variables\n")
