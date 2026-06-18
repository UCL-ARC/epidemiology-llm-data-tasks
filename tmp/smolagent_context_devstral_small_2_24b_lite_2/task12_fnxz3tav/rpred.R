library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Check if wave9 exists and load it
wave9_file <- "data/input/ns9_2022_main_interview.tab"
if (file.exists(wave9_file)) {
  wave9 <- read_delim(wave9_file, delim = "\t")
} else {
  wave9 <- NULL
}

# Merge datasets step by step
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID")

# Merge wave9 if it exists and has NSID column
if (!is.null(wave9) && "NSID" %in% colnames(wave9)) {
  merged_data <- merged_data %>%
    full_join(wave9, by = "NSID")
}

# Define a function to collapse NS-SEC categories to major groups
collapse_nssec <- function(var) {
  case_when(
    var %in% c(1.0, 2.0) ~ 1,  # Employers in large orgs & Higher managerial
    var %in% c(3.0, 3.1, 3.2, 3.3, 3.4) ~ 2,  # Higher professional
    var %in% c(4.0, 4.1, 4.2, 4.3, 4.4) ~ 3,  # Lower professional
    var %in% c(5.0) ~ 4,  # Lower managerial
    var %in% c(6.0) ~ 5,  # Higher supervisory
    var %in% c(7.0, 7.1, 7.2, 7.3, 7.4) ~ 6,  # Intermediate
    var %in% c(8.0, 8.1, 8.2) ~ 7,  # Employers in small orgs
    var %in% c(9.0, 9.1, 9.2) ~ 8,  # Own account workers
    var %in% c(10.0) ~ 9,  # Lower supervisory
    var %in% c(11.0, 11.1, 11.2) ~ 10,  # Lower technical
    var %in% c(12.0, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7) ~ 11,  # Semi-routine
    var %in% c(13.0, 13.1, 13.2, 13.3, 13.4, 13.5) ~ 12,  # Routine
    var %in% c(14.0, 14.1, 14.2, 14.3) ~ 13,  # Never worked/long-term unemployed
    var %in% c(15.0) ~ 14,  # Full-time students
    var %in% c(16.0, 17.0) ~ 15,  # Not classified/other reasons
    TRUE ~ NA_real_
  )
}

# Apply the collapse function to each wave's NS-SEC variable
merged_data <- merged_data %>%
  mutate(
    nssec17 = collapse_nssec(W4nsseccatYP),
    nssec18 = collapse_nssec(W5nsseccatYP),
    nssec19 = collapse_nssec(w6nsseccatYP),
    nssec20 = collapse_nssec(W7NSSECCat),
    nssec25 = collapse_nssec(W8DNSSEC17)
  )

# Handle nssec32 separately since wave9 may not exist
if (!is.null(wave9) && "W9NSSEC" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec32 = collapse_nssec(W9NSSEC))
} else {
  merged_data$nssec32 <- -3  # Default to -3 if wave9 is missing
}

# Convert NA to -3 (not interviewed/not applicable)
merged_data <- merged_data %>%
  mutate(across(c(nssec17, nssec18, nssec19, nssec20, nssec25, nssec32), ~ ifelse(is.na(.), -3, .)))

# Select only NSID and the derived NS-SEC variables
output_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write the output
write_csv(output_data, "data/output/cleaned_data.csv")

# Print confirmation
cat("Output file generated successfully.\n")