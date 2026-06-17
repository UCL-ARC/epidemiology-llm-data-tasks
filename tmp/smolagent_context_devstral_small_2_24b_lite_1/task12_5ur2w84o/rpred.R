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

# Check if wave9 file exists and load it
if (file.exists("data/input/ns9_2022_main_interview.tab")) {
  wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
} else {
  wave9 <- NULL
}

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID")

# If wave9 exists and has NSID, merge it
if (!is.null(wave9) && "NSID" %in% names(wave9)) {
  merged_data <- merged_data %>%
    full_join(wave9, by = "NSID")
}

# Define a function to collapse NS-SEC categories to major groups
collapse_nssec <- function(var) {
  case_when(
    var %in% c(1.0, 2.0) ~ 1,  # Higher managerial and professional
    var %in% c(3.0, 3.1, 3.2, 3.3, 3.4, 4.0, 4.1, 4.2, 4.3, 4.4) ~ 2,  # Professional
    var %in% c(5.0, 6.0) ~ 3,  # Managerial and supervisory
    var %in% c(7.0, 7.1, 7.2, 7.3, 7.4) ~ 4,  # Intermediate
    var %in% c(8.0, 8.1, 8.2, 9.0, 9.1, 9.2) ~ 5,  # Small employers and own account
    var %in% c(10.0, 11.0, 11.1, 11.2) ~ 6,  # Lower supervisory and technical
    var %in% c(12.0, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7) ~ 7,  # Semi-routine
    var %in% c(13.0, 13.1, 13.2, 13.3, 13.4, 13.5) ~ 8,  # Routine
    var %in% c(14.0, 14.1, 14.2, 14.3) ~ 9,  # Never worked and unemployed
    var %in% c(15.0) ~ 10,  # Full-time students
    var %in% c(16.0, 17.0) ~ 11,  # Not classified
    TRUE ~ NA_real_
  )
}

# Apply the function to each wave-specific NS-SEC variable
merged_data <- merged_data %>%
  mutate(
    nssec17 = collapse_nssec(W4nsseccatYP),
    nssec18 = collapse_nssec(W5nsseccatYP),
    nssec19 = collapse_nssec(w6nsseccatYP),
    nssec20 = collapse_nssec(W7NSSECCat),
    nssec25 = collapse_nssec(W8DNSSEC17)
  )

# If wave9 exists and has W9NSSEC, derive nssec32
if (!is.null(wave9) && "W9NSSEC" %in% names(wave9)) {
  merged_data <- merged_data %>%
    mutate(nssec32 = collapse_nssec(W9NSSEC))
} else {
  merged_data$nssec32 <- NA
}

# Define standard missing-value codes
merged_data <- merged_data %>%
  mutate(
    across(c(nssec17, nssec18, nssec19, nssec20, nssec25, nssec32), 
           ~ case_when(
             is.na(.) ~ -3,
             . == -9 ~ -9,
             . == -8 ~ -8,
             . == -7 ~ -7,
             . == -1 ~ -1,
             TRUE ~ .
           )
    )
  )

# Select only the ID variable and the derived NS-SEC variables
cleaned_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write the cleaned data to a CSV file
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data file
"data/output/cleaned_data.csv"