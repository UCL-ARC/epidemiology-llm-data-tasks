library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Check if ns9_main file exists and load it
if (file.exists("data/input/ns9_2022_main_interview.tab")) {
  ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
} else {
  ns9_main <- NULL
}

# Merge datasets step by step
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID")

# Join ns9_main if it exists and has NSID column
if (!is.null(ns9_main) && "NSID" %in% colnames(ns9_main)) {
  merged_data <- merged_data %>%
    full_join(ns9_main, by = "NSID")
}

# Define mapping for NS-SEC categories to major groups
map_nssec_to_major <- function(var) {
  case_when(
    var %in% c(1.0, 2.0) ~ 1,  # Employers in large orgs / Higher managerial
    var %in% c(3.0, 3.1, 3.2, 3.3, 3.4) ~ 2,  # Higher professional
    var %in% c(4.0, 4.1, 4.2, 4.3, 4.4) ~ 3,  # Lower professional/higher technical
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
    var %in% c(16.0) ~ 15,  # Not classified/inadequately stated
    var %in% c(17.0) ~ 16,  # Not classifiable for other reasons
    TRUE ~ NA_real_
  )
}

# Harmonize missing values and map to major NS-SEC categories
merged_data <- merged_data %>%
  mutate(
    # Age 17 (Wave 4)
    nssec17 = case_when(
      W4nsseccatYP == -99 ~ -3,  # Not interviewed
      W4nsseccatYP == -91 ~ -1,  # Not applicable
      W4nsseccatYP %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable
      is.na(W4nsseccatYP) ~ -3,  # Not interviewed
      TRUE ~ map_nssec_to_major(W4nsseccatYP)
    ),
    # Age 18 (Wave 5)
    nssec18 = case_when(
      W5nsseccatYP == -91 ~ -1,  # Not applicable
      W5nsseccatYP %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable
      is.na(W5nsseccatYP) ~ -3,  # Not interviewed
      TRUE ~ map_nssec_to_major(W5nsseccatYP)
    ),
    # Age 19 (Wave 6)
    nssec19 = case_when(
      w6nsseccatYP == -91 ~ -1,  # Not applicable
      w6nsseccatYP %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable
      is.na(w6nsseccatYP) ~ -3,  # Not interviewed
      TRUE ~ map_nssec_to_major(w6nsseccatYP)
    ),
    # Age 20 (Wave 7)
    nssec20 = case_when(
      W7NSSECCat == -91 ~ -1,  # Not applicable
      W7NSSECCat %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable
      is.na(W7NSSECCat) ~ -3,  # Not interviewed
      TRUE ~ map_nssec_to_major(W7NSSECCat)
    ),
    # Age 25 (Wave 8)
    nssec25 = case_when(
      W8DNSSEC17 == -9 ~ -9,  # Refused
      W8DNSSEC17 == -8 ~ -8,  # Insufficient information
      W8DNSSEC17 == -1 ~ -1,  # Not applicable
      W8DNSSEC17 %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable
      is.na(W8DNSSEC17) ~ -3,  # Not interviewed
      TRUE ~ map_nssec_to_major(W8DNSSEC17)
    )
  )

# Check if W9NSSEC exists in the merged dataset
if ("W9NSSEC" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(
      nssec32 = case_when(
        W9NSSEC == -1 ~ -1,  # Not applicable
        W9NSSEC %in% c(-999, -998, -997, -995) ~ -2,  # Schedule not applicable
        is.na(W9NSSEC) ~ -3,  # Not interviewed
        TRUE ~ map_nssec_to_major(W9NSSEC)
      )
    )
} else {
  # If W9NSSEC is missing, create nssec32 as NA
  merged_data <- merged_data %>%
    mutate(nssec32 = -3)  # Not interviewed
}

# Select only NSID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return path to output
"data/output/cleaned_data.csv"