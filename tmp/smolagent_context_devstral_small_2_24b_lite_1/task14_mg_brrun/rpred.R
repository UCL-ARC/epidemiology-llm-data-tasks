library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, labels) {
  var <- as.numeric(var)
  mapped_var <- case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refused
    var %in% c(-8, -8.0, -1, -1.0, -94, -94.0) ~ -8,  # Don't know / insufficient information
    var %in% c(-7, -7.0) ~ -7,  # Prefer not to say
    var %in% c(-999, -999.0, -998, -998.0, -997, -997.0, -995, -995.0, -99, -99.0, -2, -2.0) ~ -2,  # Schedule not applicable / script error / information lost
    var %in% c(-91, -91.0) ~ -1,  # Item not applicable
    TRUE ~ var
  )
  return(mapped_var)
}

# Function to harmonize housing tenure categories
harmonize_tenure <- function(var, wave) {
  var <- as.numeric(var)
  
  # Map missing values first
  var <- map_missing(var, NULL)
  
  # Harmonize categories based on wave-specific labels
  if (wave == "wave1" | wave == "wave2" | wave == "wave3" | wave == "wave4") {
    # Waves 1-4 have detailed categories
    var <- case_when(
      var %in% c(1, 1.0) ~ 1,  # Owned outright
      var %in% c(2, 2.0) ~ 2,  # Being bought on a mortgage/loan
      var %in% c(3, 3.0) ~ 3,  # Shared ownership
      var %in% c(4, 4.0, 5, 5.0, 6, 6.0) ~ 4,  # Rent it (collapsed)
      var %in% c(7, 7.0) ~ 5,  # Rent free
      var %in% c(8, 8.0) ~ 6,  # Some other arrangement
      TRUE ~ var
    )
  } else if (wave == "wave6" | wave == "wave7") {
    # Waves 6-7 have simplified categories
    var <- case_when(
      var %in% c(1, 1.0) ~ 1,  # Owned
      var %in% c(2, 2.0) ~ 4,  # Rented
      var %in% c(3, 3.0) ~ 6,  # Something else
      TRUE ~ var
    )
  } else if (wave == "wave8") {
    # Wave 8 has specific categories
    var <- case_when(
      var %in% c(1, 1.0) ~ 1,  # Own – outright
      var %in% c(2, 2.0) ~ 2,  # Own – buying with help of mortg or loan
      var %in% c(3, 3.0) ~ 3,  # Part rent/mortgage (shared/equity own)
      var %in% c(4, 4.0, 5, 5.0) ~ 4,  # Rent it (collapsed)
      var %in% c(6, 6.0) ~ 6,  # Squatting
      var %in% c(7, 7.0) ~ 6,  # Other arrangement
      TRUE ~ var
    )
  } else if (wave == "wave9") {
    # Wave 9 has specific categories
    var <- case_when(
      var %in% c(1, 1.0) ~ 1,  # Own outright
      var %in% c(2, 2.0) ~ 2,  # Own, buying with help of mortgage/loan
      var %in% c(3, 3.0) ~ 3,  # Part rent, part mortgage (shared equity)
      var %in% c(4, 4.0) ~ 4,  # Rent it
      var %in% c(5, 5.0) ~ 5,  # Live rent-free, incl. relatives/friends
      var %in% c(6, 6.0) ~ 6,  # Squatting
      var %in% c(7, 7.0) ~ 6,  # Other
      TRUE ~ var
    )
  }
  
  return(var)
}

# Create detailed time-varying housing tenure variables (ages 14-20)
merged_data <- merged_data %>%
  mutate(
    hownteen14 = harmonize_tenure(W1hous12HH, "wave1"),
    hownteen15 = harmonize_tenure(W2Hous12HH, "wave2"),
    hownteen16 = harmonize_tenure(W3hous12HH, "wave3"),
    hownteen17 = harmonize_tenure(W4Hous12HH, "wave4"),
    hownteen19 = harmonize_tenure(W6Hous12YP, "wave6"),
    hownteen20 = harmonize_tenure(W7Hous12YP, "wave7")
  )

# Create collapsed time-varying housing tenure variables (ages 14-32)
merged_data <- merged_data %>%
  mutate(
    hown14 = harmonize_tenure(W1hous12HH, "wave1"),
    hown15 = harmonize_tenure(W2Hous12HH, "wave2"),
    hown16 = harmonize_tenure(W3hous12HH, "wave3"),
    hown17 = harmonize_tenure(W4Hous12HH, "wave4"),
    hown19 = harmonize_tenure(W6Hous12YP, "wave6"),
    hown20 = harmonize_tenure(W7Hous12YP, "wave7"),
    hown25 = harmonize_tenure(W8TENURE, "wave8"),
    hown32 = harmonize_tenure(W9DTENURE, "wave9")
  )

# Select only the ID variable and final derived variables
final_data <- merged_data %>%
  select(NSID, starts_with("hownteen"), starts_with("hown"))

# Write the output CSV
write_csv(final_data, "data/output/cleaned_data.csv")

print("Cleaned data has been written to data/output/cleaned_data.csv")