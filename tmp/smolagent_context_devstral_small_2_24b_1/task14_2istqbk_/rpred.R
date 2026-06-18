library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
w5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- w1 %>% 
  full_join(w2, by = "NSID") %>% 
  full_join(w3, by = "NSID") %>% 
  full_join(w4, by = "NSID") %>% 
  full_join(w5, by = "NSID") %>% 
  full_join(w6, by = "NSID") %>% 
  full_join(w7, by = "NSID") %>% 
  full_join(w8, by = "NSID") %>% 
  full_join(w9, by = "NSID")

# Define standard missing-value codes
standard_missing <- c(
  `-9` = "Refusal",
  `-8` = "Don't know / insufficient information",
  `-7` = "Prefer not to say",
  `-3` = "Not asked at the fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable"
)

# Helper function to map missing values
map_missing <- function(var, wave) {
  if (wave %in% c("w1", "w2", "w3", "w4", "w5", "w6", "w7")) {
    var <- ifelse(var == -1, -8, var)
  }
  var <- ifelse(var %in% c(-999, -998, -997, -995), -2, var)
  var <- ifelse(var %in% c(-94), -8, var)
  var <- ifelse(var == -92, -9, var)
  var <- ifelse(var == -91, -1, var)
  var <- ifelse(var == -99, -3, var)
  var <- ifelse(var == -100, -3, var)
  var <- ifelse(var == -97, -3, var)
  var <- ifelse(is.na(var), -3, var)
  return(var)
}

# Detailed 8-category variables for sweeps 1-4
detailed_labels <- c(
  `1` = "Owned outright",
  `2` = "Being bought on a mortgage/ bank loan",
  `3` = "Shared ownership (owns & rents property)",
  `4` = "Rented from a Council or New Town",
  `5` = "Rented from a Housing Association",
  `6` = "Rented privately",
  `7` = "Rent free",
  `8` = "Some other arrangement"
)

merged_data$hownteen14 <- map_missing(merged_data$W1hous12HH, "w1")
merged_data$hownteen15 <- map_missing(merged_data$W2Hous12HH, "w2")
merged_data$hownteen16 <- map_missing(merged_data$W3hous12HH, "w3")
merged_data$hownteen17 <- map_missing(merged_data$W4Hous12HH, "w4")

# Collapsed 6-category variables for sweeps 1-4
collapsed_labels <- c(
  `1` = "Owned outright",
  `2` = "Being bought on a mortgage/ bank loan",
  `3` = "Shared ownership (owns & rents property)",
  `4` = "Rent it",
  `5` = "Rent free",
  `6` = "Other"
)

merged_data$hown14 <- ifelse(merged_data$hownteen14 %in% c(4, 5, 6), 4, merged_data$hownteen14)
merged_data$hown14 <- ifelse(merged_data$hownteen14 == 8, 6, merged_data$hown14)
merged_data$hown15 <- ifelse(merged_data$hownteen15 %in% c(4, 5, 6), 4, merged_data$hownteen15)
merged_data$hown15 <- ifelse(merged_data$hownteen15 == 8, 6, merged_data$hown15)
merged_data$hown16 <- ifelse(merged_data$hownteen16 %in% c(4, 5, 6), 4, merged_data$hownteen16)
merged_data$hown16 <- ifelse(merged_data$hownteen16 == 8, 6, merged_data$hown16)
merged_data$hown17 <- ifelse(merged_data$hownteen17 %in% c(4, 5, 6), 4, merged_data$hownteen17)
merged_data$hown17 <- ifelse(merged_data$hownteen17 == 8, 6, merged_data$hown17)

# Sweeps 5-7: Derive from multiple variables
# Helper function for sweeps 5-7
derive_sweep57 <- function(tenure_type, owned_subtype, rented_subtype, wave) {
  # Initialize result with missing
  result <- rep(-3, length(tenure_type))
  
  # Map missing values for subtypes
  owned_subtype <- map_missing(owned_subtype, wave)
  rented_subtype <- map_missing(rented_subtype, wave)
  
  # Priority: owned_subtype > rented_subtype
  # Owned subtypes
  result[owned_subtype %in% c(1, 2, 3)] <- owned_subtype[owned_subtype %in% c(1, 2, 3)]
  result[owned_subtype == 4] <- 8
  
  # Rented subtypes
  result[rented_subtype %in% c(1, 2, 3)] <- 4
  result[rented_subtype == 4] <- 5
  result[rented_subtype == 5] <- 8
  
  # If no substantive path, preserve missing from subtypes
  result[result == -3] <- ifelse(owned_subtype[result == -3] %in% c(-9, -8, -7, -3, -2, -1), owned_subtype[result == -3], rented_subtype[result == -3])
  
  return(result)
}

# Sweep 5
merged_data$hownteen18 <- derive_sweep57(merged_data$W5Hous12HH, merged_data$W5Hous12BHH, merged_data$W5Hous12CHH, "w5")
merged_data$hown18 <- ifelse(merged_data$hownteen18 %in% c(4, 5, 6), 4, merged_data$hownteen18)
merged_data$hown18 <- ifelse(merged_data$hownteen18 == 8, 6, merged_data$hown18)

# Sweep 6
merged_data$hownteen19 <- derive_sweep57(merged_data$W6Hous12YP, merged_data$W6Hous12bYP, merged_data$W6Hous12cYP, "w6")
merged_data$hown19 <- ifelse(merged_data$hownteen19 %in% c(4, 5, 6), 4, merged_data$hownteen19)
merged_data$hown19 <- ifelse(merged_data$hownteen19 == 8, 6, merged_data$hown19)

# Sweep 7
merged_data$hownteen20 <- derive_sweep57(merged_data$W7Hous12YP, merged_data$W7Hous12bYP, merged_data$W7Hous12cYP, "w7")
merged_data$hown20 <- ifelse(merged_data$hownteen20 %in% c(4, 5, 6), 4, merged_data$hownteen20)
merged_data$hown20 <- ifelse(merged_data$hownteen20 == 8, 6, merged_data$hown20)

# Sweeps 8-9: Single source variable
# Sweep 8
merged_data$hown25 <- map_missing(merged_data$W8TENURE, "w8")
merged_data$hown25 <- ifelse(merged_data$hown25 == 6, 6, merged_data$hown25)
merged_data$hown25 <- ifelse(merged_data$hown25 == 7, 6, merged_data$hown25)

# Sweep 9
merged_data$hown32 <- map_missing(merged_data$W9DTENURE, "w9")
merged_data$hown32 <- ifelse(merged_data$hown32 == 6, 6, merged_data$hown32)
merged_data$hown32 <- ifelse(merged_data$hown32 == 7, 6, merged_data$hown32)

# Select only final derived variables and NSID
output_data <- merged_data %>% 
  select(NSID, 
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20,
         hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
