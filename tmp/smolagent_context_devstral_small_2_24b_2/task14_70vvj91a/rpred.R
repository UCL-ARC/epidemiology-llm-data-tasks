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
merged <- w1 %>% 
  full_join(w2, by = "NSID") %>% 
  full_join(w3, by = "NSID") %>% 
  full_join(w4, by = "NSID") %>% 
  full_join(w5, by = "NSID") %>% 
  full_join(w6, by = "NSID") %>% 
  full_join(w7, by = "NSID") %>% 
  full_join(w8, by = "NSID") %>% 
  full_join(w9, by = "NSID")

# Define standard missing value codes
standard_missing <- c("-9" = "Refusal", 
                      "-8" = "Don't know / insufficient information", 
                      "-7" = "Prefer not to say", 
                      "-3" = "Not asked at the fieldwork stage / not interviewed", 
                      "-2" = "Schedule not applicable / script error / information lost", 
                      "-1" = "Item not applicable")

# Helper function to map missing values
map_missing <- function(x, wave) {
  if (wave %in% 1:7) {
    x <- na_if(x, -1)
    x <- ifelse(is.na(x), -8, x)
  } else if (wave %in% 8:9) {
    x <- ifelse(x == -1, -1, x)
  }
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(x == -91, -1, x)
  x <- ifelse(x == -999 | x == -998 | x == -997 | x == -995, -2, x)
  x <- ifelse(x == -99, -3, x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(is.na(x), -3, x)
  return(x)
}

# Detailed 8-category variables for sweeps 1-4
merged <- merged %>% 
  mutate(hownteen14 = map_missing(W1hous12HH, 1)) %>% 
  mutate(hownteen15 = map_missing(W2Hous12HH, 2)) %>% 
  mutate(hownteen16 = map_missing(W3hous12HH, 3)) %>% 
  mutate(hownteen17 = map_missing(W4Hous12HH, 4))

# Collapsed 6-category variables for sweeps 1-4
merged <- merged %>% 
  mutate(hown14 = case_when(
    hownteen14 %in% 4:6 ~ 4,  # Rent it
    hownteen14 == 8 ~ 6,      # Other
    TRUE ~ hownteen14
  )) %>% 
  mutate(hown15 = case_when(
    hownteen15 %in% 4:6 ~ 4,  # Rent it
    hownteen15 == 8 ~ 6,      # Other
    TRUE ~ hownteen15
  )) %>% 
  mutate(hown16 = case_when(
    hownteen16 %in% 4:6 ~ 4,  # Rent it
    hownteen16 == 8 ~ 6,      # Other
    TRUE ~ hownteen16
  )) %>% 
  mutate(hown17 = case_when(
    hownteen17 %in% 4:6 ~ 4,  # Rent it
    hownteen17 == 8 ~ 6,      # Other
    TRUE ~ hownteen17
  ))

# Sweeps 5-7: Detailed and collapsed variables
# Helper function for sweeps 5-7
derive_tenure_5_7 <- function(tenure_type, owned_subtype, rented_subtype, wave) {
  # Map missing values first
  tenure_type <- map_missing(tenure_type, wave)
  owned_subtype <- map_missing(owned_subtype, wave)
  rented_subtype <- map_missing(rented_subtype, wave)

  # Priority: owned_subtype > rented_subtype > tenure_type
  result <- ifelse(!is.na(owned_subtype) & owned_subtype > 0, owned_subtype, 
                   ifelse(!is.na(rented_subtype) & rented_subtype > 0, rented_subtype, 
                          ifelse(!is.na(tenure_type) & tenure_type == 3, 8, 
                                 ifelse(!is.na(tenure_type) & tenure_type > 0, tenure_type, 
                                        ifelse(!is.na(owned_subtype), owned_subtype, 
                                               ifelse(!is.na(rented_subtype), rented_subtype, 
                                                      tenure_type))))))
  return(result)
}

# Sweep 5
merged <- merged %>% 
  mutate(hownteen18 = derive_tenure_5_7(W5Hous12HH, W5Hous12BHH, W5Hous12CHH, 5))

# Sweep 6
merged <- merged %>% 
  mutate(hownteen19 = derive_tenure_5_7(W6Hous12YP, W6Hous12bYP, W6Hous12cYP, 6))

# Sweep 7
merged <- merged %>% 
  mutate(hownteen20 = derive_tenure_5_7(W7Hous12YP, W7Hous12bYP, W7Hous12cYP, 7))

# Collapsed variables for sweeps 5-7
merged <- merged %>% 
  mutate(hown18 = case_when(
    hownteen18 %in% 1:3 ~ 3,  # Some other arrangement
    hownteen18 %in% 4:6 ~ 4,  # Rent it
    hownteen18 == 8 ~ 6,      # Other
    TRUE ~ hownteen18
  )) %>% 
  mutate(hown19 = case_when(
    hownteen19 %in% 1:3 ~ 3,  # Some other arrangement
    hownteen19 %in% 4:6 ~ 4,  # Rent it
    hownteen19 == 8 ~ 6,      # Other
    TRUE ~ hownteen19
  )) %>% 
  mutate(hown20 = case_when(
    hownteen20 %in% 1:3 ~ 3,  # Some other arrangement
    hownteen20 %in% 4:6 ~ 4,  # Rent it
    hownteen20 == 8 ~ 6,      # Other
    TRUE ~ hownteen20
  ))

# Sweeps 8-9: Detailed and collapsed variables
# Helper function for sweeps 8-9
derive_tenure_8_9 <- function(tenure, wave) {
  tenure <- map_missing(tenure, wave)
  return(tenure)
}

# Sweep 8
merged <- merged %>% 
  mutate(hownteen25 = derive_tenure_8_9(W8TENURE, 8))

# Sweep 9
merged <- merged %>% 
  mutate(hownteen32 = derive_tenure_8_9(W9DTENURE, 9))

# Collapsed variables for sweeps 8-9
merged <- merged %>% 
  mutate(hown25 = case_when(
    hownteen25 == 6 ~ 6,  # Squatting
    hownteen25 == 7 ~ 6,  # Other
    TRUE ~ hownteen25
  )) %>% 
  mutate(hown32 = case_when(
    hownteen32 == 6 ~ 6,  # Squatting
    hownteen32 == 7 ~ 6,  # Other
    TRUE ~ hownteen32
  ))

# Select only the final derived variables and NSID
output <- merged %>% 
  select(NSID, 
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20, 
         hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write output
write_csv(output, "data/output/cleaned_data.csv")
