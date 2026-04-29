# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define the input and output directories
input_dir <- "data/input/"
output_dir <- "data/output/"

# Load each dataset
wave_one <- readr::read_delim(file.path(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t")
wave_two <- readr::read_delim(file.path(input_dir, "wave_two_lsype_family_background_2020.tab"), delim = "\t")
wave_three <- readr::read_delim(file.path(input_dir, "wave_three_lsype_family_background_2020.tab"), delim = "\t")
wave_four <- readr::read_delim(file.path(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t")
ns9_2022 <- readr::read_delim(file.path(input_dir, "ns9_2022_derived_variables.tab"), delim = "\t")

# Select and rename deprivation score variables from each dataset
wave_two_imd15 <- wave_two %>% select(NSID, IMDRSCORE) %>% rename(imd15 = IMDRSCORE)
wave_three_imd16 <- wave_three %>% select(NSID, IMDRSCORE) %>% rename(imd16 = IMDRSCORE)
ns9_2022_imd32 <- ns9_2022 %>% select(NSID, W9DIMDD) %>% rename(imd32 = W9DIMDD)

# Merge datasets using full_join by NSID
cleaned_data <- wave_two_imd15 %>% full_join(wave_three_imd16, by = "NSID") %>% full_join(ns9_2022_imd32, by = "NSID")

# Recode missing values
cleaned_data <- cleaned_data %>% mutate(
  imd15 = case_when(imd15 == -94 ~ -8, is.na(imd15) ~ -3, TRUE ~ imd15),
  imd16 = case_when(imd16 == -94 ~ -8, is.na(imd16) ~ -3, TRUE ~ imd16),
  imd32 = case_when(imd32 == -94 ~ -8, is.na(imd32) ~ -3, TRUE ~ imd32)
)

# Create value labels for missing values
missing_labels <- c(-9, -8, -3, -2, -1)
names(missing_labels) <- c("Refusal", "Don't know / insufficient information", 
                          "Not asked at the fieldwork stage / not interviewed", 
                          "Script error / information lost", 
                          "Not applicable")

# Convert to labelled numeric variables
cleaned_data <- cleaned_data %>% mutate(
  imd15 = labelled::labelled(imd15, missing_labels),
  imd16 = labelled::labelled(imd16, missing_labels),
  imd32 = labelled::labelled(imd32, missing_labels)
)

# Write the cleaned dataset to CSV
write_csv(cleaned_data, file.path(output_dir, "cleaned_data.csv"))