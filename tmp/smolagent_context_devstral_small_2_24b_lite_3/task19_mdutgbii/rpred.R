library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>% 
  full_join(wave_four, by = "NSID") %>% 
  full_join(ns8, by = "NSID") %>% 
  full_join(ns9, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(var, metadata) {
  var <- ifelse(var %in% c(-9, -92), -9, var)  # Refusal
  var <- ifelse(var %in% c(-8, -94), -8, var)  # Don't know / insufficient information
  var <- ifelse(var %in% c(-7, -97), -7, var)  # Prefer not to say
  var <- ifelse(var %in% c(-3, -99), -3, var)  # Not asked / not interviewed
  var <- ifelse(var %in% c(-2, -999, -998, -997, -995), -2, var)  # Schedule not applicable / script error
  var <- ifelse(var %in% c(-1, -91), -1, var)  # Item not applicable
  var <- ifelse(is.na(var), -3, var)  # Convert NA to -3
  return(var)
}

# Harmonize missing values for W8DBMI and W9DBMI
merged_data <- merged_data %>% 
  mutate(W8DBMI = harmonize_missing(W8DBMI, ns8$variables$W8DBMI),
         W9DBMI = harmonize_missing(W9DBMI, ns9$variables$W9DBMI))

# Derive BMI variables
cleaned_data <- merged_data %>% 
  select(NSID, W8DBMI, W9DBMI) %>% 
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"