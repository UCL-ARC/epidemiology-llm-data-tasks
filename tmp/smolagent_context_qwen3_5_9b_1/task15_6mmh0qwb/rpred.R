library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets
data <- full_join(wave1, wave4, by = "NSID")
data <- full_join(data, ns8, by = "NSID")
data <- full_join(data, ns9, by = "NSID")

# Rename income variables
data <- data %>%
  rename(inc25 = W8DINCB,
         inc32 = W9DINCB)

# Handle missing values
# Convert R NA to -3 (Not asked)
# Convert -1 to -1 (Item not applicable)
data$inc25 <- ifelse(is.na(data$inc25), -3, data$inc25)
data$inc25 <- ifelse(data$inc25 == -1, -1, data$inc25)
data$inc32 <- ifelse(is.na(data$inc32), -3, data$inc32)
data$inc32 <- ifelse(data$inc32 == -1, -1, data$inc32)

# Write to CSV
write_csv(data, "data/output/cleaned_data.csv")