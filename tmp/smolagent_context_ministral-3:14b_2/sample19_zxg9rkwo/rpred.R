library(haven)
library(dplyr)
library(readr)

# Load and merge datasets
wave_one_young_person <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four_young_person <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave_one_young_person, wave_four_young_person, by = "NSID")
merged_data <- full_join(merged_data, ns8_derived, by = "NSID")
merged_data <- full_join(merged_data, ns9_derived, by = "NSID")

# Rename BMI variables
merged_data <- merged_data %>%
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

# Function to harmonize missing values
harmonize_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x < -9] <- -3
  return(x)
}

# Apply missing value harmonization
merged_data <- merged_data %>%
  mutate(
    bmi25 = harmonize_missing(bmi25),
    bmi32 = harmonize_missing(bmi32)
  )

# Ensure BMI variables are numeric
merged_data <- merged_data %>%
  mutate(
    bmi25 = as.numeric(bmi25),
    bmi32 = as.numeric(bmi32)
  )

# Select required variables
cleaned_data <- merged_data %>%
  select(NSID, bmi25, bmi32)

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)