library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from the metadata
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_nine_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine_derived, by = "NSID") %>%
  full_join(wave_nine_main, by = "NSID")

# Define a function to map missing values to standard codes
map_missing <- function(var) {
  case_when(
    var %in% c(-92, -9) ~ -9,
    var %in% c(-94, -8) ~ -8,
    var %in% c(-7) ~ -7,
    var %in% c(-999, -998, -997, -995, -2) ~ -2,
    var %in% c(-99, -3) ~ -3,
    var %in% c(-91, -1) ~ -1,
    TRUE ~ var
  )
}

# Derive regub15 (Urban/Rural Indicator at age 15) from wave_two
merged_data <- merged_data %>%
  mutate(regub15 = map_missing(urbind.x))

# Derive regub16 (Urban/Rural Indicator at age 16) from wave_three
merged_data <- merged_data %>%
  mutate(regub16 = map_missing(urbind.y))

# Derive regov15 (Government Office Region at age 15) from wave_two
merged_data <- merged_data %>%
  mutate(regov15 = map_missing(gor.x))

# Derive regov16 (Government Office Region at age 16) from wave_three
merged_data <- merged_data %>%
  mutate(regov16 = map_missing(gor.y))

# Derive regor25 (Government Office Region at age 25) from wave_eight
merged_data <- merged_data %>%
  mutate(regor25 = map_missing(W8DGOR))

# Derive regor32 (Government Office Region at age 32) from wave_nine_derived
merged_data <- merged_data %>%
  mutate(regor32 = map_missing(W9DRGN))

# Derive regint32 (International residence at age 32) from wave_nine_main
merged_data <- merged_data %>%
  mutate(regint32 = map_missing(W9NATIONRES))

# Select only the ID variable and the derived variables
cleaned_data <- merged_data %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write the cleaned data to a CSV file
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data file
"data/output/cleaned_data.csv"