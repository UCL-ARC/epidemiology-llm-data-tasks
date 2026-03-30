
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define file paths
file_paths <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load each dataset
load_dataset <- function(file_path) {
  read_delim(file_path, delim = "\t")
}

datasets <- map(file_paths, load_dataset)

# Name each dataset for clarity
wave_one <- datasets[[1]]
wave_two <- datasets[[2]]
wave_three <- datasets[[3]]
wave_nine <- datasets[[4]]

# Select only NSID and IMDRSCORE from wave_two and wave_three
wave_two_clean <- wave_two %>% select(NSID, IMDRSCORE)
wave_three_clean <- wave_three %>% select(NSID, IMDRSCORE)

# Merge datasets using NSID as the key
merged_data <- full_join(wave_one, wave_two_clean, by = "NSID") %>%
  full_join(wave_three_clean, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Rename columns to imd15, imd16, and imd32
clean_data <- merged_data %>%
  rename(imd15 = IMDRSCORE.x, imd16 = IMDRSCORE.y) %>%
  mutate(imd32 = W9DIMDD) %>%
  select(NSID, imd15, imd16, imd32)

# Recode missing values
clean_data <- clean_data %>%
  mutate(
    imd15 = ifelse(is.na(imd15), -3, imd15),
    imd16 = ifelse(is.na(imd16), -3, imd16),
    imd32 = ifelse(is.na(imd32), -3, imd32)
  ) %>%
  mutate(
    imd15 = recode(imd15, '-94' = -8),
    imd16 = recode(imd16, '-94' = -8)
  )

# Add attributes for labels (without converting to factors)
attributes(clean_data$imd15) <- list(
  labels = c(
    `-9` = "Refusal",
    `-8` = "Don't know / insufficient information",
    `-3` = "Not asked at the fieldwork stage / not interviewed",
    `-2` = "Script error / information lost",
    `-1` = "Not applicable"
  )
)

attributes(clean_data$imd16) <- list(
  labels = c(
    `-9` = "Refusal",
    `-8` = "Don't know / insufficient information",
    `-3` = "Not asked at the fieldwork stage / not interviewed",
    `-2` = "Script error / information lost",
    `-1` = "Not applicable"
  )
)

attributes(clean_data$imd32) <- list(
  labels = c(
    `-9` = "Refusal",
    `-8` = "Don't know / insufficient information",
    `-3` = "Not asked at the fieldwork stage / not interviewed",
    `-2` = "Script error / information lost",
    `-1` = "Not applicable"
  )
)

# Write the cleaned data to CSV
write_csv(clean_data, "data/output/cleaned_data.csv")

# Print confirmation
message("Cleaned dataset has been successfully saved to data/output/cleaned_data.csv")
