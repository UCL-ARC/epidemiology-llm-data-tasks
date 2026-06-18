
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define file paths
file_paths <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load each file individually
wave1 <- read_delim(file_paths[1], delim = "\t")
wave2 <- read_delim(file_paths[2], delim = "\t")
wave3 <- read_delim(file_paths[3], delim = "\t")
wave4 <- read_delim(file_paths[4], delim = "\t")

# Rename variables for each wave
wave1 <- wave1 %>%
  rename(
    ecoactma14 = W1empsmum,
    ecoactpa14 = W1empsdad
  )

wave2 <- wave2 %>%
  rename(
    ecoactma15 = W2empsmum,
    ecoactpa15 = W2empsdad
  )

wave3 <- wave3 %>%
  rename(
    ecoactma16 = W3empsmum,
    ecoactpa16 = W3empsdad
  )

wave4 <- wave4 %>%
  rename(
    ecoactma17 = w4empsmum,
    ecoactpa17 = w4empsdad
  )

# Select only NSID and the renamed variables
wave1 <- wave1 %>% select(NSID, ecoactma14, ecoactpa14)
wave2 <- wave2 %>% select(NSID, ecoactma15, ecoactpa15)
wave3 <- wave3 %>% select(NSID, ecoactma16, ecoactpa16)
wave4 <- wave4 %>% select(NSID, ecoactma17, ecoactpa17)

# Combine all datasets
combined_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define mapping for missing values
missing_mapping <- function(x) {
  case_when(
    x %in% c(-999, -98, -996) ~ -3,
    TRUE ~ x
  )
}

# Apply missing value mapping
cleaned_data <- combined_data %>%
  mutate(
    ecoactma14 = missing_mapping(ecoactma14),
    ecoactpa14 = missing_mapping(ecoactpa14),
    ecoactma15 = missing_mapping(ecoactma15),
    ecoactpa15 = missing_mapping(ecoactpa15),
    ecoactma16 = missing_mapping(ecoactma16),
    ecoactpa16 = missing_mapping(ecoactpa16),
    ecoactma17 = missing_mapping(ecoactma17),
    ecoactpa17 = missing_mapping(ecoactpa17)
  )

# Create value labels for the economic activity variables
ecoact_labels <- c(
  `-3` = "Not asked at fieldwork stage / not interviewed",
  `1` = "Doing paid work for 30 or more hours a week",
  `2` = "Doing paid work for fewer than 30 hours a week",
  `3` = "Unemployed / Looking for a job",
  `4` = "On a training course or scheme",
  `5` = "In full-time education / at school",
  `6` = "Looking after the family / household",
  `7` = "Retired from work altogether",
  `8` = "Sick / disabled",
  `9` = "Other"
)

# Apply labels to each variable
cleaned_data <- cleaned_data %>%
  mutate(
    ecoactma14 = factor(ecoactma14, levels = c(-3, 1:9), labels = ecoact_labels),
    ecoactpa14 = factor(ecoactpa14, levels = c(-3, 1:9), labels = ecoact_labels),
    ecoactma15 = factor(ecoactma15, levels = c(-3, 1:9), labels = ecoact_labels),
    ecoactpa15 = factor(ecoactpa15, levels = c(-3, 1:9), labels = ecoact_labels),
    ecoactma16 = factor(ecoactma16, levels = c(-3, 1:9), labels = ecoact_labels),
    ecoactpa16 = factor(ecoactpa16, levels = c(-3, 1:9), labels = ecoact_labels),
    ecoactma17 = factor(ecoactma17, levels = c(-3, 1:9), labels = ecoact_labels),
    ecoactpa17 = factor(ecoactpa17, levels = c(-3, 1:9), labels = ecoact_labels)
  )

# Write the cleaned data to CSV
readr::write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Print summary to confirm success
summary(cleaned_data)
