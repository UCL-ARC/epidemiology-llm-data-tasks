
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)

# Define the file paths
file_paths <- list(
  wave_one_lsype_young_person_2020 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four_lsype_young_person_2020 = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_2015_main_interview = "data/input/ns8_2015_main_interview.tab",
  ns8_2015_derived = "data/input/ns8_2015_derived.tab",
  ns9_2022_main_interview = "data/input/ns9_2022_main_interview.tab",
  ns9_2022_derived_variables = "data/input/ns9_2022_derived_variables.tab"
)

# Load each file
wave_one <- read_delim(file_paths$wave_one_lsype_young_person_2020, delim = "\t")
wave_four <- read_delim(file_paths$wave_four_lsype_young_person_2020, delim = "\t")
ns8_main <- read_delim(file_paths$ns8_2015_main_interview, delim = "\t")
ns8_derived <- read_delim(file_paths$ns8_2015_derived, delim = "\t")
ns9_main <- read_delim(file_paths$ns9_2022_main_interview, delim = "\t")
ns9_derived <- read_delim(file_paths$ns9_2022_derived_variables, delim = "\t")

# Merge all datasets by NSID
merged_data <- full_join(wave_one, wave_four, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- ifelse(x == -9, -9, x)  # Refused
  x <- ifelse(x == -8, -8, x)  # Don't know
  x <- ifelse(x == -7, -7, x)  # Prefer not to say
  x <- ifelse(x == -3, -3, x)  # Not asked
  x <- ifelse(x == -2, -2, x)  # Schedule not applicable
  x <- ifelse(x == -1, -1, x)  # Item not applicable
  return(x)
}

# Derive educ25 variable from ns8_2015_derived
educ25 <- ns8_derived %>%
  select(NSID, W8DHANVQH) %>%
  rename(educ25 = W8DHANVQH) %>%
  mutate(educ25 = map_missing_values(educ25)) %>%
  mutate(educ25 = case_when(
    educ25 == 1 ~ 1,  # NVQ Level 1
    educ25 == 2 ~ 2,  # NVQ Level 2
    educ25 == 3 ~ 3,  # NVQ Level 3
    educ25 == 4 ~ 4,  # NVQ Level 4
    educ25 == 5 ~ 5,  # NVQ Level 5
    educ25 == 95 ~ 6, # Other academic qualification
    educ25 == 96 ~ 0, # None of these qualifications
    TRUE ~ educ25
  )) %>%
  mutate(educ25 = factor(educ25,
    levels = c(-9, -8, -7, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6),
    labels = c("Refused", "Insufficient information", "Prefer not to say",
      "Not asked at fieldwork stage", "Schedule not applicable/script error",
      "Not applicable", "No qualifications", "NVQ Level 1", "NVQ Level 2",
      "NVQ Level 3", "NVQ Level 4", "NVQ Level 5", "Other academic qualification")))

# Derive educ32 variable from ns9_2022_derived_variables
educ32 <- ns9_derived %>%
  select(NSID, W9DANVQH) %>%
  rename(educ32 = W9DANVQH) %>%
  mutate(educ32 = map_missing_values(educ32)) %>%
  mutate(educ32 = case_when(
    educ32 == 0 ~ 0,  # NVQ Entry Level
    educ32 == 1 ~ 1,  # NVQ Level 1
    educ32 == 2 ~ 2,  # NVQ Level 2
    educ32 == 3 ~ 3,  # NVQ Level 3
    educ32 == 4 ~ 4,  # NVQ Level 4
    educ32 == 5 ~ 5,  # NVQ Level 5
    educ32 == 95 ~ 6, # Other academic qualification
    educ32 == 96 ~ 7, # None of these qualifications
    TRUE ~ educ32
  )) %>%
  mutate(educ32 = factor(educ32,
    levels = c(-9, -8, -7, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7),
    labels = c("Refused", "Missing information", "Prefer not to say",
      "Not asked at fieldwork stage", "Schedule not applicable/script error",
      "Not applicable", "NVQ Entry Level", "NVQ Level 1", "NVQ Level 2",
      "NVQ Level 3", "NVQ Level 4", "NVQ Level 5", "Other academic qualification", "No qualifications")))

# Derive educadtl32 (detailed academic qualifications) from ns9_2022_main_interview
educadtl32 <- ns9_main %>%
  select(NSID, starts_with("W9ACQU")) %>%
  mutate(across(starts_with("W9ACQU"), map_missing_values))

# Derive educvdtl32 (detailed vocational qualifications) from ns9_2022_main_interview
educvdtl32 <- ns9_main %>%
  select(NSID, starts_with("W9VCQU")) %>%
  mutate(across(starts_with("W9VCQU"), map_missing_values))

# Combine all derived variables into a single dataset
cleaned_data <- merged_data %>%
  left_join(educ25, by = "NSID") %>%
  left_join(educ32, by = "NSID") %>%
  left_join(educadtl32, by = "NSID") %>%
  left_join(educvdtl32, by = "NSID") %>%
  select(NSID, educ25, educ32, starts_with("W9ACQU"), starts_with("W9VCQU")) %>%
  rename_with(~ gsub("W9ACQU", "educadtl32_", .), starts_with("W9ACQU")) %>%
  rename_with(~ gsub("W9VCQU", "educvdtl32_", .), starts_with("W9VCQU"))

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
