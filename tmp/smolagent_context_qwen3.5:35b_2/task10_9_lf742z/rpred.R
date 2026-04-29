# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/wave_five_lsype_young_person_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load all files
wave_one <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
wave_four <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
wave_five <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
wave_six <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
wave_seven <- read_delim(files[5], delim = "\t", show_col_types = FALSE)
wave_eight <- read_delim(files[6], delim = "\t", show_col_types = FALSE)
wave_nine <- read_delim(files[7], delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(wave_one, wave_four, by = "NSID")
df <- full_join(df, wave_five, by = "NSID")
df <- full_join(df, wave_six, by = "NSID")
df <- full_join(df, wave_seven, by = "NSID")
df <- full_join(df, wave_eight, by = "NSID")
df <- full_join(df, wave_nine, by = "NSID")

# Function to standardize missing codes
standardize_missing <- function(x) {
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[is.na(x)] <- -3
  return(x)
}

# Age 17: W4empsYP -> ecoact17 (collapsed 6-category)
df$ecoact17 <- standardize_missing(df$W4empsYP)
df$ecoact17 <- case_when(
  df$ecoact17 %in% c(-9, -8, -1, -3, -2) ~ df$ecoact17,
  df$ecoact17 == 1 | df$ecoact17 == 2 ~ 1,  # In paid work
  df$ecoact17 == 3 ~ 2,  # Unemployed
  df$ecoact17 == 4 ~ 3,  # Training
  df$ecoact17 == 5 ~ 4,  # In education
  df$ecoact17 == 6 ~ 5,  # Looking after family
  df$ecoact17 == 7 ~ 6,  # Retired
  df$ecoact17 == 8 ~ 7,  # Sick/disabled
  df$ecoact17 == 9 ~ 8,  # Other
  TRUE ~ df$ecoact17
)

# Age 18: W5mainactYP -> ecoact18 (collapsed 6-category)
df$ecoact18 <- standardize_missing(df$W5mainactYP)
df$ecoact18 <- case_when(
  df$ecoact18 %in% c(-9, -8, -1, -3, -2) ~ df$ecoact18,
  df$ecoact18 == 1 | df$ecoact18 == 2 ~ 1,  # In paid work (apprenticeship/part-time)
  df$ecoact18 == 3 ~ 1,  # In paid work
  df$ecoact18 == 4 ~ 2,  # In education
  df$ecoact18 == 5 | df$ecoact18 == 6 ~ 3,  # Training
  df$ecoact18 == 7 ~ 4,  # Unemployed
  df$ecoact18 == 8 ~ 5,  # Looking after family
  df$ecoact18 %in% c(9, 10, 11) ~ 6,  # Waiting
  TRUE ~ df$ecoact18
)

# Age 19: W6TCurrentAct -> ecoact19 (collapsed 6-category)
df$ecoact19 <- standardize_missing(df$W6TCurrentAct)
df$ecoact19 <- case_when(
  df$ecoact19 %in% c(-9, -8, -1, -3, -2) ~ df$ecoact19,
  df$ecoact19 == 1 | df$ecoact19 == 2 ~ 2,  # In education
  df$ecoact19 == 3 ~ 1,  # In paid work
  df$ecoact19 == 4 | df$ecoact19 == 5 ~ 3,  # Training
  df$ecoact19 == 6 | df$ecoact19 == 9 ~ 6,  # Waiting
  df$ecoact19 == 7 ~ 5,  # Looking after family
  df$ecoact19 == 8 ~ 4,  # Unemployed
  df$ecoact19 == 10 ~ 1,  # Part-time work/college
  df$ecoact19 == 11 ~ 3,  # Voluntary work
  TRUE ~ df$ecoact19
)

# Age 20: W7TCurrentAct -> ecoact20 (collapsed 6-category)
df$ecoact20 <- standardize_missing(df$W7TCurrentAct)
df$ecoact20 <- case_when(
  df$ecoact20 %in% c(-9, -8, -1, -3, -2) ~ df$ecoact20,
  df$ecoact20 == 1 | df$ecoact20 == 2 ~ 2,  # In education
  df$ecoact20 == 3 ~ 1,  # In paid work
  df$ecoact20 == 4 | df$ecoact20 == 5 ~ 3,  # Training
  df$ecoact20 == 6 | df$ecoact20 == 9 ~ 6,  # Waiting
  df$ecoact20 == 7 ~ 5,  # Looking after family
  df$ecoact20 == 8 ~ 4,  # Unemployed
  df$ecoact20 == 10 ~ 3,  # Voluntary work
  df$ecoact20 == 11 ~ 3,  # Govt employment programme
  df$ecoact20 == 12 ~ 6,  # Travelling
  df$ecoact20 == 13 ~ 6,  # Break
  df$ecoact20 == 14 ~ 7,  # Sick/disabled
  df$ecoact20 == 15 ~ 8,  # Not defined
  TRUE ~ df$ecoact20
)

# Age 25: W8DACTIVITYC -> ecoact25 (collapsed 6-category) AND ecoactadu25 (detailed 10-category)
df$ecoactadu25 <- standardize_missing(df$W8DACTIVITYC)
df$ecoact25 <- case_when(
  df$ecoactadu25 %in% c(-9, -8, -1, -3, -2) ~ df$ecoactadu25,
  df$ecoactadu25 == 1 | df$ecoactadu25 == 2 ~ 1,  # In paid work (employee/self-employed)
  df$ecoactadu25 == 3 ~ 3,  # Unpaid/voluntary work
  df$ecoactadu25 == 4 ~ 4,  # Unemployed
  df$ecoactadu25 == 5 | df$ecoactadu25 == 6 ~ 2,  # Education/apprenticeship
  df$ecoactadu25 == 7 ~ 3,  # Govt scheme
  df$ecoactadu25 == 8 ~ 7,  # Sick/disabled
  df$ecoactadu25 == 9 ~ 5,  # Looking after family
  df$ecoactadu25 == 10 ~ 8,  # Something else
  TRUE ~ df$ecoactadu25
)

# Age 32: W9DACTIVITYC -> ecoact32 (collapsed 6-category) AND ecoactadu32 (detailed 10-category)
df$ecoactadu32 <- standardize_missing(df$W9DACTIVITYC)
df$ecoact32 <- case_when(
  df$ecoactadu32 %in% c(-9, -8, -1, -3, -2) ~ df$ecoactadu32,
  df$ecoactadu32 == 1 | df$ecoactadu32 == 2 ~ 1,  # In paid work (employee/self-employed)
  df$ecoactadu32 == 3 ~ 3,  # Unpaid/voluntary work
  df$ecoactadu32 == 4 ~ 4,  # Unemployed
  df$ecoactadu32 == 5 | df$ecoactadu32 == 6 ~ 2,  # Education/apprenticeship
  df$ecoactadu32 == 7 ~ 3,  # Govt scheme
  df$ecoactadu32 == 8 ~ 7,  # Sick/disabled
  df$ecoactadu32 == 9 ~ 5,  # Looking after family
  df$ecoactadu32 == 10 ~ 8,  # Something else
  TRUE ~ df$ecoactadu32
)

# Select final variables
final_df <- select(df, NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")