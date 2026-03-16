# Load required libraries
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
data_14 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
data_17 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
data_18 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
data_19 <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
data_20 <- read_delim(files[5], delim = "\t", show_col_types = FALSE)
data_25 <- read_delim(files[6], delim = "\t", show_col_types = FALSE)
data_32 <- read_delim(files[7], delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(data_14, data_17, by = "NSID")
df <- full_join(df, data_18, by = "NSID")
df <- full_join(df, data_19, by = "NSID")
df <- full_join(df, data_20, by = "NSID")
df <- full_join(df, data_25, by = "NSID")
df <- full_join(df, data_32, by = "NSID")

# Function to standardize missing codes
standardize_missing <- function(x) {
  x <- as.numeric(x)
  # Map wave-specific missing codes to standard codes
  x <- case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    x %in% c(-9, -8, -1) ~ x,
    is.na(x) ~ -3
  )
  return(x)
}

# Function to harmonize economic activity - Age 17 (W4empsYP)
harmonize_age17 <- function(x) {
  x <- standardize_missing(x)
  case_when(
    x == -9 ~ "Refusal",
    x == -8 ~ "Don't know",
    x == -1 ~ "Not applicable",
    x == -3 ~ "Not asked",
    x %in% c(1, 2) ~ "In paid work",
    x == 3 ~ "Unemployed",
    x == 4 ~ "Training",
    x == 5 ~ "In education",
    x == 6 ~ "Looking after family",
    x == 7 ~ "Retired",
    x == 8 ~ "Sick/disabled",
    x == 9 ~ "Other",
    TRUE ~ as.character(x)
  )
}

# Function to harmonize economic activity - Age 18 (W5mainactYP)
harmonize_age18 <- function(x) {
  x <- standardize_missing(x)
  case_when(
    x == -8 ~ "Don't know",
    x == -1 ~ "Not applicable",
    x == -3 ~ "Not asked",
    x == 1 ~ "Apprenticeship",
    x == 2 ~ "Part work/part education",
    x == 3 ~ "In paid work",
    x == 4 ~ "In education",
    x == 5 ~ "Training course",
    x == 6 ~ "Entry to employment scheme",
    x == 7 ~ "Unemployed",
    x == 8 ~ "Looking after family",
    x == 9 ~ "Waiting for course/job",
    x == 10 ~ "Waiting for exam results",
    x == 11 ~ "Waiting for job application",
    TRUE ~ as.character(x)
  )
}

# Function to harmonize economic activity - Age 19 (W6TCurrentAct)
harmonize_age19 <- function(x) {
  x <- standardize_missing(x)
  case_when(
    x == -1 ~ "Not applicable",
    x == -3 ~ "Not asked",
    x == 1 ~ "University",
    x == 2 ~ "In education",
    x == 3 ~ "In paid work",
    x == 4 ~ "Training course",
    x == 5 ~ "Apprenticeship",
    x == 6 ~ "Waiting for course/job",
    x == 7 ~ "Looking after family",
    x == 8 ~ "Unemployed",
    x == 9 ~ "Waiting for exam/job application",
    x == 10 ~ "Part work/part education",
    x == 11 ~ "Voluntary work",
    TRUE ~ as.character(x)
  )
}

# Function to harmonize economic activity - Age 20 (W7TCurrentAct)
harmonize_age20 <- function(x) {
  x <- standardize_missing(x)
  case_when(
    x == -1 ~ "Not applicable",
    x == -3 ~ "Not asked",
    x == 1 ~ "University",
    x == 2 ~ "School/college education",
    x == 3 ~ "Paid work",
    x == 4 ~ "Training course",
    x == 5 ~ "Apprenticeship",
    x == 6 ~ "Waiting for course/job",
    x == 7 ~ "Looking after family",
    x == 8 ~ "Unemployed",
    x == 9 ~ "Part time job/part time college",
    x == 10 ~ "Voluntary work",
    x == 11 ~ "Government employment programme",
    x == 12 ~ "Travelling",
    x == 13 ~ "Break from work/college",
    x == 14 ~ "Ill/disabled",
    x == 15 ~ "Not defined",
    TRUE ~ as.character(x)
  )
}

# Function to harmonize economic activity - Age 25 (W8DACTIVITYC) - collapsed version
harmonize_age25_collapsed <- function(x) {
  x <- standardize_missing(x)
  case_when(
    x == -9 ~ "Refusal",
    x == -8 ~ "Don't know",
    x == -1 ~ "Not applicable",
    x == -3 ~ "Not asked",
    x %in% c(1, 2) ~ "In paid work",
    x == 3 ~ "Unpaid/voluntary work",
    x == 4 ~ "Unemployed",
    x == 5 ~ "Education",
    x == 6 ~ "Apprenticeship",
    x == 7 ~ "Gov't employment training scheme",
    x == 8 ~ "Sick/disabled",
    x == 9 ~ "Looking after family",
    x == 10 ~ "Other",
    TRUE ~ as.character(x)
  )
}

# Function to harmonize economic activity - Age 32 (W9DACTIVITYC) - collapsed version
harmonize_age32_collapsed <- function(x) {
  x <- standardize_missing(x)
  case_when(
    x == -9 ~ "Refusal",
    x == -8 ~ "Don't know",
    x == -1 ~ "Not applicable",
    x == -3 ~ "Not asked",
    x %in% c(1, 2) ~ "In paid work",
    x == 3 ~ "Unpaid/voluntary work",
    x == 4 ~ "Unemployed",
    x == 5 ~ "Education",
    x == 6 ~ "Apprenticeship",
    x == 7 ~ "Gov't employment training scheme",
    x == 8 ~ "Sick/disabled",
    x == 9 ~ "Looking after family",
    x == 10 ~ "Other",
    TRUE ~ as.character(x)
  )
}

# Apply harmonization to create age-specific variables
df$ecoact17 <- harmonize_age17(df$W4empsYP)
df$ecoact18 <- harmonize_age18(df$W5mainactYP)
df$ecoact19 <- harmonize_age19(df$W6TCurrentAct)
df$ecoact20 <- harmonize_age20(df$W7TCurrentAct)
df$ecoact25 <- harmonize_age25_collapsed(df$W8DACTIVITYC)
df$ecoact32 <- harmonize_age32_collapsed(df$W9DACTIVITYC)

# Create detailed adult versions for ages 25 and 32
# Age 25 detailed - map from W8DACTIVITYC
df$ecoactadu25 <- case_when(
  df$W8DACTIVITYC == -9 ~ "Refusal",
  df$W8DACTIVITYC == -8 ~ "Insufficient information",
  df$W8DACTIVITYC == -1 ~ "Not applicable",
  df$W8DACTIVITYC == -3 ~ "Not asked",
  df$W8DACTIVITYC == 1 ~ "Employee - in paid work",
  df$W8DACTIVITYC == 2 ~ "Self employed",
  df$W8DACTIVITYC == 3 ~ "Unpaid/voluntary work",
  df$W8DACTIVITYC == 4 ~ "Unemployed",
  df$W8DACTIVITYC == 5 ~ "Education: School/college/university",
  df$W8DACTIVITYC == 6 ~ "Apprenticeship",
  df$W8DACTIVITYC == 7 ~ "On gov't scheme for employment training",
  df$W8DACTIVITYC == 8 ~ "Sick or disabled",
  df$W8DACTIVITYC == 9 ~ "Looking after home or family",
  df$W8DACTIVITYC == 10 ~ "Something else",
  TRUE ~ as.character(df$W8DACTIVITYC)
)

# Age 32 detailed - map from W9DACTIVITYC
df$ecoactadu32 <- case_when(
  df$W9DACTIVITYC == -9 ~ "Refusal",
  df$W9DACTIVITYC == -8 ~ "Insufficient information",
  df$W9DACTIVITYC == -1 ~ "Not applicable",
  df$W9DACTIVITYC == -3 ~ "Not asked",
  df$W9DACTIVITYC == 1 ~ "Employee - in paid work",
  df$W9DACTIVITYC == 2 ~ "Self employed",
  df$W9DACTIVITYC == 3 ~ "Unpaid/voluntary work",
  df$W9DACTIVITYC == 4 ~ "Unemployed",
  df$W9DACTIVITYC == 5 ~ "Education: School/college/university",
  df$W9DACTIVITYC == 6 ~ "Apprenticeship",
  df$W9DACTIVITYC == 7 ~ "On gov't scheme for employment training",
  df$W9DACTIVITYC == 8 ~ "Sick or disabled",
  df$W9DACTIVITYC == 9 ~ "Looking after home or family",
  df$W9DACTIVITYC == 10 ~ "Something else",
  TRUE ~ as.character(df$W9DACTIVITYC)
)

# Convert all harmonized variables to factors
df$ecoact17 <- factor(df$ecoact17, levels = c("Refusal", "Don't know", "Not applicable", "Not asked", "In paid work", "Unemployed", "Training", "In education", "Looking after family", "Retired", "Sick/disabled", "Other"))
df$ecoact18 <- factor(df$ecoact18, levels = c("Don't know", "Not applicable", "Not asked", "Apprenticeship", "Part work/part education", "In paid work", "In education", "Training course", "Entry to employment scheme", "Unemployed", "Looking after family", "Waiting for course/job", "Waiting for exam results", "Waiting for job application"))
df$ecoact19 <- factor(df$ecoact19, levels = c("Not applicable", "Not asked", "University", "In education", "In paid work", "Training course", "Apprenticeship", "Waiting for course/job", "Looking after family", "Unemployed", "Waiting for exam/job application", "Part work/part education", "Voluntary work"))
df$ecoact20 <- factor(df$ecoact20, levels = c("Not applicable", "Not asked", "University", "School/college education", "Paid work", "Training course", "Apprenticeship", "Waiting for course/job", "Looking after family", "Unemployed", "Part time job/part time college", "Voluntary work", "Government employment programme", "Travelling", "Break from work/college", "Ill/disabled", "Not defined"))
df$ecoact25 <- factor(df$ecoact25, levels = c("Refusal", "Don't know", "Not applicable", "Not asked", "In paid work", "Unpaid/voluntary work", "Unemployed", "Education", "Apprenticeship", "Gov't employment training scheme", "Sick/disabled", "Looking after family", "Other"))
df$ecoact32 <- factor(df$ecoact32, levels = c("Refusal", "Don't know", "Not applicable", "Not asked", "In paid work", "Unpaid/voluntary work", "Unemployed", "Education", "Apprenticeship", "Gov't employment training scheme", "Sick/disabled", "Looking after family", "Other"))

# Convert detailed variables to factors
df$ecoactadu25 <- factor(df$ecoactadu25, levels = c("Refusal", "Insufficient information", "Not applicable", "Not asked", "Employee - in paid work", "Self employed", "Unpaid/voluntary work", "Unemployed", "Education: School/college/university", "Apprenticeship", "On gov't scheme for employment training", "Sick or disabled", "Looking after home or family", "Something else"))
df$ecoactadu32 <- factor(df$ecoactadu32, levels = c("Refusal", "Insufficient information", "Not applicable", "Not asked", "Employee - in paid work", "Self employed", "Unpaid/voluntary work", "Unemployed", "Education: School/college/university", "Apprenticeship", "On gov't scheme for employment training", "Sick or disabled", "Looking after home or family", "Something else"))

# Select only required variables
output_df <- select(df, NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
