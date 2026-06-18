library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Full merge by NSID
df <- full_join(wave1, wave4, by = "NSID")
df <- full_join(df, wave5, by = "NSID")
df <- full_join(df, wave6, by = "NSID")
df <- full_join(df, wave7, by = "NSID")
df <- full_join(df, wave8, by = "NSID")
df <- full_join(df, wave9, by = "NSID")

# Function to map W4empsYP (Age 17) to collapsed 6-category scheme
map_w4 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,  # Paid work (30+ or <30 hours)
    x %in% c(4) ~ 2,      # Training course or scheme
    x == 5 ~ 3,           # Full-time education/school
    x == 3 ~ 4,           # Unemployed/looking for job
    x == 6 ~ 5,           # Looking after family/household
    x %in% c(7, 8, 9) ~ 6, # Other (retired, sick/disabled, other)
    TRUE ~ NA_real_
  )
}

# Function to map W5mainactYP (Age 18) to collapsed 6-category scheme
map_w5 <- function(x) {
  case_when(
    x %in% c(3) ~ 1,                          # In paid work
    x %in% c(1, 5, 6) ~ 2,                    # Apprenticeship, training course/scheme, Entry to Employment
    x == 4 ~ 3,                               # In education
    x == 7 ~ 4,                               # Unemployed and looking for work
    x == 8 ~ 5,                               # Looking after family and home
    x %in% c(9, 10, 11) ~ 6                   # Other (waiting for course/job, exam results, job application)
  )
}

# Function to map W6TCurrentAct (Age 19) to collapsed 6-category scheme
map_w6 <- function(x) {
  case_when(
    x %in% c(3) ~ 1,                          # In paid work
    x %in% c(4, 5) ~ 2,                       # Training course/scheme, Apprenticeship
    x %in% c(1, 2) ~ 3,                       # University, Education
    x == 8 ~ 4,                               # Unemployed and looking for work
    x == 7 ~ 5,                               # Looking after family and home
    x %in% c(6, 9, 10, 11) ~ 6                # Other (waiting, part-time work, voluntary)
  )
}

# Function to map W7TCurrentAct (Age 20) to collapsed 6-category scheme
map_w7 <- function(x) {
  case_when(
    x %in% c(3) ~ 1,                          # Paid work
    x %in% c(4, 5, 11) ~ 2,                   # Training course/scheme, Apprenticeship, Government employment programme
    x %in% c(1, 2) ~ 3,                       # University, School/college education
    x == 8 ~ 4,                               # Unemployed and looking for work
    x == 7 ~ 5,                               # Looking after home/family
    x %in% c(6, 9, 10, 12, 13, 14, 15) ~ 6    # Other (waiting, part-time, voluntary, travelling, break, ill, undefined)
  )
}

# Function to map W8DACTIVITYC (Age 25) to collapsed 6-category scheme
map_w8 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,                       # Employee/Self employed (in paid work)
    x %in% c(6, 7) ~ 2,                       # Apprenticeship, Gov't scheme for employment training
    x == 5 ~ 3,                               # Education
    x == 4 ~ 4,                               # Unemployed
    x == 9 ~ 5,                               # Looking after home or family
    x %in% c(3, 8, 10) ~ 6                    # Other (voluntary, sick/disabled, something else)
  )
}

# Function to map W9DACTIVITYC (Age 32) to collapsed 6-category scheme
map_w9 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,                       # Employee/Self employed (in paid work)
    x %in% c(6, 7) ~ 2,                       # Apprenticeship, Gov't scheme for employment training
    x == 5 ~ 3,                               # Education
    x == 4 ~ 4,                               # Unemployed
    x == 9 ~ 5,                               # Looking after home or family
    x %in% c(3, 8, 10) ~ 6                    # Other (voluntary, sick/disabled, something else)
  )
}

# Create collapsed variables with NA for missing values (will be converted later)
df$ecoact17_raw <- map_w4(df$W4empsYP)
df$ecoact18_raw <- map_w5(df$W5mainactYP)
df$ecoact19_raw <- map_w6(df$W6TCurrentAct)
df$ecoact20_raw <- map_w7(df$W7TCurrentAct)
df$ecoact25_raw <- map_w8(df$W8DACTIVITYC)
df$ecoact32_raw <- map_w9(df$W9DACTIVITYC)

# Missing code mapping function
map_missing <- function(x) {
  case_when(
    x %in% c(-9, -92) ~ -9,                   # Refusal
    x %in% c(-999, -94, -8, -98) ~ -8,        # Don't know / insufficient information
    x %in% c(-997, -998, -995) ~ -2,          # Schedule not applicable / script error / information lost
    x %in% c(-99, -97) ~ -3,                  # Not asked at fieldwork
    x %in% c(-91, -1) ~ -1,                   # Item not applicable / Not applicable
    is.na(x) ~ NA_real_,                      # Keep as NA for valid data
    TRUE ~ x                                  # Keep original
  )
}

# Map missing codes for each wave
df$ecoact17_missing <- map_missing(df$W4empsYP)
df$ecoact18_missing <- map_missing(df$W5mainactYP)
df$ecoact19_missing <- map_missing(df$W6TCurrentAct)
df$ecoact20_missing <- map_missing(df$W7TCurrentAct)
df$ecoact25_missing <- map_missing(df$W8DACTIVITYC)
df$ecoact32_missing <- map_missing(df$W9DACTIVITYC)

# Combine mapped categories with missing codes
combine_categories <- function(cat, miss) {
  case_when(
    !is.na(cat) & !is.na(miss) ~ cat,         # Valid category
    is.na(cat) & !is.na(miss) ~ miss,         # Missing code (from invalid value)
    !is.na(cat) & is.na(miss) ~ cat,          # Valid category (no missing code in source)
    TRUE ~ -3                                 # Default to -3 (not asked)
  )
}

df$ecoact17 <- combine_categories(df$ecoact17_raw, df$ecoact17_missing)
df$ecoact18 <- combine_categories(df$ecoact18_raw, df$ecoact18_missing)
df$ecoact19 <- combine_categories(df$ecoact19_raw, df$ecoact19_missing)
df$ecoact20 <- combine_categories(df$ecoact20_raw, df$ecoact20_missing)
df$ecoact25 <- combine_categories(df$ecoact25_raw, df$ecoact25_missing)
df$ecoact32 <- combine_categories(df$ecoact32_raw, df$ecoact32_missing)

# Detailed variables for ages 25 and 32 (use exact 10 categories from metadata)
df$ecoactadu25 <- df$W8DACTIVITYC
df$ecoactadu32 <- df$W9DACTIVITYC

# Standardize missing codes for detailed variables
map_w8_missing <- function(x) {
  case_when(
    x %in% c(-9) ~ -9,                        # Refused
    x %in% c(-8) ~ -8,                        # Insufficient information
    x %in% c(-1) ~ -1,                        # Not applicable
    TRUE ~ NA_real_                           # Keep valid values
  )
}

df$ecoactadu25_missing <- map_w8_missing(df$W8DACTIVITYC)
df$ecoactadu32_missing <- map_w8_missing(df$W9DACTIVITYC)

# Combine for detailed variables
df$ecoactadu25 <- combine_categories(df$ecoactadu25, df$ecoactadu25_missing)
df$ecoactadu32 <- combine_categories(df$ecoactadu32, df$ecoactadu32_missing)

# Remove raw working variables
df <- df %>% select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output using readr::write_csv
readr::write_csv(df, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Variables:", paste(names(df), collapse = ", "), "\n")
cat("Sample data:\n")
print(head(df))