# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Function to safely check if a column exists in a data frame
column_exists <- function(df, col_name) {
  col_name %in% names(df)
}

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Select and rename specific variables
wave4 <- wave4 %>% select(NSID, W4empsYP)
wave5 <- wave5 %>% select(NSID, W5mainactYP)
wave6 <- wave6 %>% select(NSID, W6TCurrentAct)
wave7 <- wave7 %>% select(NSID, W7TCurrentAct)
wave8 <- wave8 %>% select(NSID, W8DACTIVITYC)
wave9 <- wave9 %>% select(NSID, W9DACTIVITYC)

# Merge datasets
merged_data <- wave1
merged_data <- full_join(merged_data, wave4, by = 'NSID')
merged_data <- full_join(merged_data, wave5, by = 'NSID')
merged_data <- full_join(merged_data, wave6, by = 'NSID')
merged_data <- full_join(merged_data, wave7, by = 'NSID')
merged_data <- full_join(merged_data, wave8, by = 'NSID')
merged_data <- full_join(merged_data, wave9, by = 'NSID')

# Define mapping functions for collapsed categories
map_collapsed_wave4 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,  # Doing paid work
    x == 4 ~ 2,            # Training
    x == 5 ~ 3,            # Education
    x == 3 ~ 4,            # Unemployed
    x == 6 ~ 5,            # Looking after home/family
    TRUE ~ 6              # Other
  )
}

map_collapsed_wave5 <- function(x) {
  case_when(
    x == 3 ~ 1,            # In paid work
    x %in% c(5, 6) ~ 2,      # Training
    x == 4 ~ 3,             # Education
    x == 7 ~ 4,             # Unemployed
    x == 8 ~ 5,             # Looking after home/family
    TRUE ~ 6                # Other
  )
}

map_collapsed_wave6 <- function(x) {
  case_when(
    x == 3 ~ 1,            # In paid work
    x %in% c(4, 5) ~ 2,      # Training/Apprenticeship
    x %in% c(1, 2) ~ 3,      # Education
    x == 8 ~ 4,             # Unemployed
    x == 7 ~ 5,             # Looking after home/family
    TRUE ~ 6                # Other
  )
}

map_collapsed_wave7 <- function(x) {
  case_when(
    x == 3 ~ 1,            # In paid work
    x %in% c(4, 5, 7, 11) ~ 2, # Training/Apprenticeship/Other training
    x %in% c(1, 2) ~ 3,      # Education
    x == 8 ~ 4,             # Unemployed
    x == 7 ~ 5,             # Looking after home/family
    TRUE ~ 6                # Other
  )
}

map_collapsed_wave8 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,     # In paid work
    x == 7 ~ 2,              # Training
    x == 5 ~ 3,              # Education
    x == 4 ~ 4,              # Unemployed
    x == 9 ~ 5,              # Looking after home/family
    TRUE ~ 6                # Other
  )
}

map_collapsed_wave9 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,     # In paid work
    x == 7 ~ 2,              # Training
    x == 5 ~ 3,              # Education
    x == 4 ~ 4,              # Unemployed
    x == 9 ~ 5,              # Looking after home/family
    TRUE ~ 6                # Other
  )
}

# Define missing value handling function
handle_missing <- function(x, wave) {
  missing_codes <- switch(wave,
    4, c(-999, -94, -92, -91),
    5, c(-999, -94, -91),
    6, c(-999, -91),
    7, c(-999, -91),
    8, c(-9, -8, -1),
    9, c(-9, -8, -1)
  )
  ifelse(x %in% missing_codes, NA_integer_, x)
}

# Create derived variables with checks
if (column_exists(merged_data, 'W4empsYP')) {
  merged_data$W4empsYP <- handle_missing(merged_data$W4empsYP, 4)
  merged_data$ecoact17 <- map_collapsed_wave4(merged_data$W4empsYP)
  merged_data$ecoact17[is.na(merged_data$ecoact17)] <- -3
} else {
  merged_data$ecoact17 <- rep(-3, nrow(merged_data))
}

if (column_exists(merged_data, 'W5mainactYP')) {
  merged_data$W5mainactYP <- handle_missing(merged_data$W5mainactYP, 5)
  merged_data$ecoact18 <- map_collapsed_wave5(merged_data$W5mainactYP)
  merged_data$ecoact18[is.na(merged_data$ecoact18)] <- -3
} else {
  merged_data$ecoact18 <- rep(-3, nrow(merged_data))
}

if (column_exists(merged_data, 'W6TCurrentAct')) {
  merged_data$W6TCurrentAct <- handle_missing(merged_data$W6TCurrentAct, 6)
  merged_data$ecoact19 <- map_collapsed_wave6(merged_data$W6TCurrentAct)
  merged_data$ecoact19[is.na(merged_data$ecoact19)] <- -3
} else {
  merged_data$ecoact19 <- rep(-3, nrow(merged_data))
}

if (column_exists(merged_data, 'W7TCurrentAct')) {
  merged_data$W7TCurrentAct <- handle_missing(merged_data$W7TCurrentAct, 7)
  merged_data$ecoact20 <- map_collapsed_wave7(merged_data$W7TCurrentAct)
  merged_data$ecoact20[is.na(merged_data$ecoact20)] <- -3
} else {
  merged_data$ecoact20 <- rep(-3, nrow(merged_data))
}

if (column_exists(merged_data, 'W8DACTIVITYC')) {
  merged_data$W8DACTIVITYC <- handle_missing(merged_data$W8DACTIVITYC, 8)
  merged_data$ecoact25 <- map_collapsed_wave8(merged_data$W8DACTIVITYC)
  merged_data$ecoact25[is.na(merged_data$ecoact25)] <- -3
  merged_data$ecoactadu25 <- merged_data$W8DACTIVITYC
  merged_data$ecoactadu25[is.na(merged_data$ecoactadu25)] <- -3
} else {
  merged_data$ecoact25 <- rep(-3, nrow(merged_data))
  merged_data$ecoactadu25 <- rep(-3, nrow(merged_data))
}

if (column_exists(merged_data, 'W9DACTIVITYC')) {
  merged_data$W9DACTIVITYC <- handle_missing(merged_data$W9DACTIVITYC, 9)
  merged_data$ecoact32 <- map_collapsed_wave9(merged_data$W9DACTIVITYC)
  merged_data$ecoact32[is.na(merged_data$ecoact32)] <- -3
  merged_data$ecoactadu32 <- merged_data$W9DACTIVITYC
  merged_data$ecoactadu32[is.na(merged_data$ecoactadu32)] <- -3
} else {
  merged_data$ecoact32 <- rep(-3, nrow(merged_data))
  merged_data$ecoactadu32 <- rep(-3, nrow(merged_data))
}

# Ensure variables are numeric
numeric_vars <- c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32", "ecoactadu25", "ecoactadu32")
for (var in numeric_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- as.integer(merged_data[[var]])
  }
}

# Select final variables
final_vars <- c("NSID", "ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32", "ecoactadu25", "ecoactadu32")
final_output <- merged_data %>% select(all_of(final_vars))

# Write output
write_csv(final_output, 'data/output/cleaned_data.csv')

# Confirm output
cat('Output file created successfully: ', file.exists('data/output/cleaned_data.csv'), '\n')