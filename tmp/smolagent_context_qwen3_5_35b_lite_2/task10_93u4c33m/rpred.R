library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Full join all files by NSID
df <- full_join(wave1, wave4, by = 'NSID')
df <- full_join(df, wave5, by = 'NSID')
df <- full_join(df, wave6, by = 'NSID')
df <- full_join(df, wave7, by = 'NSID')
df <- full_join(df, ns8, by = 'NSID')
df <- full_join(df, ns9, by = 'NSID')

# Function to convert missing values to standard codes
convert_missing <- function(x) {
  x <- ifelse(x == -999, -2, x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(x == -91, -1, x)
  x <- ifelse(x == -995, -2, x)
  x <- ifelse(x == -997, -2, x)
  x <- ifelse(x == -998, -2, x)
  x <- ifelse(x == -99, -3, x)
  x <- ifelse(x == -100, -2, x)
  x <- ifelse(x == -97, -2, x)
  x <- ifelse(x == -8, -8, x)
  x <- ifelse(x == -7, -7, x)
  x <- ifelse(is.na(x) & is.numeric(x), -3, x)
  return(x)
}

# Map variables to 6-category collapsed scheme
# Categories: 1=Employed (30+ hrs), 2=Employed (<30 hrs), 3=Unemployed, 4=Education, 5=Training, 6=Other

# Age 17: W4empsYP
map_ecoact17 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 5,
    x == 5 ~ 4,
    x %in% c(6, 7, 8, 9) ~ 6,
    TRUE ~ convert_missing(x)
  )
}
df$ecoact17 <- map_ecoact17(df$W4empsYP)

# Age 18: W5mainactYP
map_ecoact18 <- function(x) {
  case_when(
    x == 3 ~ 1,
    x %in% c(1, 2) ~ 2,
    x == 7 ~ 3,
    x %in% c(4, 10, 11) ~ 4,
    x %in% c(5, 6, 9) ~ 5,
    x %in% c(8) ~ 6,
    TRUE ~ convert_missing(x)
  )
}
df$ecoact18 <- map_ecoact18(df$W5mainactYP)

# Age 19: W6TCurrentAct
map_ecoact19 <- function(x) {
  case_when(
    x %in% c(3, 10) ~ 1,
    x %in% c(1, 5) ~ 2,
    x == 8 ~ 3,
    x %in% c(2, 9) ~ 4,
    x == 4 ~ 5,
    x %in% c(6, 7, 11) ~ 6,
    TRUE ~ convert_missing(x)
  )
}
df$ecoact19 <- map_ecoact19(df$W6TCurrentAct)

# Age 20: W7TCurrentAct
map_ecoact20 <- function(x) {
  case_when(
    x %in% c(3, 9) ~ 1,
    x %in% c(1, 5) ~ 2,
    x == 8 ~ 3,
    x %in% c(2) ~ 4,
    x == 4 ~ 5,
    x %in% c(6, 7, 10, 11, 12, 13, 14, 15) ~ 6,
    TRUE ~ convert_missing(x)
  )
}
df$ecoact20 <- map_ecoact20(df$W7TCurrentAct)

# Age 25: W8DACTIVITYC - collapsed 6-category
map_ecoact25 <- function(x) {
  case_when(
    x %in% c(1, 2, 3) ~ 1,
    x == 4 ~ 3,
    x %in% c(5, 6) ~ 4,
    x == 7 ~ 5,
    x %in% c(8, 9, 10) ~ 6,
    TRUE ~ convert_missing(x)
  )
}
df$ecoact25 <- map_ecoact25(df$W8DACTIVITYC)

# Age 32: W9DACTIVITYC - collapsed 6-category
map_ecoact32 <- function(x) {
  case_when(
    x %in% c(1, 2, 3) ~ 1,
    x == 4 ~ 3,
    x %in% c(5, 6) ~ 4,
    x == 7 ~ 5,
    x %in% c(8, 9, 10) ~ 6,
    TRUE ~ convert_missing(x)
  )
}
df$ecoact32 <- map_ecoact32(df$W9DACTIVITYC)

# Detailed variables for age 25 and 32
map_ecoactadu25 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 5 ~ 5,
    x == 6 ~ 6,
    x == 7 ~ 7,
    x == 8 ~ 8,
    x == 9 ~ 9,
    x == 10 ~ 10,
    TRUE ~ convert_missing(x)
  )
}
df$ecoactadu25 <- map_ecoactadu25(df$W8DACTIVITYC)

map_ecoactadu32 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 5 ~ 5,
    x == 6 ~ 6,
    x == 7 ~ 7,
    x == 8 ~ 8,
    x == 9 ~ 9,
    x == 10 ~ 10,
    TRUE ~ convert_missing(x)
  )
}
df$ecoactadu32 <- map_ecoactadu32(df$W9DACTIVITYC)

# Keep only final variables
final_vars <- df %>% select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Convert 6-category variables to labelled factors using labelled::to_factor
ecoact_labels <- c('1' = 'Employed (30+ hrs)', '2' = 'Employed (<30 hrs)', '3' = 'Unemployed', '4' = 'Education', '5' = 'Training', '6' = 'Other')
missing_labels <- c('-9' = 'Refused', '-8' = 'Insufficient information', '-7' = 'Prefer not to say', '-3' = 'Not asked', '-2' = 'Not applicable', '-1' = 'Item not applicable')
all_labels <- c(ecoact_labels, missing_labels)

for (var in c('ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32')) {
  final_vars[[var]] <- labelled::to_factor(final_vars[[var]], labels = all_labels)
}

# Detailed variables - keep numeric with labels for missing codes only
edu_missing_labels <- c('-9' = 'Refused', '-8' = 'Insufficient information', '-3' = 'Not asked', '-2' = 'Not applicable', '-1' = 'Item not applicable')
final_vars$ecoactadu25 <- labelled::to_factor(final_vars$ecoactadu25, labels = edu_missing_labels)
final_vars$ecoactadu32 <- labelled::to_factor(final_vars$ecoactadu32, labels = edu_missing_labels)

# Write output
write_csv(final_vars, 'data/output/cleaned_data.csv')

# Summary
print(paste('Output dimensions:', nrow(final_vars), 'rows,', ncol(final_vars), 'columns'))
print(head(final_vars))
