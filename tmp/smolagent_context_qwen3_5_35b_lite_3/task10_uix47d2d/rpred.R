library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID
clean_data <- full_join(wave1, wave4, by = 'NSID')
clean_data <- full_join(clean_data, wave5, by = 'NSID')
clean_data <- full_join(clean_data, wave6, by = 'NSID')
clean_data <- full_join(clean_data, wave7, by = 'NSID')
clean_data <- full_join(clean_data, wave8, by = 'NSID')
clean_data <- full_join(clean_data, wave9, by = 'NSID')

# Helper function for standard missing value conversion
standardise_missing <- function(x) {
  x <- as.integer(x)
  x[x == -999] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -97] <- -1
  x[x == -100] <- -1
  x[x == -99] <- -3
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  return(x)
}

# Create economic activity variables using mutate on the merged dataframe
clean_data <- clean_data %>%
  mutate(
    # Age 17 - W4empsYP (Sweep 4)
    ecoact17_raw = standardise_missing(W4empsYP),
    ecoact17 = case_when(
      ecoact17_raw %in% c(1, 2) ~ 1,
      ecoact17_raw %in% c(5) ~ 2,
      ecoact17_raw %in% c(4) ~ 3,
      ecoact17_raw %in% c(3) ~ 4,
      ecoact17_raw %in% c(6, 9) ~ 5,
      ecoact17_raw %in% c(7, 8) ~ 6,
      ecoact17_raw %in% c(-1, -2, -3, -8, -9) ~ ecoact17_raw,
      TRUE ~ ecoact17_raw
    ) %>% as.integer(),
    
    # Age 18 - W5mainactYP (Sweep 5)
    ecoact18_raw = standardise_missing(W5mainactYP),
    ecoact18 = case_when(
      ecoact18_raw %in% c(1, 2, 3) ~ 1,
      ecoact18_raw %in% c(4) ~ 2,
      ecoact18_raw %in% c(5, 6) ~ 3,
      ecoact18_raw %in% c(7) ~ 4,
      ecoact18_raw %in% c(8) ~ 5,
      ecoact18_raw %in% c(9, 10, 11) ~ 6,
      ecoact18_raw %in% c(-1, -2, -3, -8, -9) ~ ecoact18_raw,
      TRUE ~ ecoact18_raw
    ) %>% as.integer(),
    
    # Age 19 - W6TCurrentAct (Sweep 6)
    ecoact19_raw = standardise_missing(W6TCurrentAct),
    ecoact19 = case_when(
      ecoact19_raw %in% c(3, 5, 10, 11) ~ 1,
      ecoact19_raw %in% c(1, 2, 9) ~ 2,
      ecoact19_raw %in% c(4) ~ 3,
      ecoact19_raw %in% c(8) ~ 4,
      ecoact19_raw %in% c(7) ~ 5,
      ecoact19_raw %in% c(6) ~ 6,
      ecoact19_raw %in% c(-1, -2, -3, -8, -9) ~ ecoact19_raw,
      TRUE ~ ecoact19_raw
    ) %>% as.integer(),
    
    # Age 20 - W7TCurrentAct (Sweep 7)
    ecoact20_raw = standardise_missing(W7TCurrentAct),
    ecoact20 = case_when(
      ecoact20_raw %in% c(3, 10, 11, 13, 14, 15) ~ 1,
      ecoact20_raw %in% c(1, 2, 9) ~ 2,
      ecoact20_raw %in% c(4, 5) ~ 3,
      ecoact20_raw %in% c(8) ~ 4,
      ecoact20_raw %in% c(7) ~ 5,
      ecoact20_raw %in% c(6, 12) ~ 6,
      ecoact20_raw %in% c(-1, -2, -3, -8, -9) ~ ecoact20_raw,
      TRUE ~ ecoact20_raw
    ) %>% as.integer(),
    
    # Age 25 - W8DACTIVITYC (Sweep 8)
    ecoact25_raw = standardise_missing(W8DACTIVITYC),
    ecoact25 = case_when(
      ecoact25_raw %in% c(1, 2, 3) ~ 1,
      ecoact25_raw %in% c(5, 6) ~ 2,
      ecoact25_raw %in% c(7) ~ 3,
      ecoact25_raw %in% c(4) ~ 4,
      ecoact25_raw %in% c(9) ~ 5,
      ecoact25_raw %in% c(8, 10) ~ 6,
      ecoact25_raw %in% c(-1, -2, -3, -8, -9) ~ ecoact25_raw,
      TRUE ~ ecoact25_raw
    ) %>% as.integer(),
    
    # Age 32 - W9DACTIVITYC (Sweep 9)
    ecoact32_raw = standardise_missing(W9DACTIVITYC),
    ecoact32 = case_when(
      ecoact32_raw %in% c(1, 2, 3) ~ 1,
      ecoact32_raw %in% c(5, 6) ~ 2,
      ecoact32_raw %in% c(7) ~ 3,
      ecoact32_raw %in% c(4) ~ 4,
      ecoact32_raw %in% c(9) ~ 5,
      ecoact32_raw %in% c(8, 10) ~ 6,
      ecoact32_raw %in% c(-1, -2, -3, -8, -9) ~ ecoact32_raw,
      TRUE ~ ecoact32_raw
    ) %>% as.integer(),
    
    # Detailed adult variables
    ecoactadu25 = standardise_missing(W8DACTIVITYC),
    ecoactadu32 = standardise_missing(W9DACTIVITYC)
  )

# Remove intermediate raw variables and keep only final derived variables
clean_data <- clean_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Create output directory if it doesn't exist
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)

# Write to output
write_csv(clean_data, 'data/output/cleaned_data.csv')

cat('Successfully created cleaned_data.csv\n')
cat('Number of rows:', nrow(clean_data), '\n')
cat('Number of columns:', ncol(clean_data), '\n')
cat('Columns:', names(clean_data), '\n')
