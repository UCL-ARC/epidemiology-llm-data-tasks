library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define base path
base <- "data/input/"

# Helper to read file
read_tab <- function(fname){
  read_delim(file.path(base, fname), delim="\t", col_types = cols())
}

# Load all files
ns9 <- read_tab("ns9_2022_derived_variables.tab")
wave4 <- read_tab("wave_four_lsype_family_background_2020.tab")
wave3 <- read_tab("wave_three_lsype_family_background_2020.tab")
wave2 <- read_tab("wave_two_lsype_family_background_2020.tab")
wave1 <- read_tab("wave_one_lsype_family_background_2020.tab")
ns8 <- read_tab("ns8_2015_main_interview.tab")
wave5 <- read_tab("wave_five_lsype_family_background_2020.tab")
wave6 <- read_tab("wave_six_lsype_young_person_2020.tab")
wave7 <- read_tab("wave_seven_lsype_young_person_2020.tab")

# Merge all datasets by NSID
merged <- reduce(list(ns9, wave4, wave3, wave2, wave1, ns8, wave5, wave6, wave7), full_join, by = "NSID")

# Function to map missing codes for sweeps 1-7
map_missing_1_7 <- function(x){
  case_when(
    x %in% c(-999,-998,-997,-995,-99) ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
}
# Function to map missing codes for sweeps 8-9 (keep -1 as -1)
map_missing_8_9 <- function(x){
  case_when(
    x %in% c(-999,-998,-997,-995,-99) ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    TRUE ~ x
  )
}

# Detailed for sweeps 1-4 (adolescent)
merged <- merged %>%
  mutate(
    hownteen14 = map_missing_1_7(as.numeric(W1hous12HH)),
    hownteen15 = map_missing_1_7(as.numeric(W2Hous12HH)),
    hownteen16 = map_missing_1_7(as.numeric(W3hous12HH)),
    hownteen17 = map_missing_1_7(as.numeric(W4Hous12HH))
  )

# Detailed for sweeps 5-7 using priority owned then rented
# Helper mapping functions
map_owned <- function(x){
  case_when(
    x %in% c(-999,-998,-997,-995,-99,-92,-91,-8,-1) ~ NA_real_,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 8,
    TRUE ~ NA_real_
  )
}
map_rented <- function(x){
  case_when(
    x %in% c(-999,-998,-997,-995,-99,-92,-91,-8,-1) ~ NA_real_,
    x == 1 ~ 4,
    x == 2 ~ 4,
    x == 3 ~ 4,
    x == 4 ~ 5,
    x == 5 ~ 8,
    TRUE ~ NA_real_
  )
}

merged <- merged %>%
  mutate(
    owned5 = map_owned(as.numeric(W5Hous12BHH)),
    rented5 = map_rented(as.numeric(W5Hous12CHH)),
    hownteen18 = coalesce(owned5, rented5),
    owned6 = map_owned(as.numeric(W6Hous12bYP)),
    rented6 = map_rented(as.numeric(W6Hous12cYP)),
    hownteen19 = coalesce(owned6, rented6),
    owned7 = map_owned(as.numeric(W7Hous12bYP)),
    rented7 = map_rented(as.numeric(W7Hous12cYP)),
    hownteen20 = coalesce(owned7, rented7)
  ) %>%
  select(-owned5, -rented5, -owned6, -rented6, -owned7, -rented7)

# Replace NA with -3 (not asked) for detailed variables
merged <- merged %>%
  mutate(
    hownteen14 = replace_na(hownteen14, -3),
    hownteen15 = replace_na(hownteen15, -3),
    hownteen16 = replace_na(hownteen16, -3),
    hownteen17 = replace_na(hownteen17, -3),
    hownteen18 = replace_na(hownteen18, -3),
    hownteen19 = replace_na(hownteen19, -3),
    hownteen20 = replace_na(hownteen20, -3)
  )

# Collapsed variables for sweeps 1-4
collapsed_map <- function(x){
  case_when(
    x %in% c(-2,-9,-1,-8) ~ x,          # keep missing codes as is
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x %in% c(4,5,6) ~ 4,
    x == 7 ~ 5,
    x == 8 ~ 6,
    TRUE ~ x
  )
}
merged <- merged %>%
  mutate(
    hown14 = collapsed_map(hownteen14),
    hown15 = collapsed_map(hownteen15),
    hown16 = collapsed_map(hownteen16),
    hown17 = collapsed_map(hownteen17)
  )

# Collapsed for sweeps 5-7 using same collapsed_map on detailed vars
merged <- merged %>%
  mutate(
    hown18 = collapsed_map(hownteen18),
    hown19 = collapsed_map(hownteen19),
    hown20 = collapsed_map(hownteen20)
  )

# Collapsed for sweeps 8-9 directly from source
# Map source to collapsed codes
collapsed_map_8_9 <- function(x){
  case_when(
    x %in% c(-2,-9,-1,-8) ~ x,
    x %in% c(1,2,3) ~ x,          # 1,2,3 stay
    x %in% c(4,5,6) ~ 4,          # Rent it
    x == 7 ~ 5,                    # Rent free
    x == 8 ~ 6,                    # Other
    TRUE ~ x
  )
}
merged <- merged %>%
  mutate(
    hown25 = collapsed_map_8_9(as.numeric(W8TENURE)),
    hown32 = collapsed_map_8_9(as.numeric(W9DTENURE))
  )

# Replace NA with -3 for collapsed variables that may be NA (e.g., if source missing)
merged <- merged %>%
  mutate(
    hown25 = replace_na(hown25, -3),
    hown32 = replace_na(hown32, -3)
  )

# Select final variables
final <- merged %>%
  select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20,
         hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write output
write_csv(final, file.path("data/output/cleaned_data.csv"))
