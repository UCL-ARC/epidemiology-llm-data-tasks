library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from the metadata
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, labels) {
  if (is.numeric(var)) {
    var <- ifelse(var == -92, -9, var)
    var <- ifelse(var == -91, -1, var)
    var <- ifelse(var == -99, -3, var)
    var <- ifelse(var == -999 | var == -998 | var == -997 | var == -995, -2, var)
    var <- ifelse(var == -94, -8, var)
    var <- ifelse(var == -1, -1, var)
    var <- ifelse(is.na(var), -3, var)
  }
  return(var)
}

# Function to harmonize housing tenure categories
harmonize_tenure <- function(var, wave) {
  if (wave == 14) {
    var <- map_missing(var, labels = c("-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 3,
      var == 4 ~ 4,
      var == 5 ~ 4,
      var == 6 ~ 4,
      var == 7 ~ 5,
      var == 8 ~ 6,
      TRUE ~ var
    )
  } else if (wave == 15) {
    var <- map_missing(var, labels = c("-998" = "Interviewer missed question", "-997" = "Script error", "-995" = "Missing history section data", "-99" = "Missing household grid", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 3,
      var == 4 ~ 4,
      var == 5 ~ 4,
      var == 6 ~ 4,
      var == 7 ~ 5,
      var == 8 ~ 6,
      TRUE ~ var
    )
  } else if (wave == 16) {
    var <- map_missing(var, labels = c("-999" = "HH grid missing", "-99" = "", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 3,
      var == 4 ~ 4,
      var == 5 ~ 4,
      var == 6 ~ 4,
      var == 7 ~ 5,
      var == 8 ~ 6,
      TRUE ~ var
    )
  } else if (wave == 17) {
    var <- map_missing(var, labels = c("-999" = "Missing household grid", "-997" = "Script error", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 3,
      var == 4 ~ 4,
      var == 5 ~ 4,
      var == 6 ~ 4,
      var == 7 ~ 5,
      var == 8 ~ 6,
      TRUE ~ var
    )
  } else if (wave == 19) {
    var <- map_missing(var, labels = c("-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 6,
      TRUE ~ var
    )
  } else if (wave == 20) {
    var <- map_missing(var, labels = c("-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 6,
      TRUE ~ var
    )
  } else if (wave == 25) {
    var <- map_missing(var, labels = c("-9" = "Refused", "-8" = "Don't know", "-1" = "Not applicable"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 3,
      var == 4 ~ 4,
      var == 5 ~ 5,
      var == 6 ~ 6,
      var == 7 ~ 6,
      TRUE ~ var
    )
  } else if (wave == 32) {
    var <- map_missing(var, labels = c("-8" = "Insufficient information"))
    var <- case_when(
      var == 1 ~ 1,
      var == 2 ~ 2,
      var == 3 ~ 3,
      var == 4 ~ 4,
      var == 5 ~ 5,
      var == 6 ~ 6,
      var == 7 ~ 6,
      TRUE ~ var
    )
  }
  return(var)
}

# Create detailed time-varying housing tenure variables for ages 14-20
merged_data <- merged_data %>%
  mutate(
    hownteen14 = harmonize_tenure(W1hous12HH, 14),
    hownteen15 = harmonize_tenure(W2Hous12HH, 15),
    hownteen16 = harmonize_tenure(W3hous12HH, 16),
    hownteen17 = harmonize_tenure(W4Hous12HH, 17),
    hownteen19 = harmonize_tenure(W6Hous12YP, 19),
    hownteen20 = harmonize_tenure(W7Hous12YP, 20)
  )

# Create collapsed time-varying housing tenure variables for ages 14-32
merged_data <- merged_data %>%
  mutate(
    hown14 = case_when(
      hownteen14 == 1 ~ 1,
      hownteen14 == 2 ~ 2,
      hownteen14 == 3 ~ 3,
      hownteen14 == 4 ~ 4,
      hownteen14 == 5 ~ 5,
      hownteen14 == 6 ~ 6,
      TRUE ~ hownteen14
    ),
    hown15 = case_when(
      hownteen15 == 1 ~ 1,
      hownteen15 == 2 ~ 2,
      hownteen15 == 3 ~ 3,
      hownteen15 == 4 ~ 4,
      hownteen15 == 5 ~ 5,
      hownteen15 == 6 ~ 6,
      TRUE ~ hownteen15
    ),
    hown16 = case_when(
      hownteen16 == 1 ~ 1,
      hownteen16 == 2 ~ 2,
      hownteen16 == 3 ~ 3,
      hownteen16 == 4 ~ 4,
      hownteen16 == 5 ~ 5,
      hownteen16 == 6 ~ 6,
      TRUE ~ hownteen16
    ),
    hown17 = case_when(
      hownteen17 == 1 ~ 1,
      hownteen17 == 2 ~ 2,
      hownteen17 == 3 ~ 3,
      hownteen17 == 4 ~ 4,
      hownteen17 == 5 ~ 5,
      hownteen17 == 6 ~ 6,
      TRUE ~ hownteen17
    ),
    hown19 = case_when(
      hownteen19 == 1 ~ 1,
      hownteen19 == 2 ~ 2,
      hownteen19 == 6 ~ 6,
      TRUE ~ hownteen19
    ),
    hown20 = case_when(
      hownteen20 == 1 ~ 1,
      hownteen20 == 2 ~ 2,
      hownteen20 == 6 ~ 6,
      TRUE ~ hownteen20
    ),
    hown25 = case_when(
      harmonize_tenure(W8TENURE, 25) == 1 ~ 1,
      harmonize_tenure(W8TENURE, 25) == 2 ~ 2,
      harmonize_tenure(W8TENURE, 25) == 3 ~ 3,
      harmonize_tenure(W8TENURE, 25) == 4 ~ 4,
      harmonize_tenure(W8TENURE, 25) == 5 ~ 5,
      harmonize_tenure(W8TENURE, 25) == 6 ~ 6,
      TRUE ~ harmonize_tenure(W8TENURE, 25)
    ),
    hown32 = case_when(
      harmonize_tenure(W9DTENURE, 32) == 1 ~ 1,
      harmonize_tenure(W9DTENURE, 32) == 2 ~ 2,
      harmonize_tenure(W9DTENURE, 32) == 3 ~ 3,
      harmonize_tenure(W9DTENURE, 32) == 4 ~ 4,
      harmonize_tenure(W9DTENURE, 32) == 5 ~ 5,
      harmonize_tenure(W9DTENURE, 32) == 6 ~ 6,
      TRUE ~ harmonize_tenure(W9DTENURE, 32)
    )
  )

# Select only the ID variable and the final derived variables
final_data <- merged_data %>%
  select(NSID, starts_with("hown"))

# Write the final cleaned data to CSV
write_csv(final_data, "data/output/cleaned_data.csv")
