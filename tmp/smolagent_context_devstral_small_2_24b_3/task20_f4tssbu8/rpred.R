library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define a function to map missing values to standard codes
harmonise_missing <- function(var, wave) {
  case_when(
    var %in% c(-999, -998, -997, -995, -996) ~ -2,
    var == -99 ~ -3,
    var == -97 ~ -2,
    var == -96 ~ -2,
    var == -92 ~ -9,
    var == -91 ~ -1,
    var == -8 ~ -8,
    var == -3 ~ -3,
    var == -1 ~ -8,
    TRUE ~ var
  )
}

# Apply missing value harmonisation to all relevant variables
merged_data <- merged_data %>%
  mutate(
    W1alceverYP = harmonise_missing(W1alceverYP, "wave1"),
    W1alcmonYP = harmonise_missing(W1alcmonYP, "wave1"),
    W2alceverYP = harmonise_missing(W2alceverYP, "wave2"),
    W3alceverYP = harmonise_missing(W3alceverYP, "wave3"),
    W4AlcEverYP = harmonise_missing(W4AlcEverYP, "wave4"),
    W6AlcEverYP = harmonise_missing(W6AlcEverYP, "wave6"),
    W7AlcEverYP = harmonise_missing(W7AlcEverYP, "wave7"),
    W8AUDIT1 = harmonise_missing(W8AUDIT1, "wave8"),
    W9AUDIT1 = harmonise_missing(W9AUDIT1, "wave9")
  )

# Determine the earliest age of drinking
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(
    alcfst = case_when(
      !is.na(W1alceverYP) & !is.na(W1alcmonYP) & W1alceverYP == 1 & W1alcmonYP == 1 ~ 14,
      is.na(W1alceverYP) | is.na(W1alcmonYP) ~ NA_real_,
      !is.na(W2alceverYP) & W2alceverYP == 1 ~ 15,
      is.na(W2alceverYP) ~ NA_real_,
      !is.na(W3alceverYP) & W3alceverYP == 1 ~ 16,
      is.na(W3alceverYP) ~ NA_real_,
      !is.na(W4AlcEverYP) & W4AlcEverYP == 1 ~ 17,
      is.na(W4AlcEverYP) ~ NA_real_,
      !is.na(W6AlcEverYP) & W6AlcEverYP == 1 ~ 19,
      is.na(W6AlcEverYP) ~ NA_real_,
      !is.na(W7AlcEverYP) & W7AlcEverYP == 1 ~ 20,
      is.na(W7AlcEverYP) ~ NA_real_,
      !is.na(W8AUDIT1) & W8AUDIT1 > 1 ~ 25,
      is.na(W8AUDIT1) ~ NA_real_,
      !is.na(W9AUDIT1) & W9AUDIT1 > 1 ~ 32,
      is.na(W9AUDIT1) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Determine the final alcfst value based on the earliest age of drinking
merged_data <- merged_data %>%
  group_by(NSID) %>%
  summarise(
    alcfst = case_when(
      all(is.na(alcfst)) ~ -8,
      all(alcfst %in% c(2), na.rm = TRUE) ~ 99,
      TRUE ~ ifelse(all(is.na(alcfst)), -8, min(alcfst, na.rm = TRUE))
    )
  ) %>%
  ungroup()

# Convert alcfst to a factor with appropriate levels and labels
merged_data$alcfst <- factor(
  merged_data$alcfst,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")
)

# Write the output file
write_csv(merged_data, "data/output/cleaned_data.csv")