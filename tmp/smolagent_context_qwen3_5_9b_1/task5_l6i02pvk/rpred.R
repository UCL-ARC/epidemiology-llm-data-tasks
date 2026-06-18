library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
all_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Clean missing values in W6MarStatYP (age 19)
all_data <- all_data %>%
  mutate(
    W6MarStatYP = case_when(
      W6MarStatYP %in% c(-997, -97) ~ -2,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      is.na(W6MarStatYP) ~ -3,
      TRUE ~ as.integer(W6MarStatYP)
    )
  )

# Clean missing values in W8DMARSTAT (age 25)
all_data <- all_data %>%
  mutate(
    W8DMARSTAT = case_when(
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      is.na(W8DMARSTAT) ~ -3,
      TRUE ~ as.integer(W8DMARSTAT)
    )
  )

# Clean missing values in W9DMARSTAT (age 32)
all_data <- all_data %>%
  mutate(
    W9DMARSTAT = case_when(
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      W9DMARSTAT == -1 ~ -1,
      is.na(W9DMARSTAT) ~ -3,
      TRUE ~ as.integer(W9DMARSTAT)
    )
  )

# Create partnr19 from W6MarStatYP (collapsed harmonized)
all_data <- all_data %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP %in% c(-997, -97) ~ -2,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      is.na(W6MarStatYP) ~ -3,
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      TRUE ~ NA_integer_
    )
  )

# Create partnradu25 from W8DMARSTAT (detailed adult categories)
all_data <- all_data %>%
  mutate(
    partnradu25 = case_when(
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      is.na(W8DMARSTAT) ~ -3,
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 6 ~ 6,
      W8DMARSTAT == 7 ~ 7,
      W8DMARSTAT == 8 ~ 8,
      W8DMARSTAT == 9 ~ 9,
      TRUE ~ NA_integer_
    )
  )

# Create partnradu32 from W9DMARSTAT (detailed adult categories)
all_data <- all_data %>%
  mutate(
    partnradu32 = case_when(
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      W9DMARSTAT == -1 ~ -1,
      is.na(W9DMARSTAT) ~ -3,
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 3,
      W9DMARSTAT == 4 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 6 ~ 6,
      W9DMARSTAT == 7 ~ 7,
      W9DMARSTAT == 8 ~ 8,
      TRUE ~ NA_integer_
    )
  )

# Create partnr25 (collapsed harmonized) from partnradu25
all_data <- all_data %>%
  mutate(
    partnr25 = case_when(
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      partnradu25 == 1 ~ 1,
      partnradu25 == 2 ~ 2,
      partnradu25 == 3 ~ 3,
      partnradu25 == 4 ~ 4,
      partnradu25 == 5 ~ 5,
      partnradu25 == 6 ~ 1,
      partnradu25 == 7 ~ 3,
      partnradu25 == 8 ~ 4,
      partnradu25 == 9 ~ 5,
      is.na(partnradu25) ~ -3,
      TRUE ~ NA_integer_
    )
  )

# Create partnr32 (collapsed harmonized) from partnradu32
all_data <- all_data %>%
  mutate(
    partnr32 = case_when(
      partnradu32 == -9 ~ -9,
      partnradu32 == -8 ~ -8,
      partnradu32 == -1 ~ -1,
      partnradu32 == 1 ~ 1,
      partnradu32 == 2 ~ 2,
      partnradu32 == 3 ~ 4,
      partnradu32 == 4 ~ 3,
      partnradu32 == 5 ~ 5,
      partnradu32 == 6 ~ 1,
      partnradu32 == 7 ~ 4,
      partnradu32 == 8 ~ 5,
      is.na(partnradu32) ~ -3,
      TRUE ~ NA_integer_
    )
  )

# Remove raw source variables
all_data <- all_data %>%
  select(-W6MarStatYP, -W8DMARSTAT, -W9DMARSTAT)

# Write output
write_csv(all_data, "data/output/cleaned_data.csv")

# Simple verification
print(paste("Rows:", nrow(all_data)))
print(paste("Cols:", ncol(all_data)))
print(paste("Has partnr19:", "partnr19" %in% names(all_data)))
print(paste("Has partnr25:", "partnr25" %in% names(all_data)))
print(paste("Has partnr32:", "partnr32" %in% names(all_data)))
print(paste("Has partnradu25:", "partnradu25" %in% names(all_data)))
print(paste("Has partnradu32:", "partnradu32" %in% names(all_data)))