library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets using NSID
merged <- w1 %>% full_join(w2, by = "NSID") %>% full_join(w3, by = "NSID") %>% full_join(w4, by = "NSID") %>% full_join(w5, by = "NSID") %>% full_join(w6, by = "NSID") %>% full_join(w7, by = "NSID") %>% full_join(w8, by = "NSID") %>% full_join(w9, by = "NSID")

# Define mapping functions for missing values
harmonise_missing_sweeps1_4 <- function(x) {
  case_when(
    x == -999 | x == -998 | x == -997 | x == -995 | x == -99 | x == -92 ~ -2,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

harmonise_missing_sweeps5_7 <- function(x) {
  case_when(
    x == -999 | x == -92 ~ -2,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

harmonise_missing_sweeps8_9 <- function(x) {
  case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ x
  )
}

# Detailed 8-category variables for sweeps 1-4
merged <- merged %>% mutate(
  hownteen14 = harmonise_missing_sweeps1_4(W1hous12HH),
  hownteen15 = harmonise_missing_sweeps1_4(W2Hous12HH),
  hownteen16 = harmonise_missing_sweeps1_4(W3hous12HH),
  hownteen17 = harmonise_missing_sweeps1_4(W4Hous12HH)
)

# Collapsed 6-category variables for sweeps 1-4
merged <- merged %>% mutate(
  hown14 = case_when(
    hownteen14 %in% c(4, 5, 6) ~ 4,
    hownteen14 == 8 ~ 6,
    TRUE ~ hownteen14
  ),
  hown15 = case_when(
    hownteen15 %in% c(4, 5, 6) ~ 4,
    hownteen15 == 8 ~ 6,
    TRUE ~ hownteen15
  ),
  hown16 = case_when(
    hownteen16 %in% c(4, 5, 6) ~ 4,
    hownteen16 == 8 ~ 6,
    TRUE ~ hownteen16
  ),
  hown17 = case_when(
    hownteen17 %in% c(4, 5, 6) ~ 4,
    hownteen17 == 8 ~ 6,
    TRUE ~ hownteen17
  )
)

# Function to derive tenure for sweeps 5-7
derive_tenure_sweeps5_7 <- function(type_var, owned_var, rented_var) {
  type_var <- harmonise_missing_sweeps5_7(type_var)
  owned_var <- harmonise_missing_sweeps5_7(owned_var)
  rented_var <- harmonise_missing_sweeps5_7(rented_var)

  case_when(
    !is.na(owned_var) & owned_var > 0 ~ owned_var,
    !is.na(rented_var) & rented_var > 0 ~ rented_var + 3,
    type_var == 3 ~ 8,
    TRUE ~ ifelse(!is.na(type_var), type_var, NA_real_)
  )
}

# Detailed variables for sweeps 5-7
merged <- merged %>% mutate(
  hownteen18 = derive_tenure_sweeps5_7(W5Hous12HH, W5Hous12BHH, W5Hous12CHH),
  hownteen19 = derive_tenure_sweeps5_7(W6Hous12YP, W6Hous12bYP, W6Hous12cYP),
  hownteen20 = derive_tenure_sweeps5_7(W7Hous12YP, W7Hous12bYP, W7Hous12cYP)
)

# Collapsed variables for sweeps 5-7
merged <- merged %>% mutate(
  hown18 = case_when(
    hownteen18 %in% c(4, 5, 6) ~ 4,
    hownteen18 == 8 ~ 6,
    TRUE ~ hownteen18
  ),
  hown19 = case_when(
    hownteen19 %in% c(4, 5, 6) ~ 4,
    hownteen19 == 8 ~ 6,
    TRUE ~ hownteen19
  ),
  hown20 = case_when(
    hownteen20 %in% c(4, 5, 6) ~ 4,
    hownteen20 == 8 ~ 6,
    TRUE ~ hownteen20
  )
)

# Detailed variables for sweeps 8-9
merged <- merged %>% mutate(
  hownteen25 = harmonise_missing_sweeps8_9(W8TENURE),
  hownteen32 = harmonise_missing_sweeps8_9(W9DTENURE)
)

# Collapsed variables for sweeps 8-9
merged <- merged %>% mutate(
  hown25 = case_when(
    hownteen25 == 6 | hownteen25 == 7 ~ 6,
    TRUE ~ hownteen25
  ),
  hown32 = case_when(
    hownteen32 == 6 | hownteen32 == 7 ~ 6,
    TRUE ~ hownteen32
  )
)

# Select only the required variables
output <- merged %>% select(NSID, starts_with("hown"))

# Write the output file
write_csv(output, "data/output/cleaned_data.csv")
