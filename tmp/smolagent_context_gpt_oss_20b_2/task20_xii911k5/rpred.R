# Load required packages
library(readr)
library(dplyr)
library(tidyr)

# Define file paths
input_dir <- "data/input/"

# Read all input files
wave1  <- read_delim(file.path(input_dir, "wave_one_lsype_young_person_2020.tab"),  delim = "\t", col_types = cols())
wave2  <- read_delim(file.path(input_dir, "wave_two_lsype_young_person_2020.tab"),  delim = "\t", col_types = cols())
wave3  <- read_delim(file.path(input_dir, "wave_three_lsype_young_person_2020.tab"), delim = "\t", col_types = cols())
wave4  <- read_delim(file.path(input_dir, "wave_four_lsype_young_person_2020.tab"),  delim = "\t", col_types = cols())
wave6  <- read_delim(file.path(input_dir, "wave_six_lsype_young_person_2020.tab"),  delim = "\t", col_types = cols())
wave7  <- read_delim(file.path(input_dir, "wave_seven_lsype_young_person_2020.tab"),  delim = "\t", col_types = cols())
wave8  <- read_delim(file.path(input_dir, "ns8_2015_self_completion.tab"),          delim = "\t", col_types = cols())
wave9  <- read_delim(file.path(input_dir, "ns9_2022_main_interview.tab"),            delim = "\t", col_types = cols())

# Merge all datasets on NSID
merged_df <- wave1 %>%
  full_join(wave2,  by = "NSID") %>%
  full_join(wave3,  by = "NSID") %>%
  full_join(wave4,  by = "NSID") %>%
  full_join(wave6,  by = "NSID") %>%
  full_join(wave7,  by = "NSID") %>%
  full_join(wave8,  by = "NSID") %>%
  full_join(wave9,  by = "NSID")

# Create drinking indicator per wave
cleaned <- merged_df %>%
  mutate(
    # Age 14: both W1alceverYP and W1alcmonYP must be 1
    d14 = case_when(
      W1alceverYP == 1 & W1alcmonYP == 1 ~ 1,
      (W1alceverYP == 2 | W1alcmonYP == 2) ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 15
    d15 = case_when(
      W2alceverYP == 1 ~ 1,
      W2alceverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 16
    d16 = case_when(
      W3alceverYP == 1 ~ 1,
      W3alceverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 17
    d17 = case_when(
      W4AlcEverYP == 1 ~ 1,
      W4AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 19
    d19 = case_when(
      W6AlcEverYP == 1 ~ 1,
      W6AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 20
    d20 = case_when(
      W7AlcEverYP == 1 ~ 1,
      W7AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 25
    d25 = case_when(
      W8AUDIT1 > 1 ~ 1,
      W8AUDIT1 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 32
    d32 = case_when(
      W9AUDIT1 > 1 ~ 1,
      W9AUDIT1 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Determine earliest age of alcohol consumption
final_df <- cleaned %>%
  rowwise() %>%
  mutate(
    alcfst_val = {
      drinks <- c(d14, d15, d16, d17, d19, d20, d25, d32)
      ages   <- c(14, 15, 16, 17, 19, 20, 25, 32)
      if(any(drinks == 1, na.rm = TRUE)){
        min(ages[drinks == 1])
      } else {
        if(all(drinks == 0, na.rm = TRUE) && all(!is.na(drinks))){
          99
        } else {
          -8
        }
      }
    }
  ) %>%
  ungroup() %>%
  mutate(
    alcfst = factor(alcfst_val,
      levels = c(14,15,16,17,19,20,25,32,99,-8),
      labels = c("Age 14","Age 15","Age 16","Age 17","Age 19","Age 20","Age 25","Age 32","Never had alcohol","Don\'t know/insufficient information"))
  ) %>%
  select(NSID, alcfst)

# Write output
write_csv(final_df, file = "data/output/cleaned_data.csv")
