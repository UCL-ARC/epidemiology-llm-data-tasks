
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Rename columns to avoid conflicts
wave_two <- wave_two %>% rename(urbind_w2 = urbind, gor_w2 = gor)
wave_three <- wave_three %>% rename(urbind_w3 = urbind, gor_w3 = gor)

# Merge datasets
cohort_frame <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# Define recoding for W9NATIONRES to regint32
cohort_frame <- cohort_frame %>%
  mutate(regint32 = case_when(
    W9NATIONRES >= 1 & W9NATIONRES <= 4 ~ 1,  # England, Scotland, Wales, Northern Ireland -> 1
    W9NATIONRES == 5 ~ 2,                     # Outside of UK or unknown -> 2
    is.na(W9NATIONRES) ~ -3,                  # NA -> -3
    TRUE ~ W9NATIONRES
  ))

# Define missing value mapping function for vectors
map_missing_values <- function(x) {
  ifelse(x == 13, -2, x)  # Map 13 to -2 as per requirements
}

# Derive regub15 (Urban/Rural at age 15)
cohort_frame <- cohort_frame %>%
  mutate(regub15 = case_when(
    urbind_w2 %in% c(1:8) ~ urbind_w2,
    urbind_w2 == -94 ~ -94,
    urbind_w2 == -999 ~ -999,
    is.na(urbind_w2) ~ -3
  )) %>%
  mutate(regub15 = map_missing_values(regub15))

# Derive regov15 (Government Office Region at age 15)
cohort_frame <- cohort_frame %>%
  mutate(regov15 = case_when(
    gor_w2 %in% c(1:9) ~ gor_w2,
    gor_w2 == -94 ~ -94,
    gor_w2 == -999 ~ -999,
    is.na(gor_w2) ~ -3
  )) %>%
  mutate(regov15 = map_missing_values(regov15))

# Derive regub16 (Urban/Rural at age 16)
cohort_frame <- cohort_frame %>%
  mutate(regub16 = case_when(
    urbind_w3 %in% c(1:8) ~ urbind_w3,
    urbind_w3 == -94 ~ -94,
    urbind_w3 == -999 ~ -999,
    is.na(urbind_w3) ~ -3
  )) %>%
  mutate(regub16 = map_missing_values(regub16))

# Derive regov16 (Government Office Region at age 16)
cohort_frame <- cohort_frame %>%
  mutate(regov16 = case_when(
    gor_w3 %in% c(1:9) ~ gor_w3,
    gor_w3 == -94 ~ -94,
    gor_w3 == -999 ~ -999,
    is.na(gor_w3) ~ -3
  )) %>%
  mutate(regov16 = map_missing_values(regov16))

# Derive regor25 (UK Region at age 25)
cohort_frame <- cohort_frame %>%
  mutate(regor25 = case_when(
    W8DGOR %in% c(1:12) ~ W8DGOR,
    W8DGOR == 13 ~ -2,
    is.na(W8DGOR) ~ -3
  ))

# Derive regor32 (UK Region at age 32)
cohort_frame <- cohort_frame %>%
  mutate(regor32 = case_when(
    W9DRGN %in% c(1:12) ~ W9DRGN,
    W9DRGN == 13 ~ -2,
    is.na(W9DRGN) ~ -3
  ))

# Select only the ID and derived variables for the final output
final_output <- cohort_frame %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write the final output to CSV
write_csv(final_output, "data/output/cleaned_data.csv")
