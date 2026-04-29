# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  "wave_one_lsype_young_person_2020.tab" = "data/input/wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab" = "data/input/wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab" = "data/input/wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab" = "data/input/wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab" = "data/input/wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab" = "data/input/ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab" = "data/input/ns9_2022_derived_variables.tab"
)

# Load each dataset
wave_one <- readr::read_delim(files[[1]], delim = "\t")
wave_four <- readr::read_delim(files[[2]], delim = "\t")
wave_five <- readr::read_delim(files[[3]], delim = "\t")
wave_six <- readr::read_delim(files[[4]], delim = "\t")
wave_seven <- readr::read_delim(files[[5]], delim = "\t")
wave_eight <- readr::read_delim(files[[6]], delim = "\t")
wave_nine <- readr::read_delim(files[[7]], delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Standard missing value codes
standard_missing <- function(df) {
  df <- df %>%
    mutate(across(where(is.numeric), ~ case_when(
      .x == -999 | .x == -998 | .x == -997 | .x == -995 ~ -2,
      .x == -94 ~ -8,
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -99 ~ -3,
      is.na(.x) ~ -3,
      TRUE ~ .x
    )))
  return(df)
}

merged_data <- standard_missing(merged_data)

# Harmonize economic activity variables
# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(ecoact17 = case_when(
    W4empsYP == 1 ~ 1,
    W4empsYP == 2 ~ 1,
    W4empsYP == 3 ~ 2,
    W4empsYP == 4 ~ 3,
    W4empsYP == 5 ~ 4,
    W4empsYP == 6 ~ 5,
    W4empsYP == 7 ~ 6,
    W4empsYP == 8 ~ 7,
    W4empsYP == 9 ~ 8,
    TRUE ~ W4empsYP
  ))

# Wave 5 (Age 18)
merged_data <- merged_data %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 1 ~ 1,
    W5mainactYP == 2 ~ 1,
    W5mainactYP == 3 ~ 1,
    W5mainactYP == 4 ~ 4,
    W5mainactYP == 5 ~ 3,
    W5mainactYP == 6 ~ 3,
    W5mainactYP == 7 ~ 2,
    W5mainactYP == 8 ~ 5,
    W5mainactYP == 9 ~ 6,
    W5mainactYP == 10 ~ 6,
    W5mainactYP == 11 ~ 6,
    TRUE ~ W5mainactYP
  ))

# Wave 6 (Age 19)
merged_data <- merged_data %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 1 ~ 4,
    W6TCurrentAct == 2 ~ 4,
    W6TCurrentAct == 3 ~ 1,
    W6TCurrentAct == 4 ~ 3,
    W6TCurrentAct == 5 ~ 1,
    W6TCurrentAct == 6 ~ 6,
    W6TCurrentAct == 7 ~ 5,
    W6TCurrentAct == 8 ~ 2,
    W6TCurrentAct == 9 ~ 6,
    W6TCurrentAct == 10 ~ 1,
    W6TCurrentAct == 11 ~ 5,
    TRUE ~ W6TCurrentAct
  ))

# Wave 7 (Age 20)
merged_data <- merged_data %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 1 ~ 4,
    W7TCurrentAct == 2 ~ 4,
    W7TCurrentAct == 3 ~ 1,
    W7TCurrentAct == 4 ~ 3,
    W7TCurrentAct == 5 ~ 1,
    W7TCurrentAct == 6 ~ 6,
    W7TCurrentAct == 7 ~ 5,
    W7TCurrentAct == 8 ~ 2,
    W7TCurrentAct == 9 ~ 1,
    W7TCurrentAct == 10 ~ 5,
    W7TCurrentAct == 11 ~ 3,
    W7TCurrentAct == 12 ~ 6,
    W7TCurrentAct == 13 ~ 6,
    W7TCurrentAct == 14 ~ 7,
    W7TCurrentAct == 15 ~ 8,
    TRUE ~ W7TCurrentAct
  ))

# Wave 8 (Age 25)
merged_data <- merged_data %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1 ~ 1,
    W8DACTIVITYC == 2 ~ 1,
    W8DACTIVITYC == 3 ~ 5,
    W8DACTIVITYC == 4 ~ 2,
    W8DACTIVITYC == 5 ~ 4,
    W8DACTIVITYC == 6 ~ 1,
    W8DACTIVITYC == 7 ~ 3,
    W8DACTIVITYC == 8 ~ 7,
    W8DACTIVITYC == 9 ~ 5,
    W8DACTIVITYC == 10 ~ 8,
    TRUE ~ W8DACTIVITYC
  ))

# Wave 9 (Age 32)
merged_data <- merged_data %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1 ~ 1,
    W9DACTIVITYC == 2 ~ 1,
    W9DACTIVITYC == 3 ~ 5,
    W9DACTIVITYC == 4 ~ 2,
    W9DACTIVITYC == 5 ~ 4,
    W9DACTIVITYC == 6 ~ 1,
    W9DACTIVITYC == 7 ~ 3,
    W9DACTIVITYC == 8 ~ 7,
    W9DACTIVITYC == 9 ~ 5,
    W9DACTIVITYC == 10 ~ 8,
    TRUE ~ W9DACTIVITYC
  ))

# Detailed adult variables for waves 8 and 9
merged_data <- merged_data %>%
  mutate(ecoactadu25 = W8DACTIVITYC,
         ecoactadu32 = W9DACTIVITYC)

# Convert harmonized variables to factors
merged_data <- merged_data %>%
  mutate(
    ecoact17 = factor(ecoact17, levels = 1:8, labels = c(
      "In paid work (30+ hrs)", "In paid work (<30 hrs)", "Unemployed",
      "On a training course", "In education", "Looking after family",
      "Retired", "Sick/disabled"
    )),
    ecoact18 = factor(ecoact18, levels = 1:6, labels = c(
      "In paid work", "Unemployed", "On a training course",
      "In education", "Looking after family", "Waiting"
    )),
    ecoact19 = factor(ecoact19, levels = 1:6, labels = c(
      "In paid work", "Unemployed", "On a training course",
      "In education", "Looking after family", "Waiting"
    )),
    ecoact20 = factor(ecoact20, levels = 1:8, labels = c(
      "In paid work", "Unemployed", "On a training course",
      "In education", "Looking after family", "Waiting", "Retired", "Sick/disabled"
    )),
    ecoact25 = factor(ecoact25, levels = 1:8, labels = c(
      "In paid work", "Unemployed", "On a training course",
      "In education", "Looking after family", "Waiting", "Retired", "Sick/disabled"
    )),
    ecoact32 = factor(ecoact32, levels = 1:8, labels = c(
      "In paid work", "Unemployed", "On a training course",
      "In education", "Looking after family", "Waiting", "Retired", "Sick/disabled"
    ))
  )

# Convert detailed adult variables to factors
merged_data <- merged_data %>%
  mutate(
    ecoactadu25 = factor(ecoactadu25, levels = 1:10, labels = c(
      "Employee - in paid work", "Self employed", "In unpaid/voluntary work",
      "Unemployed", "Education: School/college/university", "Apprenticeship",
      "On gov't scheme for employment training", "Sick or disabled",
      "Looking after home or family", "Something else"
    )),
    ecoactadu32 = factor(ecoactadu32, levels = 1:10, labels = c(
      "Employee - in paid work", "Self employed", "In unpaid/voluntary work",
      "Unemployed", "Education: School/college/university", "Apprenticeship",
      "On gov't scheme for employment training", "Sick or disabled",
      "Looking after home or family", "Something else"
    ))
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Save to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)