library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Helper function to map missing values by label meaning
map_missing <- function(var, labels) {
  ifelse(var %in% names(labels), -3, var)
}

# Map W4empsYP (wave4/age17) to ecoact17
merged <- merged %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP == 1 | W4empsYP == 2 ~ 1,  # In paid work
      W4empsYP == 4 ~ 2,                  # On a training course or scheme
      W4empsYP == 5 ~ 3,                  # In full-time education
      W4empsYP == 3 ~ 4,                  # Unemployed
      W4empsYP == 6 ~ 5,                  # Looking after family
      W4empsYP == 7 | W4empsYP == 8 | W4empsYP == 9 ~ 6,  # Other
      TRUE ~ map_missing(W4empsYP, c(-999, -94, -92, -91))
    )
  )

# Map W5mainactYP (wave5/age18) to ecoact18
merged <- merged %>%
  mutate(
    ecoact18 = case_when(
      W5mainactYP == 3 ~ 1,  # In paid work
      W5mainactYP == 1 | W5mainactYP == 2 | W5mainactYP == 5 | W5mainactYP == 6 ~ 2,  # Apprenticeship/training
      W5mainactYP == 4 ~ 3,  # In education
      W5mainactYP == 7 ~ 4,  # Unemployed
      W5mainactYP == 8 ~ 5,  # Looking after family
      W5mainactYP == 9 | W5mainactYP == 10 | W5mainactYP == 11 ~ 6,  # Other
      TRUE ~ map_missing(W5mainactYP, c(-94))
    )
  )

# Map W6TCurrentAct (wave6/age19) to ecoact19
merged <- merged %>%
  mutate(
    ecoact19 = case_when(
      W6TCurrentAct == 3 ~ 1,  # In paid work
      W6TCurrentAct == 4 | W6TCurrentAct == 5 ~ 2,  # Training/apprenticeship
      W6TCurrentAct == 1 | W6TCurrentAct == 2 ~ 3,  # Education
      W6TCurrentAct == 8 ~ 4,  # Unemployed
      W6TCurrentAct == 7 ~ 5,  # Looking after family
      W6TCurrentAct == 6 | W6TCurrentAct == 9 | W6TCurrentAct == 10 | W6TCurrentAct == 11 ~ 6,  # Other
      TRUE ~ map_missing(W6TCurrentAct, c(-91))
    )
  )

# Map W7TCurrentAct (wave7/age20) to ecoact20
merged <- merged %>%
  mutate(
    ecoact20 = case_when(
      W7TCurrentAct == 3 ~ 1,  # Paid work
      W7TCurrentAct == 4 | W7TCurrentAct == 5 | W7TCurrentAct == 11 ~ 2,  # Training/apprenticeship
      W7TCurrentAct == 1 | W7TCurrentAct == 2 ~ 3,  # Education
      W7TCurrentAct == 8 ~ 4,  # Unemployed
      W7TCurrentAct == 7 ~ 5,  # Looking after family
      W7TCurrentAct == 6 | W7TCurrentAct == 9 | W7TCurrentAct == 10 | W7TCurrentAct == 12 | W7TCurrentAct == 13 | W7TCurrentAct == 14 | W7TCurrentAct == 15 ~ 6,  # Other
      TRUE ~ map_missing(W7TCurrentAct, c(-91))
    )
  )

# Map W8DACTIVITYC (wave8/age25) to ecoact25 and ecoactadu25
merged <- merged %>%
  mutate(
    ecoact25 = case_when(
      W8DACTIVITYC == 1 | W8DACTIVITYC == 2 ~ 1,  # In paid work
      W8DACTIVITYC == 6 | W8DACTIVITYC == 7 ~ 2,  # Apprenticeship/training
      W8DACTIVITYC == 5 ~ 3,  # Education
      W8DACTIVITYC == 4 ~ 4,  # Unemployed
      W8DACTIVITYC == 9 ~ 5,  # Looking after family
      W8DACTIVITYC == 3 | W8DACTIVITYC == 8 | W8DACTIVITYC == 10 ~ 6,  # Other
      TRUE ~ map_missing(W8DACTIVITYC, c(-9, -8, -1))
    ),
    ecoactadu25 = W8DACTIVITYC  # Keep original 10 categories
  )

# Map W9DACTIVITYC (wave9/age32) to ecoact32 and ecoactadu32
merged <- merged %>%
  mutate(
    ecoact32 = case_when(
      W9DACTIVITYC == 1 | W9DACTIVITYC == 2 ~ 1,  # In paid work
      W9DACTIVITYC == 6 | W9DACTIVITYC == 7 ~ 2,  # Apprenticeship/training
      W9DACTIVITYC == 5 ~ 3,  # Education
      W9DACTIVITYC == 4 ~ 4,  # Unemployed
      W9DACTIVITYC == 9 ~ 5,  # Looking after family
      W9DACTIVITYC == 3 | W9DACTIVITYC == 8 | W9DACTIVITYC == 10 ~ 6,  # Other
      TRUE ~ map_missing(W9DACTIVITYC, c(-9, -8, -1))
    ),
    ecoactadu32 = W9DACTIVITYC  # Keep original 10 categories
  )

# Select only requested variables
output <- merged %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

# Return path to output
"data/output/cleaned_data.csv"
