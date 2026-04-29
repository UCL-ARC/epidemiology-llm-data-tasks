library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Process wave4 (age 17)
wave4 <- wave4 %>%
  mutate(W4empsYP = case_when(
    W4empsYP == -999 ~ -2,
    W4empsYP == -94 ~ -8,
    W4empsYP == -92 ~ -9,
    W4empsYP == -91 ~ -1,
    W4empsYP < 0 ~ -3,
    TRUE ~ W4empsYP
  )) %>%
  mutate(ecoact17 = case_when(
    W4empsYP %in% c(1, 2) ~ 1,
    W4empsYP == 3 ~ 2,
    W4empsYP %in% c(4, 5) ~ 3,
    W4empsYP == 6 ~ 4,
    W4empsYP == 8 ~ 5,
    W4empsYP %in% c(7, 9) ~ 6,
    TRUE ~ W4empsYP
  )) %>%
  mutate(ecoact17 = labelled(ecoact17, labels = c("In paid work" = 1, "Unemployed" = 2, "Education" = 3, "Looking after home" = 4, "Sick/disabled" = 5, "Other" = 6)))

# Process wave5 (age 18)
wave5 <- wave5 %>%
  mutate(W5mainactYP = case_when(
    W5mainactYP == -94 ~ -8,
    W5mainactYP < 0 ~ -3,
    TRUE ~ W5mainactYP
  )) %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 3 ~ 1,
    W5mainactYP %in% c(1, 2, 5, 6) ~ 4,
    W5mainactYP == 4 ~ 3,
    W5mainactYP == 7 ~ 2,
    W5mainactYP == 8 ~ 5,
    W5mainactYP %in% c(9, 10, 11) ~ 6,
    TRUE ~ W5mainactYP
  )) %>%
  mutate(ecoact18 = labelled(ecoact18, labels = c("In paid work" = 1, "Unemployed" = 2, "Education" = 3, "Training" = 4, "Looking after home" = 5, "Other" = 6)))

# Process wave6 (age 19)
wave6 <- wave6 %>%
  mutate(W6TCurrentAct = case_when(
    W6TCurrentAct == -91 ~ -8,
    W6TCurrentAct < 0 ~ -3,
    TRUE ~ W6TCurrentAct
  )) %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct %in% c(3, 10) ~ 1,
    W6TCurrentAct == 8 ~ 2,
    W6TCurrentAct %in% c(1, 2) ~ 3,
    W6TCurrentAct %in% c(4,5) ~ 4,
    W6TCurrentAct == 7 ~ 5,
    W6TCurrentAct %in% c(6, 9, 11) ~ 6,
    TRUE ~ W6TCurrentAct
  )) %>%
  mutate(ecoact19 = labelled(ecoact19, labels = c("In paid work" = 1, "Unemployed" = 2, "Education" = 3, "Training" = 4, "Looking after home" = 5, "Other" = 6)))

# Process wave7 (age 20)
wave7 <- wave7 %>%
  mutate(W7TCurrentAct = case_when(
    W7TCurrentAct == -91 ~ -1,
    W7TCurrentAct < 0 ~ -3,
    TRUE ~ W7TCurrentAct
  )) %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct %in% c(3,9) ~ 1,
    W7TCurrentAct == 8 ~ 2,
    W7TCurrentAct %in% c(1,2) ~ 3,
    W7TCurrentAct %in% c(4,5,11) ~ 4,
    W7TCurrentAct == 7 ~ 5,
    W7TCurrentAct == 14 ~ 6,
    W7TCurrentAct %in% c(6,10,12,13,15) ~ 7,
    TRUE ~ W7TCurrentAct
  )) %>%
  mutate(ecoact20 = labelled(ecoact20, labels = c("In paid work" = 1, "Unemployed" = 2, "Education" = 3, "Training" = 4, "Looking after home" = 5, "Sick/disabled" = 6, "Other" = 7)))

# Process wave8
wave8 <- wave8 %>%
  mutate(W8DACTIVITYC = case_when(
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    TRUE ~ W8DACTIVITYC
  )) %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC %in% c(1,2) ~ 1,
    W8DACTIVITYC == 4 ~ 2,
    W8DACTIVITYC == 5 ~ 3,
    W8DACTIVITYC %in% c(6,7) ~ 4,
    W8DACTIVITYC == 9 ~ 5,
    W8DACTIVITYC == 8 ~ 6,
    W8DACTIVITYC %in% c(3,10) ~ 7,
    TRUE ~ W8DACTIVITYC
  )) %>%
  mutate(ecoactadu25 = W8DACTIVITYC) %>%
  mutate(ecoact25 = labelled(ecoact25, labels = c("In paid work" = 1, "Unemployed" = 2, "Education" = 3, "Training" = 4, "Looking after home" = 5, "Sick/disabled" = 6, "Other" = 7)),
         ecoactadu25 = labelled(ecoactadu25, labels = c("Employee" = 1, "Self employed" = 2, "Unpaid/voluntary" = 3, "Unemployed" = 4, "Education" = 5, "Apprenticeship" = 6, "Gov't scheme" = 7, "Sick/disabled" = 8, "Looking after home" = 9, "Other" = 10)))

# Process wave9
wave9 <- wave9 %>%
  mutate(W9DACTIVITYC = case_when(
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    TRUE ~ W9DACTIVITYC
  )) %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC %in% c(1,2) ~ 1,
    W9DACTIVITYC == 4 ~ 2,
    W9DACTIVITYC == 5 ~ 3,
    W9DACTIVITYC %in% c(6,7) ~ 4,
    W9DACTIVITYC == 9 ~ 5,
    W9DACTIVITYC == 8 ~ 6,
    W9DACTIVITYC %in% c(3,10) ~ 7,
    TRUE ~ W9DACTIVITYC
  )) %>%
  mutate(ecoactadu32 = W9DACTIVITYC) %>%
  mutate(ecoact32 = labelled(ecoact32, labels = c("In paid work" = 1, "Unemployed" = 2, "Education" = 3, "Training" = 4, "Looking after home" = 5, "Sick/disabled" = 6, "Other" = 7)),
         ecoactadu32 = labelled(ecoactadu32, labels = c("Employee" = 1, "Self employed" = 2, "Unpaid/voluntary" = 3, "Unemployed" = 4, "Education" = 5, "Apprenticeship" = 6, "Gov't scheme" = 7, "Sick/disabled" = 8, "Looking after home" = 9, "Other" = 10)))

# Merge all waves
df <- full_join(wave1, wave4, by = "NSID")
df <- full_join(df, wave5, by = "NSID")
df <- full_join(df, wave6, by = "NSID")
df <- full_join(df, wave7, by = "NSID")
df <- full_join(df, wave8, by = "NSID")
df <- full_join(df, wave9, by = "NSID")

# Select variables
selected_df <- df %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(selected_df, "data/output/cleaned_data.csv")