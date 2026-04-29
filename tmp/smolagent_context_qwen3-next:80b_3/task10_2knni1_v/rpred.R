library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

wave4_clean <- wave4 %>%
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
    W4empsYP == 5 ~ 3,
    W4empsYP == 4 ~ 4,
    W4empsYP == 6 ~ 5,
    W4empsYP == 8 ~ 6,
    W4empsYP %in% c(7, 9) ~ 8,
    TRUE ~ NA_real_
  )) %>%
  select(NSID, ecoact17)

wave5_clean <- wave5 %>%
  mutate(W5mainactYP = case_when(
    W5mainactYP == -999 ~ -2,
    W5mainactYP == -94 ~ -8,
    W5mainactYP < 0 ~ -3,
    TRUE ~ W5mainactYP
  )) %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 3 ~ 1,
    W5mainactYP == 7 ~ 2,
    W5mainactYP == 4 ~ 3,
    W5mainactYP %in% c(1, 2, 5, 6) ~ 4,
    W5mainactYP == 8 ~ 5,
    W5mainactYP %in% c(9, 10, 11) ~ 8,
    TRUE ~ NA_real_
  )) %>%
  select(NSID, ecoact18)

wave6_clean <- wave6 %>%
  mutate(W6TCurrentAct = case_when(
    W6TCurrentAct == -91 ~ -1,
    W6TCurrentAct < 0 ~ -3,
    TRUE ~ W6TCurrentAct
  )) %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 3 ~ 1,
    W6TCurrentAct == 8 ~ 2,
    W6TCurrentAct %in% c(1, 2) ~ 3,
    W6TCurrentAct %in% c(4, 5, 10) ~ 4,
    W6TCurrentAct == 7 ~ 5,
    W6TCurrentAct == 11 ~ 7,
    W6TCurrentAct %in% c(6, 9) ~ 8,
    TRUE ~ NA_real_
  )) %>%
  select(NSID, ecoact19)

wave7_clean <- wave7 %>%
  mutate(W7TCurrentAct = case_when(
    W7TCurrentAct == -91 ~ -1,
    W7TCurrentAct < 0 ~ -3,
    TRUE ~ W7TCurrentAct
  )) %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 3 ~ 1,
    W7TCurrentAct == 8 ~ 2,
    W7TCurrentAct %in% c(1, 2) ~ 3,
    W7TCurrentAct %in% c(4, 5, 9, 11) ~ 4,
    W7TCurrentAct == 7 ~ 5,
    W7TCurrentAct == 14 ~ 6,
    W7TCurrentAct == 10 ~ 7,
    W7TCurrentAct %in% c(6, 12, 13, 15) ~ 8,
    TRUE ~ NA_real_
  )) %>%
  select(NSID, ecoact20)

ns8_clean <- ns8 %>%
  mutate(W8DACTIVITYC = case_when(
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    W8DACTIVITYC < 0 ~ -3,
    TRUE ~ W8DACTIVITYC
  )) %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC %in% c(1, 2) ~ 1,
    W8DACTIVITYC == 4 ~ 2,
    W8DACTIVITYC == 5 ~ 3,
    W8DACTIVITYC %in% c(6, 7) ~ 4,
    W8DACTIVITYC == 9 ~ 5,
    W8DACTIVITYC == 8 ~ 6,
    W8DACTIVITYC == 3 ~ 7,
    W8DACTIVITYC == 10 ~ 8,
    TRUE ~ NA_real_
  )) %>%
  mutate(ecoactadu25 = W8DACTIVITYC) %>%
  select(NSID, ecoact25, ecoactadu25)

ns9_clean <- ns9 %>%
  mutate(W9DACTIVITYC = case_when(
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    W9DACTIVITYC < 0 ~ -3,
    TRUE ~ W9DACTIVITYC
  )) %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC %in% c(1, 2) ~ 1,
    W9DACTIVITYC == 4 ~ 2,
    W9DACTIVITYC == 5 ~ 3,
    W9DACTIVITYC %in% c(6, 7) ~ 4,
    W9DACTIVITYC == 9 ~ 5,
    W9DACTIVITYC == 8 ~ 6,
    W9DACTIVITYC == 3 ~ 7,
    W9DACTIVITYC == 10 ~ 8,
    TRUE ~ NA_real_
  )) %>%
  mutate(ecoactadu32 = W9DACTIVITYC) %>%
  select(NSID, ecoact32, ecoactadu32)

all_data <- full_join(wave1, wave4_clean, by = "NSID") %>%
  full_join(wave5_clean, by = "NSID") %>%
  full_join(wave6_clean, by = "NSID") %>%
  full_join(wave7_clean, by = "NSID") %>%
  full_join(ns8_clean, by = "NSID") %>%
  full_join(ns9_clean, by = "NSID")

cleaned_data <- all_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

write_csv(cleaned_data, "data/output/cleaned_data.csv")