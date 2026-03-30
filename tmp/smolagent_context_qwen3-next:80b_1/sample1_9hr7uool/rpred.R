library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Process wave1 sex variable
wave1 <- wave1 %>%
  mutate(W1sexYP = case_when(
    W1sexYP == -99 ~ -3,
    W1sexYP == -92 ~ -9,
    W1sexYP == -91 ~ -1,
    TRUE ~ W1sexYP
  )) %>%
  rename(sex14 = W1sexYP)

# Process wave2 sex variable
wave2 <- wave2 %>%
  mutate(W2SexYP = case_when(
    W2SexYP == -998 ~ -3,
    W2SexYP == -997 ~ -3,
    W2SexYP == -995 ~ -3,
    W2SexYP == -99 ~ -3,
    W2SexYP == -92 ~ -9,
    W2SexYP == -91 ~ -1,
    W2SexYP == -1 ~ -8,
    TRUE ~ W2SexYP
  )) %>%
  rename(sex15 = W2SexYP)

# Process wave3 sex variable
wave3 <- wave3 %>%
  mutate(W3sexYP = case_when(
    W3sexYP == -99 ~ -3,
    W3sexYP == -92 ~ -9,
    W3sexYP == -91 ~ -1,
    TRUE ~ W3sexYP
  )) %>%
  rename(sex16 = W3sexYP)

# Process wave4 sex variable
wave4 <- wave4 %>%
  mutate(W4SexYP = case_when(
    W4SexYP == -99 ~ -3,
    W4SexYP == -92 ~ -9,
    W4SexYP == -91 ~ -1,
    W4SexYP == -1 ~ -8,
    TRUE ~ W4SexYP
  )) %>%
  rename(sex17 = W4SexYP)

# Process wave5 sex variable
wave5 <- wave5 %>%
  mutate(W5SexYP = case_when(
    W5SexYP == -1 ~ -8,
    TRUE ~ W5SexYP
  )) %>%
  rename(sex18 = W5SexYP)

# Process wave6 sex variable
wave6 <- wave6 %>%
  mutate(W6Sex = case_when(
    W6Sex == -92 ~ -9,
    W6Sex == -91 ~ -1,
    TRUE ~ W6Sex
  )) %>%
  rename(sex19 = W6Sex)

# Process wave7 sex variable
wave7 <- wave7 %>%
  mutate(W7Sex = case_when(
    W7Sex == -91 ~ -1,
    TRUE ~ W7Sex
  )) %>%
  rename(sex20 = W7Sex)

# Process ns8 sex variable
ns8 <- ns8 %>%
  mutate(W8CMSEX = case_when(
    W8CMSEX == -9 ~ -9,
    W8CMSEX == -8 ~ -8,
    W8CMSEX == -1 ~ -1,
    TRUE ~ W8CMSEX
  )) %>%
  rename(sex21 = W8CMSEX)

# Process ns9 sex variable
ns9 <- ns9 %>%
  rename(sex32 = W9DSEX)

# Merge all files
data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Create consolidated sex variable
data <- data %>%
  mutate(
    sex32 = ifelse(sex32 %in% c(1, 2), sex32, NA),
    sex21 = ifelse(sex21 %in% c(1, 2), sex21, NA),
    sex20 = ifelse(sex20 %in% c(1, 2), sex20, NA),
    sex19 = ifelse(sex19 %in% c(1, 2), sex19, NA),
    sex18 = ifelse(sex18 %in% c(1, 2), sex18, NA),
    sex17 = ifelse(sex17 %in% c(1, 2), sex17, NA),
    sex16 = ifelse(sex16 %in% c(1, 2), sex16, NA),
    sex15 = ifelse(sex15 %in% c(1, 2), sex15, NA),
    sex14 = ifelse(sex14 %in% c(1, 2), sex14, NA)
  ) %>%
  mutate(sex = coalesce(sex32, sex21, sex20, sex19, sex18, sex17, sex16, sex15, sex14)) %>%
  mutate(sex = ifelse(is.na(sex), -3, sex)) %>%
  select(NSID, sex)

# Write output
write_csv(data, "data/output/cleaned_data.csv")