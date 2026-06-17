library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave_one_lsype_young_person_2020 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two_lsype_young_person_2020 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave_three_lsype_young_person_2020 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave_four_lsype_young_person_2020 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_five_lsype_young_person_2020 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave_six_lsype_young_person_2020 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven_lsype_young_person_2020 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave_ns8_2015_main_interview <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave_ns9_2022_main_interview <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all datasets using full_join by NSID
merged_data <- wave_one_lsype_young_person_2020 %>%
  full_join(wave_two_lsype_young_person_2020, by = 'NSID') %>%
  full_join(wave_three_lsype_young_person_2020, by = 'NSID') %>%
  full_join(wave_four_lsype_young_person_2020, by = 'NSID') %>%
  full_join(wave_five_lsype_young_person_2020, by = 'NSID') %>%
  full_join(wave_six_lsype_young_person_2020, by = 'NSID') %>%
  full_join(wave_seven_lsype_young_person_2020, by = 'NSID') %>%
  full_join(wave_ns8_2015_main_interview, by = 'NSID') %>%
  full_join(wave_ns9_2022_main_interview, by = 'NSID')

# Convert sex variables to numeric and create indicator for valid values (1 or 2)
merged_data <- merged_data %>%
  mutate(
    W9DSEX_valid = ifelse(W9DSEX %in% c(1, 2), W9DSEX, NA_integer_),
    W8CMSEX_valid = ifelse(W8CMSEX %in% c(1, 2), W8CMSEX, NA_integer_),
    W7Sex_valid = ifelse(W7Sex %in% c(1, 2), W7Sex, NA_integer_),
    W6Sex_valid = ifelse(W6Sex %in% c(1, 2), W6Sex, NA_integer_),
    W5SexYP_valid = ifelse(W5SexYP %in% c(1, 2), W5SexYP, NA_integer_),
    W4SexYP_valid = ifelse(W4SexYP %in% c(1, 2), W4SexYP, NA_integer_),
    W3sexYP_valid = ifelse(W3sexYP %in% c(1, 2), W3sexYP, NA_integer_),
    W2SexYP_valid = ifelse(W2SexYP %in% c(1, 2), W2SexYP, NA_integer_),
    W1sexYP_valid = ifelse(W1sexYP %in% c(1, 2), W1sexYP, NA_integer_)
  )

# Derive sex variable using most recent valid response first
merged_data <- merged_data %>%
  mutate(
    sex = coalesce(W9DSEX_valid, W8CMSEX_valid, W7Sex_valid, W6Sex_valid, W5SexYP_valid, W4SexYP_valid, W3sexYP_valid, W2SexYP_valid, W1sexYP_valid)
  )

# Write output
write_csv(merged_data %>% select(NSID, sex), 'data/output/cleaned_data.csv')