library(haven)
library(dplyr)
library(readr)

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave1_selected <- wave1 %>% select(NSID)

wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t') %>%
  rename(regub15 = urbind, regov15 = gor) %>%
  mutate(regub15 = case_when(
    regub15 == -94 ~ -8,
    regub15 < 0 ~ -3,
    TRUE ~ regub15
  )) %>%
  mutate(regov15 = case_when(
    regov15 == -94 ~ -8,
    regov15 < 0 ~ -3,
    TRUE ~ regov15
  ))
wave2_selected <- wave2 %>% select(NSID, regub15, regov15)

wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t') %>%
  rename(regub16 = urbind, regov16 = gor) %>%
  mutate(regub16 = case_when(
    regub16 == -94 ~ -8,
    regub16 < 0 ~ -3,
    TRUE ~ regub16
  )) %>%
  mutate(regov16 = case_when(
    regov16 == -94 ~ -8,
    regov16 < 0 ~ -3,
    TRUE ~ regov16
  ))
wave3_selected <- wave3 %>% select(NSID, regub16, regov16)

ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t') %>%
  rename(regov23 = W8DGOR) %>%
  mutate(regov23 = case_when(
    regov23 == -9 ~ -9,
    regov23 == -8 ~ -8,
    regov23 == -1 ~ -1,
    regov23 < 0 ~ -3,
    TRUE ~ regov23
  ))
ns8_selected <- ns8 %>% select(NSID, regov23)

ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t') %>%
  rename(regov32 = W9DRGN) %>%
  mutate(regov32 = case_when(
    regov32 == -9 ~ -9,
    regov32 == -8 ~ -8,
    regov32 == -1 ~ -1,
    regov32 < 0 ~ -3,
    TRUE ~ regov32
  ))
ns9_derived_selected <- ns9_derived %>% select(NSID, regov32)

ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t') %>%
  rename(nation32 = W9NATIONRES) %>%
  mutate(nation32 = case_when(
    nation32 == -9 ~ -9,
    nation32 == -8 ~ -8,
    nation32 == -3 ~ -3,
    nation32 == -1 ~ -1,
    nation32 < 0 ~ -3,
    TRUE ~ nation32
  ))
ns9_main_selected <- ns9_main %>% select(NSID, nation32)

cleaned_data <- wave1_selected %>%
  full_join(wave2_selected, by = 'NSID') %>%
  full_join(wave3_selected, by = 'NSID') %>%
  full_join(ns8_selected, by = 'NSID') %>%
  full_join(ns9_derived_selected, by = 'NSID') %>%
  full_join(ns9_main_selected, by = 'NSID')

write_csv(cleaned_data, 'data/output/cleaned_data.csv')