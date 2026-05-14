library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t") %>% select(NSID)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>% select(NSID)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t") %>% 
  mutate(W6MarStatYP = case_when(
    W6MarStatYP == -997 ~ -2,
    W6MarStatYP == -97 ~ -9,
    W6MarStatYP == -92 ~ -9,
    W6MarStatYP == -91 ~ -1,
    W6MarStatYP == -1 ~ -8,
    TRUE ~ W6MarStatYP
  )) %>% select(NSID, partnr19 = W6MarStatYP)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t") %>% 
  mutate(W8DMARSTAT = case_when(
    W8DMARSTAT == -9 ~ -9,
    W8DMARSTAT == -8 ~ -8,
    W8DMARSTAT == -1 ~ -1,
    TRUE ~ W8DMARSTAT
  )) %>% 
  mutate(partnradu25 = W8DMARSTAT) %>% 
  mutate(partnr25 = case_when(
    partnradu25 == 1 ~ 1,
    partnradu25 == 2 ~ 2,
    partnradu25 == 3 ~ 3,
    partnradu25 == 4 ~ 4,
    partnradu25 == 5 ~ 5,
    partnradu25 == 6 ~ 2,
    partnradu25 == 7 ~ 3,
    partnradu25 == 8 ~ 4,
    partnradu25 == 9 ~ 5,
    TRUE ~ partnradu25
  )) %>% select(NSID, partnr25, partnradu25)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t") %>% 
  mutate(W9DMARSTAT = case_when(
    W9DMARSTAT == -9 ~ -9,
    W9DMARSTAT == -8 ~ -8,
    TRUE ~ W9DMARSTAT
  )) %>% 
  mutate(partnradu32 = W9DMARSTAT) %>% 
  mutate(partnr32 = case_when(
    partnradu32 == 1 ~ 1,
    partnradu32 == 2 ~ 2,
    partnradu32 == 3 ~ 4,
    partnradu32 == 4 ~ 3,
    partnradu32 == 5 ~ 5,
    partnradu32 == 6 ~ 2,
    partnradu32 == 7 ~ 4,
    partnradu32 == 8 ~ 5,
    TRUE ~ partnradu32
  )) %>% select(NSID, partnr32, partnradu32)

cleaned_data <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

write_csv(cleaned_data, "data/output/cleaned_data.csv")