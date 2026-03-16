library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave1 <- wave1 %>% mutate(eth14 = case_when(
  W1ethnic2YP == -999 ~ -3,
  W1ethnic2YP == -94 ~ -8,
  W1ethnic2YP == -92 ~ -9,
  W1ethnic2YP == -91 ~ -1,
  W1ethnic2YP == -1 ~ -8,
  TRUE ~ W1ethnic2YP
)) %>% mutate(eth14 = ifelse(eth14 < 0, NA, eth14)) %>% select(NSID, eth14)
wave2 <- wave2 %>% mutate(eth15 = case_when(
  W2ethnicYP == -998 ~ -3,
  W2ethnicYP == -997 ~ -3,
  W2ethnicYP == -995 ~ -3,
  W2ethnicYP == -99 ~ -3,
  W2ethnicYP == -92 ~ -9,
  W2ethnicYP == -91 ~ -1,
  W2ethnicYP == -1 ~ -8,
  TRUE ~ W2ethnicYP
)) %>% mutate(eth15 = ifelse(eth15 < 0, NA, eth15)) %>% select(NSID, eth15)
wave4 <- wave4 %>% mutate(eth17 = case_when(
  w4ethnic2YP == -94 ~ -8,
  w4ethnic2YP == -1 ~ -8,
  TRUE ~ w4ethnic2YP
)) %>% mutate(eth17 = ifelse(eth17 < 0, NA, eth17)) %>% select(NSID, eth17)
wave8 <- wave8 %>% mutate(eth25 = case_when(
  W8DETHN15 == -9 ~ -9,
  W8DETHN15 == -8 ~ -8,
  W8DETHN15 == -1 ~ -1,
  TRUE ~ W8DETHN15
)) %>% mutate(eth25 = ifelse(eth25 < 0, NA, eth25)) %>% select(NSID, eth25)
wave9 <- wave9 %>% mutate(eth32 = case_when(
  W9DETHN15 == -8 ~ -8,
  TRUE ~ W9DETHN15
)) %>% mutate(eth32 = ifelse(eth32 < 0, NA, eth32)) %>% select(NSID, eth32)
merged <- full_join(wave1, wave2, by = "NSID") %>% full_join(wave4, by = "NSID") %>% full_join(wave8, by = "NSID") %>% full_join(wave9, by = "NSID")
cleaned_data <- merged %>% mutate(eth = coalesce(eth14, eth15, eth17, eth25, eth32)) %>% select(NSID, eth)
write_csv(cleaned_data, "data/output/cleaned_data.csv")