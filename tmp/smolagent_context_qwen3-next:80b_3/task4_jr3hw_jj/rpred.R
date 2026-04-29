library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

wave6 <- wave6 %>%
  rename(sori19 = W6SexualityYP) %>%
  mutate(sori19 = case_when(
    sori19 == -97 ~ -9,
    sori19 == -92 ~ -9,
    sori19 == -91 ~ -1,
    sori19 == -1 ~ -8,
    TRUE ~ sori19
  ))

wave7 <- wave7 %>%
  rename(sori20 = W7SexualityYP) %>%
  mutate(sori20 = case_when(
    sori20 == -100 ~ -9,
    sori20 == -97 ~ -9,
    sori20 == -92 ~ -9,
    sori20 == -91 ~ -1,
    sori20 == -1 ~ -8,
    TRUE ~ sori20
  ))

wave8 <- wave8 %>%
  rename(sori24 = W8SEXUALITY) %>%
  mutate(sori24 = case_when(
    sori24 == -9 ~ -9,
    sori24 == -8 ~ -8,
    sori24 == -1 ~ -1,
    TRUE ~ sori24
  ))

wave9 <- wave9 %>%
  rename(sori32 = W9SORI) %>%
  mutate(sori32 = case_when(
    sori32 == -9 ~ -9,
    sori32 == -8 ~ -8,
    sori32 == -3 ~ -3,
    sori32 == -1 ~ -1,
    sori32 == 5 ~ -7,
    TRUE ~ sori32
  ))

merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

cleaned_data <- merged_data %>% select(NSID, sori19, sori20, sori24, sori32)

write_csv(cleaned_data, "data/output/cleaned_data.csv")