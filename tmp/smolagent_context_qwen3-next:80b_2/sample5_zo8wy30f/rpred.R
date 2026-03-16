library(readr)
library(dplyr)
library(haven)
library(purrr)
library(labelled)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

replace_na_with_minus3 <- function(df) {
  df %>% mutate(across(everything(), ~ ifelse(is.na(.), -3, .)))
}

wave1 <- replace_na_with_minus3(wave1)
wave4 <- replace_na_with_minus3(wave4)
wave6 <- replace_na_with_minus3(wave6)
ns8 <- replace_na_with_minus3(ns8)
ns9 <- replace_na_with_minus3(ns9)

wave6 <- wave6 %>%
  mutate(W6MarStatYP = case_when(
    W6MarStatYP == -997 ~ -2,
    W6MarStatYP == -97 | W6MarStatYP == -92 ~ -9,
    W6MarStatYP == -91 ~ -1,
    W6MarStatYP == -1 ~ -8,
    TRUE ~ W6MarStatYP
  ))

wave6 <- wave6 %>% rename(partnr19 = W6MarStatYP)
ns8 <- ns8 %>% rename(partnradu25 = W8DMARSTAT)
ns9 <- ns9 %>% rename(partnradu32 = W9DMARSTAT)

cleaned_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID") %>%
  select(NSID, partnr19, partnradu25, partnradu32)

write_csv(cleaned_data, "data/output/cleaned_data.csv")