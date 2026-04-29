library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

cleaned_data <- merged_data %>%
  select(NSID, W8DBMI, W9DBMI) %>%
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

cleaned_data <- cleaned_data %>%
  mutate(bmi25 = case_when(
    is.na(bmi25) ~ -3,
    bmi25 < 0 & !bmi25 %in% c(-9, -8, -1) ~ -3,
    TRUE ~ bmi25
  )) %>%
  mutate(bmi32 = case_when(
    is.na(bmi32) ~ -3,
    bmi32 < 0 & !bmi32 %in% c(-9, -8, -1) ~ -3,
    TRUE ~ bmi32
  ))

cleaned_data$bmi25 <- labelled::labelled(cleaned_data$bmi25, labels = c("Refusal" = -9, "Don't know/insufficient information" = -8, "Not applicable" = -1, "Not asked/interviewed" = -3))
cleaned_data$bmi32 <- labelled::labelled(cleaned_data$bmi32, labels = c("Refusal" = -9, "Don't know/insufficient information" = -8, "Not applicable" = -1, "Not asked/interviewed" = -3))

write_csv(cleaned_data, "data/output/cleaned_data.csv")