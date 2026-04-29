library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
merged_data <- wave1 %>% full_join(wave4, by = "NSID") %>% full_join(ns8, by = "NSID") %>% full_join(ns9, by = "NSID")
cleaned_data <- merged_data %>% select(NSID, W8DBMI, W9DBMI) %>% rename(bmi25 = W8DBMI, bmi32 = W9DBMI)
cleaned_data$bmi25[is.na(cleaned_data$bmi25)] <- -3
cleaned_data$bmi25[cleaned_data$bmi25 < 0 & !cleaned_data$bmi25 %in% c(-9, -8, -1, -2, -3)] <- -3
cleaned_data$bmi32[is.na(cleaned_data$bmi32)] <- -3
cleaned_data$bmi32[cleaned_data$bmi32 < 0 & !cleaned_data$bmi32 %in% c(-9, -8, -1, -2, -3)] <- -3
cleaned_data$bmi25 <- labelled::labelled(cleaned_data$bmi25, labels = c(Refusal = -9, "Don't know" = -8, "Not applicable" = -1, "Script error" = -2, "Not asked" = -3))
cleaned_data$bmi32 <- labelled::labelled(cleaned_data$bmi32, labels = c(Refusal = -9, "Don't know" = -8, "Not applicable" = -1, "Script error" = -2, "Not asked" = -3))
write_csv(cleaned_data, "data/output/cleaned_data.csv")