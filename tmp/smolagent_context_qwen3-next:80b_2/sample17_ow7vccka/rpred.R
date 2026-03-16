library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_one <- wave_one %>% select(NSID)
wave_two <- wave_two %>% select(NSID, IMDRSCORE) %>% rename(imd15 = IMDRSCORE)
wave_three <- wave_three %>% select(NSID, IMDRSCORE) %>% rename(imd16 = IMDRSCORE)
wave_four <- wave_four %>% select(NSID)
ns9 <- ns9 %>% select(NSID, W9DIMDD) %>% rename(imd32 = W9DIMDD)
merged <- full_join(wave_one, wave_two, by = "NSID") %>%
full_join(wave_three, by = "NSID") %>%
full_join(wave_four, by = "NSID") %>%
full_join(ns9, by = "NSID")
merged$imd15[is.na(merged$imd15)] <- -3
merged$imd15[merged$imd15 == -94] <- -8
merged$imd16[is.na(merged$imd16)] <- -3
merged$imd16[merged$imd16 == -94] <- -8
merged$imd32[is.na(merged$imd32)] <- -3
merged$imd15 <- labelled(merged$imd15, labels = c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked at the fieldwork stage / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1))
merged$imd16 <- labelled(merged$imd16, labels = c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked at the fieldwork stage / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1))
merged$imd32 <- labelled(merged$imd32, labels = c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked at the fieldwork stage / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1))
cleaned_data <- merged %>% select(NSID, imd15, imd16, imd32)
write_csv(cleaned_data, "data/output/cleaned_data.csv")