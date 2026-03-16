library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

wave2_clean <- wave2 %>% select(NSID, IMDRSCORE) %>% rename(imd15 = IMDRSCORE)
wave3_clean <- wave3 %>% select(NSID, IMDRSCORE) %>% rename(imd16 = IMDRSCORE)
ns9_clean <- ns9 %>% select(NSID, W9DIMDD) %>% rename(imd32 = W9DIMDD)
wave1_clean <- wave1 %>% select(NSID)
wave4_clean <- wave4 %>% select(NSID)

merged_data <- wave1_clean %>% full_join(wave2_clean, by = "NSID") %>% full_join(wave3_clean, by = "NSID") %>% full_join(wave4_clean, by = "NSID") %>% full_join(ns9_clean, by = "NSID")

merged_data <- merged_data %>% mutate(across(c(imd15, imd16, imd32), ~ ifelse(. == -94, -8, .)))
merged_data <- merged_data %>% mutate(across(c(imd15, imd16, imd32), ~ ifelse(is.na(.), -3, .)))

merged_data <- merged_data %>% mutate(across(c(imd15, imd16, imd32), ~ labelled(. , labels = c("Refusal" = -9, "Don’t know / insufficient information" = -8, "Not asked at the fieldwork stage / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1))))

final_data <- merged_data %>% select(NSID, imd15, imd16, imd32)

write_csv(final_data, "data/output/cleaned_data.csv")