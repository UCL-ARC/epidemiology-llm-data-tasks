library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

merged <- wave1 %>% full_join(wave2, by = 'NSID') %>% full_join(wave3, by = 'NSID') %>% full_join(wave4, by = 'NSID') %>% full_join(wave5, by = 'NSID') %>% full_join(wave6, by = 'NSID') %>% full_join(wave7, by = 'NSID') %>% full_join(wave8, by = 'NSID') %>% full_join(wave9, by = 'NSID')

merged$W1sexYP <- ifelse(merged$W1sexYP == -99, -3, merged$W1sexYP)
merged$W1sexYP <- ifelse(merged$W1sexYP == -92, -9, merged$W1sexYP)
merged$W1sexYP <- ifelse(merged$W1sexYP == -91, -1, merged$W1sexYP)
merged$W2SexYP <- ifelse(merged$W2SexYP == -998, -2, merged$W2SexYP)
merged$W2SexYP <- ifelse(merged$W2SexYP == -997, -2, merged$W2SexYP)
merged$W2SexYP <- ifelse(merged$W2SexYP == -995, -2, merged$W2SexYP)
merged$W2SexYP <- ifelse(merged$W2SexYP == -99, -3, merged$W2SexYP)
merged$W2SexYP <- ifelse(merged$W2SexYP == -92, -9, merged$W2SexYP)
merged$W2SexYP <- ifelse(merged$W2SexYP == -91, -1, merged$W2SexYP)
merged$W2SexYP <- ifelse(merged$W2SexYP == -1, -8, merged$W2SexYP)
merged$W3sexYP <- ifelse(merged$W3sexYP == -99, -3, merged$W3sexYP)
merged$W3sexYP <- ifelse(merged$W3sexYP == -92, -9, merged$W3sexYP)
merged$W3sexYP <- ifelse(merged$W3sexYP == -91, -1, merged$W3sexYP)
merged$W4SexYP <- ifelse(merged$W4SexYP == -99, -3, merged$W4SexYP)
merged$W4SexYP <- ifelse(merged$W4SexYP == -92, -9, merged$W4SexYP)
merged$W4SexYP <- ifelse(merged$W4SexYP == -91, -1, merged$W4SexYP)
merged$W4SexYP <- ifelse(merged$W4SexYP == -1, -8, merged$W4SexYP)
merged$W5SexYP <- ifelse(merged$W5SexYP == -1, -8, merged$W5SexYP)
merged$W6Sex <- ifelse(merged$W6Sex == -92, -9, merged$W6Sex)
merged$W6Sex <- ifelse(merged$W6Sex == -91, -1, merged$W6Sex)
merged$W7Sex <- ifelse(merged$W7Sex == -91, -1, merged$W7Sex)
merged$W8CMSEX <- ifelse(merged$W8CMSEX == -9, -9, merged$W8CMSEX)
merged$W8CMSEX <- ifelse(merged$W8CMSEX == -8, -8, merged$W8CMSEX)
merged$W8CMSEX <- ifelse(merged$W8CMSEX == -1, -1, merged$W8CMSEX)
merged$W9DSEX <- as.numeric(merged$W9DSEX)

merged <- merged %>% mutate(sex = case_when(!is.na(W9DSEX) & W9DSEX %in% c(1, 2) ~ W9DSEX, !is.na(W8CMSEX) & W8CMSEX %in% c(1, 2) ~ W8CMSEX, !is.na(W7Sex) & W7Sex %in% c(1, 2) ~ W7Sex, !is.na(W6Sex) & W6Sex %in% c(1, 2) ~ W6Sex, !is.na(W5SexYP) & W5SexYP %in% c(1, 2) ~ W5SexYP, !is.na(W4SexYP) & W4SexYP %in% c(1, 2) ~ W4SexYP, !is.na(W3sexYP) & W3sexYP %in% c(1, 2) ~ W3sexYP, !is.na(W2SexYP) & W2SexYP %in% c(1, 2) ~ W2SexYP, !is.na(W1sexYP) & W1sexYP %in% c(1, 2) ~ W1sexYP, TRUE ~ -1))
merged$sex <- ifelse(merged$sex == -1, -3, merged$sex)

lvl <- character(5)
lvl[1] <- 'Not asked at the fieldwork stage / not interviewed'
lvl[2] <- 'Refusal'
lvl[3] <- 'Not applicable'
lvl[4] <- 'Male'
lvl[5] <- 'Female'
merged$sex <- labelled(merged$sex, value_labels = as.list(c(-3 = lvl[1], -9 = lvl[2], -1 = lvl[3], 1 = lvl[4], 2 = lvl[5])))

write_csv(merged, 'data/output/cleaned_data.csv')