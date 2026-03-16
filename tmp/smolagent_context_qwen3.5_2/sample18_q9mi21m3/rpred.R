library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_items <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_items <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

wave2_sub <- wave2 %>% select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP, W2ghq12scr)
wave4_sub <- wave4 %>% select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP, W4ghq12scr)
wave8_items_sub <- wave8_items %>% select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)
wave8_derived_sub <- wave8_derived %>% select(NSID, W8DGHQSC)
wave9_items_sub <- wave9_items %>% select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)
wave9_derived_sub <- wave9_derived %>% select(NSID, W9DGHQSC)

merged <- wave2_sub %>% full_join(wave4_sub, by = "NSID") %>% full_join(wave8_items_sub, by = "NSID") %>% full_join(wave8_derived_sub, by = "NSID") %>% full_join(wave9_items_sub, by = "NSID") %>% full_join(wave9_derived_sub, by = "NSID")

merged <- merged %>% mutate(ghq15 = case_when(W2ghq12scr %in% c(-96, -99) ~ -3, W2ghq12scr %in% c(-97, -92) ~ -9, is.na(W2ghq12scr) ~ -3, TRUE ~ W2ghq12scr))
merged <- merged %>% mutate(ghq17 = case_when(W4ghq12scr %in% c(-96, -99) ~ -3, W4ghq12scr %in% c(-97, -92) ~ -9, is.na(W4ghq12scr) ~ -3, TRUE ~ W4ghq12scr))
merged <- merged %>% mutate(ghq25 = case_when(is.na(W8DGHQSC) ~ -3, TRUE ~ W8DGHQSC))
merged <- merged %>% mutate(ghq32 = case_when(is.na(W9DGHQSC) ~ -3, TRUE ~ W9DGHQSC))

ghq_items_15 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
ghq_items_17 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
ghq_items_25 <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")
ghq_items_32 <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")

merged <- merged %>% mutate(ghqtl15 = case_when(rowSums(is.na(across(all_of(ghq_items_15)))) == 12 ~ -3, rowSums(across(all_of(ghq_items_15)) < 0, na.rm = TRUE) > 0 ~ -8, TRUE ~ rowSums(across(all_of(ghq_items_15)), na.rm = TRUE)))
merged <- merged %>% mutate(ghqtl17 = case_when(rowSums(is.na(across(all_of(ghq_items_17)))) == 12 ~ -3, rowSums(across(all_of(ghq_items_17)) < 0, na.rm = TRUE) > 0 ~ -8, TRUE ~ rowSums(across(all_of(ghq_items_17)), na.rm = TRUE)))
merged <- merged %>% mutate(ghqtl25 = case_when(rowSums(is.na(across(all_of(ghq_items_25)))) == 12 ~ -3, rowSums(across(all_of(ghq_items_25)) < 0, na.rm = TRUE) > 0 ~ -8, TRUE ~ rowSums(across(all_of(ghq_items_25)), na.rm = TRUE)))
merged <- merged %>% mutate(ghqtl32 = case_when(rowSums(is.na(across(all_of(ghq_items_32)))) == 12 ~ -3, rowSums(across(all_of(ghq_items_32)) < 0, na.rm = TRUE) > 0 ~ -8, TRUE ~ rowSums(across(all_of(ghq_items_32)), na.rm = TRUE)))

missing_labels <- c("-1" = "Item not applicable", "-2" = "Script error/information lost", "-3" = "Not asked at the fieldwork stage/participated/interviewed", "-8" = "Don't know/insufficient information", "-9" = "Refusal")

merged <- merged %>% mutate(ghq15 = as.character(ghq15), ghq17 = as.character(ghq17), ghq25 = as.character(ghq25), ghq32 = as.character(ghq32), ghqtl15 = as.character(ghqtl15), ghqtl17 = as.character(ghqtl17), ghqtl25 = as.character(ghqtl25), ghqtl32 = as.character(ghqtl32))
merged <- merged %>% mutate(ghq15 = as.factor(ghq15), ghq17 = as.factor(ghq17), ghq25 = as.factor(ghq25), ghq32 = as.factor(ghq32), ghqtl15 = as.factor(ghqtl15), ghqtl17 = as.factor(ghqtl17), ghqtl25 = as.factor(ghqtl25), ghqtl32 = as.factor(ghqtl32))

output <- merged %>% select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)
write_csv(output, "data/output/cleaned_data.csv")
cat("Script completed successfully.\n")