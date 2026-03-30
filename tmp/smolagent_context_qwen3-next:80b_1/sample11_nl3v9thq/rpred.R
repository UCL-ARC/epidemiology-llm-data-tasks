library(dplyr)
library(readr)
library(purrr)
library(labelled)

wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

wave1 <- wave1 %>% rename(ecoactmum14 = W1empsmum, ecoactdad14 = W1empsdad)
wave2 <- wave2 %>% rename(ecoactmum15 = W2empsmum, ecoactdad15 = W2empsdad)
wave3 <- wave3 %>% rename(ecoactmum16 = W3empsmum, ecoactdad16 = W3empsdad)
wave4 <- wave4 %>% rename(ecoactmum17 = w4empsmum, ecoactdad17 = w4empsdad)

wave1 <- wave1 %>% mutate(ecoactmum14 = case_when(ecoactmum14 == -999 ~ -2, ecoactmum14 == -99 ~ -3, ecoactmum14 == -98 ~ -1, ecoactmum14 == -94 ~ -8, TRUE ~ ecoactmum14)) %>% mutate(ecoactdad14 = case_when(ecoactdad14 == -999 ~ -2, ecoactdad14 == -99 ~ -3, ecoactdad14 == -98 ~ -1, ecoactdad14 == -94 ~ -8, TRUE ~ ecoactdad14))
wave2 <- wave2 %>% mutate(ecoactmum15 = case_when(ecoactmum15 == -999 ~ -2, ecoactmum15 == -99 ~ -3, ecoactmum15 == -98 ~ -1, ecoactmum15 == -94 ~ -8, TRUE ~ ecoactmum15)) %>% mutate(ecoactdad15 = case_when(ecoactdad15 == -999 ~ -2, ecoactdad15 == -99 ~ -3, ecoactdad15 == -98 ~ -1, ecoactdad15 == -94 ~ -8, TRUE ~ ecoactdad15))
wave3 <- wave3 %>% mutate(ecoactmum16 = case_when(ecoactmum16 == -999 ~ -2, ecoactmum16 == -99 ~ -3, ecoactmum16 == -98 ~ -1, ecoactmum16 == -94 ~ -8, TRUE ~ ecoactmum16)) %>% mutate(ecoactdad16 = case_when(ecoactdad16 == -999 ~ -2, ecoactdad16 == -99 ~ -3, ecoactdad16 == -98 ~ -1, ecoactdad16 == -94 ~ -8, TRUE ~ ecoactdad16))
wave4 <- wave4 %>% mutate(ecoactmum17 = case_when(ecoactmum17 == -999 ~ -2, ecoactmum17 == -99 ~ -3, ecoactmum17 == -98 ~ -1, ecoactmum17 == -94 ~ -8, TRUE ~ ecoactmum17)) %>% mutate(ecoactdad17 = case_when(ecoactdad17 == -999 ~ -2, ecoactdad17 == -996 ~ -1, ecoactdad17 == -99 ~ -3, ecoactdad17 == -98 ~ -1, ecoactdad17 == -94 ~ -8, ecoactdad17 == -92 ~ -9, TRUE ~ ecoactdad17))

merged_data <- wave1 %>% full_join(wave2, by = "NSID") %>% full_join(wave3, by = "NSID") %>% full_join(wave4, by = "NSID")
cleaned_data <- merged_data %>% select(NSID, starts_with("ecoact"))
if (!dir.exists("data/output")) { dir.create("data/output", recursive = TRUE) }
write_csv(cleaned_data, "data/output/cleaned_data.csv")