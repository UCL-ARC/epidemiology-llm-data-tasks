library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

files <- c("wave_one_lsype_young_person_2020.tab", "wave_two_lsype_young_person_2020.tab", "wave_three_lsype_young_person_2020.tab", "wave_four_lsype_young_person_2020.tab", "wave_five_lsype_young_person_2020.tab", "wave_six_lsype_young_person_2020.tab", "wave_seven_lsype_young_person_2020.tab", "ns8_2015_main_interview.tab", "ns9_2022_main_interview.tab")

data_list <- map(files, ~ read_delim(paste0("data/input/", .x), delim = "\t"))

data_w1 <- data_list[[1]]
data_w1 <- data_w1 %>% mutate(W1sexYP = case_when(
  W1sexYP == -99.0 ~ -3,
  W1sexYP == -92.0 ~ -9,
  W1sexYP == -91.0 ~ -1,
  TRUE ~ W1sexYP
))
data_list[[1]] <- data_w1

data_w2 <- data_list[[2]]
data_w2 <- data_w2 %>% mutate(W2SexYP = case_when(
  W2SexYP == -998.0 ~ -8,
  W2SexYP == -997.0 ~ -8,
  W2SexYP == -995.0 ~ -8,
  W2SexYP == -99.0 ~ -3,
  W2SexYP == -92.0 ~ -9,
  W2SexYP == -91.0 ~ -1,
  W2SexYP == -1.0 ~ -8,
  TRUE ~ W2SexYP
))
data_list[[2]] <- data_w2

data_w3 <- data_list[[3]]
data_w3 <- data_w3 %>% mutate(W3sexYP = case_when(
  W3sexYP == -99.0 ~ -3,
  W3sexYP == -92.0 ~ -9,
  W3sexYP == -91.0 ~ -1,
  TRUE ~ W3sexYP
))
data_list[[3]] <- data_w3

data_w4 <- data_list[[4]]
data_w4 <- data_w4 %>% mutate(W4SexYP = case_when(
  W4SexYP == -99.0 ~ -3,
  W4SexYP == -92.0 ~ -9,
  W4SexYP == -91.0 ~ -1,
  W4SexYP == -1.0 ~ -8,
  TRUE ~ W4SexYP
))
data_list[[4]] <- data_w4

data_w5 <- data_list[[5]]
data_w5 <- data_w5 %>% mutate(W5SexYP = case_when(
  W5SexYP == -1.0 ~ -8,
  TRUE ~ W5SexYP
))
data_list[[5]] <- data_w5

data_w6 <- data_list[[6]]
data_w6 <- data_w6 %>% mutate(W6Sex = case_when(
  W6Sex == -92.0 ~ -9,
  W6Sex == -91.0 ~ -1,
  TRUE ~ W6Sex
))
data_list[[6]] <- data_w6

data_w7 <- data_list[[7]]
data_w7 <- data_w7 %>% mutate(W7Sex = case_when(
  W7Sex == -91.0 ~ -1,
  TRUE ~ W7Sex
))
data_list[[7]] <- data_w7

data_ns8 <- data_list[[8]]
data_ns8 <- data_ns8 %>% mutate(W8CMSEX = case_when(
  W8CMSEX == -9.0 ~ -9,
  W8CMSEX == -8.0 ~ -8,
  W8CMSEX == -1.0 ~ -1,
  TRUE ~ W8CMSEX
))
data_list[[8]] <- data_ns8

data_ns9 <- data_list[[9]]
data_ns9 <- data_ns9 %>% mutate(W9DSEX = case_when(
  W9DSEX %in% c(1, 2) ~ W9DSEX,
  TRUE ~ -3
))
data_list[[9]] <- data_ns9

merged_data <- reduce(data_list, full_join, by = "NSID")

sex_columns <- c("W9DSEX", "W8CMSEX", "W7Sex", "W6Sex", "W5SexYP", "W4SexYP", "W3sexYP", "W2SexYP", "W1sexYP")

for (col in sex_columns) {
  merged_data <- merged_data %>% mutate(!!sym(col) := ifelse(!!sym(col) %in% c(-3, -9, -8, -1), NA, !!sym(col)))
}

merged_data <- merged_data %>% mutate(sex = coalesce(!!!syms(sex_columns)))
merged_data <- merged_data %>% mutate(sex = ifelse(is.na(sex), -3, sex))

cleaned_data <- merged_data %>% select(NSID, sex)

write_csv(cleaned_data, "data/output/cleaned_data.csv")