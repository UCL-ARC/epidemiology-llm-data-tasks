library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

input_dir <- "data/input/"

wave1 <- read_delim(paste0(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave2 <- read_delim(paste0(input_dir, "wave_two_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave3 <- read_delim(paste0(input_dir, "wave_three_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave4 <- read_delim(paste0(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave5 <- read_delim(paste0(input_dir, "wave_five_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave6 <- read_delim(paste0(input_dir, "wave_six_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave7 <- read_delim(paste0(input_dir, "wave_seven_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave8 <- read_delim(paste0(input_dir, "ns8_2015_main_interview.tab"), delim = "\t", col_types = cols(.default = col_guess()))
wave9 <- read_delim(paste0(input_dir, "ns9_2022_main_interview.tab"), delim = "\t", col_types = cols(.default = col_guess()))

wave1 <- wave1 %>% mutate(sex14 = case_when(
  `W1sexYP` == 1 ~ 1,
  `W1sexYP` == 2 ~ 2,
  `W1sexYP` == -92 ~ -9,
  `W1sexYP` == -91 ~ -1,
  `W1sexYP` == -99 ~ -3,
  TRUE ~ NA_real_
)) %>% mutate(sex14 = replace_na(sex14, -3))

wave2 <- wave2 %>% mutate(sex15 = case_when(
  `W2SexYP` == 1 ~ 1,
  `W2SexYP` == 2 ~ 2,
  `W2SexYP` == -998 ~ -2,
  `W2SexYP` == -997 ~ -2,
  `W2SexYP` == -995 ~ -2,
  `W2SexYP` == -92 ~ -9,
  `W2SexYP` == -91 ~ -1,
  `W2SexYP` == -1 ~ -8,
  `W2SexYP` == -99 ~ -3,
  TRUE ~ NA_real_
)) %>% mutate(sex15 = replace_na(sex15, -3))

wave3 <- wave3 %>% mutate(sex16 = case_when(
  `W3sexYP` == 1 ~ 1,
  `W3sexYP` == 2 ~ 2,
  `W3sexYP` == -92 ~ -9,
  `W3sexYP` == -91 ~ -1,
  `W3sexYP` == -99 ~ -3,
  TRUE ~ NA_real_
)) %>% mutate(sex16 = replace_na(sex16, -3))

wave4 <- wave4 %>% mutate(sex17 = case_when(
  `W4SexYP` == 1 ~ 1,
  `W4SexYP` == 2 ~ 2,
  `W4SexYP` == -92 ~ -9,
  `W4SexYP` == -91 ~ -1,
  `W4SexYP` == -1 ~ -8,
  `W4SexYP` == -99 ~ -3,
  TRUE ~ NA_real_
)) %>% mutate(sex17 = replace_na(sex17, -3))

wave5 <- wave5 %>% mutate(sex18 = case_when(
  `W5SexYP` == 1 ~ 1,
  `W5SexYP` == 2 ~ 2,
  `W5SexYP` == -1 ~ -8,
  TRUE ~ NA_real_
)) %>% mutate(sex18 = replace_na(sex18, -3))

wave6 <- wave6 %>% mutate(sex19 = case_when(
  `W6Sex` == 1 ~ 1,
  `W6Sex` == 2 ~ 2,
  `W6Sex` == -92 ~ -9,
  `W6Sex` == -91 ~ -1,
  TRUE ~ NA_real_
)) %>% mutate(sex19 = replace_na(sex19, -3))

wave7 <- wave7 %>% mutate(sex20 = case_when(
  `W7Sex` == 1 ~ 1,
  `W7Sex` == 2 ~ 2,
  `W7Sex` == -91 ~ -1,
  TRUE ~ NA_real_
)) %>% mutate(sex20 = replace_na(sex20, -3))

wave8 <- wave8 %>% mutate(sex25 = case_when(
  `W8CMSEX` == 1 ~ 1,
  `W8CMSEX` == 2 ~ 2,
  `W8CMSEX` == -9 ~ -9,
  `W8CMSEX` == -8 ~ -8,
  `W8CMSEX` == -1 ~ -1,
  TRUE ~ NA_real_
)) %>% mutate(sex25 = replace_na(sex25, -3))

wave9 <- wave9 %>% mutate(sex32 = case_when(
  `W9DSEX` == 1 ~ 1,
  `W9DSEX` == 2 ~ 2,
  TRUE ~ NA_real_
)) %>% mutate(sex32 = replace_na(sex32, -3))

all_waves <- reduce(list(wave1, wave2, wave3, wave4, wave5, wave6, wave7, wave8, wave9), full_join, by = "NSID")

all_waves <- all_waves %>% rowwise() %>% mutate(sex = {
  sex_vals <- c(sex32, sex25, sex20, sex19, sex18, sex17, sex16, sex15, sex14)
  sub_idx <- which(sex_vals %in% c(1,2))[1]
  if(!is.na(sub_idx)) {
    sex_vals[sub_idx]
  } else {
    miss_idx <- which(sex_vals %in% c(-9,-8,-7,-3,-2,-1))[1]
    if(!is.na(miss_idx)) sex_vals[miss_idx] else NA_real_
  }
}) %>% ungroup()

final_df <- all_waves %>% select(NSID, sex)

output_dir <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
write_csv(final_df, file.path(output_dir, "cleaned_data.csv"))
cat("Done writing cleaned_data.csv\n")