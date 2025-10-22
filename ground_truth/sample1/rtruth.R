# This script is used to run the Next Steps MSEU 
# Load packages
library(haven)  # for reading SPSS/Stata files
library(dplyr)  # for data manipulation
library(purrr)  # for functional programming (map, reduce)
library(here)  # for file paths
library(labelled)  # for handling labelled data
library(readr)  # for reading delimited files

# Set folder path (change as needed)
data_path <- file.path(getwd(),"data", "input")

# Define sweep file names
sweeps <- list(
  S1youngperson = "wave_one_lsype_young_person_2020.tab",
  S2youngperson = "wave_two_lsype_young_person_2020.tab",
  S3youngperson = "wave_three_lsype_young_person_2020.tab",
  S4youngperson = "wave_four_lsype_young_person_2020.tab",
  S5youngperson = "wave_five_lsype_young_person_2020.tab",
  S6youngperson = "wave_six_lsype_young_person_2020.tab",
  S7youngperson = "wave_seven_lsype_young_person_2020.tab",
  S8maininterview = "ns8_2015_main_interview.tab",
  S9maininterview = "ns9_2022_main_interview.tab"
)

#### sex ####
# Load sex variables from relevant sweeps
sex_vars <- list(
  S1 = read_delim(file.path(data_path, sweeps$S1youngperson), , delim = "\t") %>% 
    select(NSID, sex_S1 = W1sexYP),
  S2 = read_delim(file.path(data_path, sweeps$S2youngperson), delim = "\t") %>% 
    select(NSID, sex_S2 = W2SexYP),
  S3 = read_delim(file.path(data_path, sweeps$S3youngperson), delim = "\t") %>% 
    select(NSID, sex_S3 = W3sexYP),
  S4 = read_delim(file.path(data_path, sweeps$S4youngperson), delim = "\t") %>% 
    select(NSID, W4Boost, sex_S4 = W4SexYP),
  S5 = read_delim(file.path(data_path, sweeps$S5youngperson), delim = "\t") %>% 
    select(NSID, sex_S5 = W5SexYP),
  S6 = read_delim(file.path(data_path, sweeps$S6youngperson), delim = "\t") %>% 
    select(NSID, sex_S6 = W6Sex),
  S7 = read_delim(file.path(data_path, sweeps$S7youngperson), delim = "\t") %>% 
    select(NSID, sex_S7 = W7Sex),
  S8 = read_delim(file.path(data_path, sweeps$S8maininterview), delim = "\t") %>% 
    select(NSID, sex_S8 = W8CMSEX),
  S9 = read_delim(file.path(data_path, sweeps$S9maininterview), delim = "\t") %>% 
    select(NSID, sex_S9 = W9DSEX)
)

# Merge all sweeps by NSID
sex_all <- reduce(sex_vars, full_join, by = "NSID")

# Harmonised the missing values for S1-7
# Vector of S1–S7 variable names
sex_vars_s1_s7 <- paste0("sex_S", 1:7)

# Apply custom recode to S1–S7
sex_all <- sex_all %>%
  mutate(across(
    all_of(sex_vars_s1_s7),
    ~ case_when(
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -99 ~ -3,
      TRUE ~ .x
    )
  ))

# Derive harmonised sex 
sex_all <- sex_all %>%
mutate(
  # First pass: positive values only
  sex_final_main = case_when(
    !is.na(sex_S9) & sex_S9 > 0 ~ sex_S9,
    !is.na(sex_S1) & sex_S1 > 0 ~ sex_S1,
    !is.na(sex_S2) & sex_S2 > 0 ~ sex_S2,
    !is.na(sex_S3) & sex_S3 > 0 ~ sex_S3,
    !is.na(sex_S4) & sex_S4 > 0 & W4Boost == 2 ~ sex_S4,  # main
    !is.na(sex_S4) & sex_S4 > 0 & W4Boost == 1 ~ sex_S4,  # boost
    !is.na(sex_S5) & sex_S5 > 0 ~ sex_S5,
    !is.na(sex_S6) & sex_S6 > 0 ~ sex_S6,
    !is.na(sex_S7) & sex_S7 > 0 ~ sex_S7,
    !is.na(sex_S8) & sex_S8 > 0 ~ sex_S8,
    TRUE ~ NA_real_
  ),
  
  # Second pass: fallback to non-positive values (< 0)
  sex_final = case_when(
    !is.na(sex_final_main) ~ sex_final_main,
    !is.na(sex_S1) & sex_S1 < 1 ~ sex_S1,
    !is.na(sex_S2) & sex_S2 < 1 ~ sex_S2,
    !is.na(sex_S3) & sex_S3 < 1 ~ sex_S3,
    !is.na(sex_S4) & sex_S4 < 1 ~ sex_S4,
    !is.na(sex_S5) & sex_S5 < 1 ~ sex_S5,
    !is.na(sex_S6) & sex_S6 < 1 ~ sex_S6,
    !is.na(sex_S7) & sex_S7 < 1 ~ sex_S7,
    !is.na(sex_S8) & sex_S8 < 1 ~ sex_S8,
    TRUE ~ NA_real_
  )
)

sex_all <- sex_all %>%
  mutate(
    sex = case_when(
      sex_final == 1 ~ 0,  # 1 = male → 0
      sex_final == 2 ~ 1,  # 2 = female → 1
      TRUE ~ sex_final      # handle others or missing
    )
  ) %>%
  select(NSID, sex)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(sex_all, output_data_path, row.names = FALSE)