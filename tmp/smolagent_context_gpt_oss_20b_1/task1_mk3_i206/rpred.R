library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

dir_in <- 'data/input'

w1 <- read_delim(file.path(dir_in,'wave_one_lsype_young_person_2020.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w2 <- read_delim(file.path(dir_in,'wave_two_lsype_young_person_2020.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w3 <- read_delim(file.path(dir_in,'wave_three_lsype_young_person_2020.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w4 <- read_delim(file.path(dir_in,'wave_four_lsype_young_person_2020.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w5 <- read_delim(file.path(dir_in,'wave_five_lsype_young_person_2020.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w6 <- read_delim(file.path(dir_in,'wave_six_lsype_young_person_2020.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w7 <- read_delim(file.path(dir_in,'wave_seven_lsype_young_person_2020.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w8 <- read_delim(file.path(dir_in,'ns8_2015_main_interview.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))
w9 <- read_delim(file.path(dir_in,'ns9_2022_main_interview.tab'), delim='\\t', col_types=cols(.default=col_double(), NSID=col_character()))

merged <- w1 %>%
  full_join(w2, by='NSID') %>%
  full_join(w3, by='NSID') %>%
  full_join(w4, by='NSID') %>%
  full_join(w5, by='NSID') %>%
  full_join(w6, by='NSID') %>%
  full_join(w7, by='NSID') %>%
  full_join(w8, by='NSID') %>%
  full_join(w9, by='NSID')

merged <- merged %>%
  mutate(
    sex_w1 = case_when(
      W1sexYP == 1 ~ 1,
      W1sexYP == 2 ~ 2,
      W1sexYP %in% c(-99,-92,-91) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w2 = case_when(
      W2SexYP == 1 ~ 1,
      W2SexYP == 2 ~ 2,
      W2SexYP %in% c(-998,-997,-995,-99,-92,-91,-1) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w3 = case_when(
      W3sexYP == 1 ~ 1,
      W3sexYP == 2 ~ 2,
      W3sexYP %in% c(-99,-92,-91) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w4 = case_when(
      W4SexYP == 1 ~ 1,
      W4SexYP == 2 ~ 2,
      W4SexYP %in% c(-99,-92,-91,-1) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w5 = case_when(
      W5SexYP == 1 ~ 1,
      W5SexYP == 2 ~ 2,
      W5SexYP == -1 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w6 = case_when(
      W6Sex == 1 ~ 1,
      W6Sex == 2 ~ 2,
      W6Sex %in% c(-92,-91) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w7 = case_when(
      W7Sex == 1 ~ 1,
      W7Sex == 2 ~ 2,
      W7Sex == -91 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w8 = case_when(
      W8CMSEX == 1 ~ 1,
      W8CMSEX == 2 ~ 2,
      W8CMSEX %in% c(-9,-8,-1) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    sex_w9 = case_when(
      W9DSEX == 1 ~ 1,
      W9DSEX == 2 ~ 2,
      TRUE ~ NA_real_
    ),
    sex = coalesce(sex_w9, sex_w1, sex_w2, sex_w3, sex_w4, sex_w5, sex_w6, sex_w7, sex_w8)
  )

merged <- merged %>%
  mutate(sex = if_else(is.na(sex), -3, sex))

final_df <- merged %>% select(NSID, sex)

write_csv(final_df, 'data/output/cleaned_data.csv')