library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

wave1_clean <- wave1 %>% mutate(W1sexYP = case_when(
  W1sexYP == -92 ~ -9, W1sexYP == -91 ~ -1, W1sexYP == -99 ~ -3,
  W1sexYP == 1 ~ 1, W1sexYP == 2 ~ 2, TRUE ~ NA_real_))

wave2_clean <- wave2 %>% mutate(W2SexYP = case_when(
  W2SexYP == -997 ~ -2, W2SexYP == -998 ~ -2, W2SexYP == -995 ~ -2,
  W2SexYP == -99 ~ -3, W2SexYP == -92 ~ -9, W2SexYP == -91 ~ -1,
  W2SexYP == -1 ~ -7, W2SexYP == 1 ~ 1, W2SexYP == 2 ~ 2, TRUE ~ NA_real_))

wave3_clean <- wave3 %>% mutate(W3sexYP = case_when(
  W3sexYP == -99 ~ -3, W3sexYP == -92 ~ -9, W3sexYP == -91 ~ -1,
  W3sexYP == 1 ~ 1, W3sexYP == 2 ~ 2, TRUE ~ NA_real_))

wave4_clean <- wave4 %>% mutate(W4SexYP = case_when(
  W4SexYP == -99 ~ -3, W4SexYP == -92 ~ -9, W4SexYP == -91 ~ -1,
  W4SexYP == -1 ~ -7, W4SexYP == 1 ~ 1, W4SexYP == 2 ~ 2, TRUE ~ NA_real_))

wave5_clean <- wave5 %>% mutate(W5SexYP = case_when(
  W5SexYP == -1 ~ -7, W5SexYP == 1 ~ 1, W5SexYP == 2 ~ 2, TRUE ~ NA_real_))

wave6_clean <- wave6 %>% mutate(W6Sex = case_when(
  W6Sex == -92 ~ -9, W6Sex == -91 ~ -1, W6Sex == 1 ~ 1,
  W6Sex == 2 ~ 2, TRUE ~ NA_real_))

wave7_clean <- wave7 %>% mutate(W7Sex = case_when(
  W7Sex == -91 ~ -1, W7Sex == 1 ~ 1, W7Sex == 2 ~ 2, TRUE ~ NA_real_))

wave8_clean <- wave8 %>% mutate(W8CMSEX = case_when(
  W8CMSEX == -9 ~ -9, W8CMSEX == -8 ~ -8, W8CMSEX == -1 ~ -1,
  W8CMSEX == 1 ~ 1, W8CMSEX == 2 ~ 2, TRUE ~ NA_real_))

wave9_clean <- wave9 %>% mutate(W9DSEX = case_when(
  W9DSEX == 1 ~ 1, W9DSEX == 2 ~ 2, TRUE ~ NA_real_))

merged_data <- wave1_clean %>%
  full_join(wave2_clean, by = 'NSID') %>%
  full_join(wave3_clean, by = 'NSID') %>%
  full_join(wave4_clean, by = 'NSID') %>%
  full_join(wave5_clean, by = 'NSID') %>%
  full_join(wave6_clean, by = 'NSID') %>%
  full_join(wave7_clean, by = 'NSID') %>%
  full_join(wave8_clean, by = 'NSID') %>%
  full_join(wave9_clean, by = 'NSID')

merged_data <- merged_data %>%
  mutate(
    sex = ifelse(!is.na(W9DSEX) & W9DSEX %in% c(1, 2), W9DSEX, NA_real_),
    sex = ifelse(is.na(sex) & !is.na(W8CMSEX) & W8CMSEX %in% c(1, 2), W8CMSEX, sex),
    sex = ifelse(is.na(sex) & !is.na(W7Sex) & W7Sex %in% c(1, 2), W7Sex, sex),
    sex = ifelse(is.na(sex) & !is.na(W6Sex) & W6Sex %in% c(1, 2), W6Sex, sex),
    sex = ifelse(is.na(sex) & !is.na(W5SexYP) & W5SexYP %in% c(1, 2), W5SexYP, sex),
    sex = ifelse(is.na(sex) & !is.na(W4SexYP) & W4SexYP %in% c(1, 2), W4SexYP, sex),
    sex = ifelse(is.na(sex) & !is.na(W3sexYP) & W3sexYP %in% c(1, 2), W3sexYP, sex),
    sex = ifelse(is.na(sex) & !is.na(W2SexYP) & W2SexYP %in% c(1, 2), W2SexYP, sex),
    sex = ifelse(is.na(sex) & !is.na(W1sexYP) & W1sexYP %in% c(1, 2), W1sexYP, sex),
    sex = ifelse(is.na(sex), -3, sex)
  )

attr(merged_data$sex, 'label') <- 'Cohort member sex'
attr(merged_data$sex, 'labels') <- list('1' = 'Male', '2' = 'Female', '-9' = 'Refused', '-8' = "Don't know", '-7' = 'Prefer not to say', '-3' = 'Not asked', '-1' = 'Item not applicable', '-2' = 'Not applicable')

output_data <- merged_data %>% select(NSID, sex)

write_csv(output_data, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(paste('Rows:', nrow(output_data)))
print(paste('Columns:', ncol(output_data)))