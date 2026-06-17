library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define helper functions first
get_qual_cat_8 <- function(data) {
  # W8VCQUC0J = NVQ/SVQ Level 3-5 -> 0
  # W8VCQUC0D = Entry level Wales -> 2
  # W8VCQUC0E = Modern apprenticeship -> 0
  # W8VCQUC0K = HNC/HND -> 0
  # W8VCQUC0B = Key Skills -> 2
  # W8VCQUC0A = Youth training certificate -> 2
  
  data %>%
    mutate(result = case_when(
      !is.na(W8VCQUC0J) & W8VCQUC0J == 1 ~ 0,
      !is.na(W8VCQUC0K) & W8VCQUC0K == 1 ~ 0,
      !is.na(W8VCQUC0E) & W8VCQUC0E == 1 ~ 0,
      !is.na(W8VCQUC0D) & W8VCQUC0D == 1 ~ 2,
      !is.na(W8VCQUC0B) & W8VCQUC0B == 1 ~ 2,
      !is.na(W8VCQUC0A) & W8VCQUC0A == 1 ~ 2,
      TRUE ~ 4
    ))
}

get_qual_cat_9 <- function(data) {
  # W9ACQUC0A = Doctorate -> 0
  # W9ACQUC0B = Masters -> 0
  # W9ACQUC0C = Undergraduate -> 0
  # W9ACQUC0D = Post-graduate Diplomas -> 0
  # W9ACQUC0E = Diplomas in higher ed -> 0
  
  data %>%
    mutate(result = case_when(
      !is.na(W9ACQUC0A) & W9ACQUC0A == 1 ~ 0,
      !is.na(W9ACQUC0B) & W9ACQUC0B == 1 ~ 0,
      !is.na(W9ACQUC0C) & W9ACQUC0C == 1 ~ 0,
      !is.na(W9ACQUC0D) & W9ACQUC0D == 1 ~ 0,
      !is.na(W9ACQUC0E) & W9ACQUC0E == 1 ~ 0,
      TRUE ~ 4
    ))
}

# Load all files from metadata
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_six <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave_eight <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave_nine <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all datasets by NSID
cohort <- full_join(wave_one, wave_four, by = 'NSID')
cohort <- full_join(cohort, wave_six, by = 'NSID')
cohort <- full_join(cohort, wave_seven, by = 'NSID')
cohort <- full_join(cohort, wave_eight, by = 'NSID')
cohort <- full_join(cohort, wave_nine, by = 'NSID')

# Wave 4 (Age 17) - w4saim mapping
cohort <- cohort %>%
  mutate(educaim17 = case_when(
    is.na(w4saim) | w4saim %in% c(-999, -998, -997, -995, -94, -92, -91, -99, -100, -97, -1) ~ -3,
    w4saim == 14 ~ 5,
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11) ~ 1,
    w4saim %in% c(8) ~ 2,
    w4saim %in% c(12, 13) ~ 3,
    TRUE ~ 4
  ))

# Wave 6 (Age 19) - W6Saim mapping
cohort <- cohort %>%
  mutate(educaim19 = case_when(
    is.na(W6Saim) | W6Saim %in% c(-999, -998, -997, -995, -94, -92, -91, -99, -100, -97, -1) ~ -3,
    W6Saim == 16 ~ 5,
    W6Saim %in% c(1, 2, 3, 4) ~ 0,
    W6Saim %in% c(5, 6, 7, 8, 9, 10) ~ 1,
    W6Saim %in% c(11) ~ 2,
    W6Saim %in% c(12, 13) ~ 1,
    W6Saim %in% c(14, 15) ~ 3,
    TRUE ~ 4
  ))

# Wave 7 (Age 20) - W7SAim mapping
cohort <- cohort %>%
  mutate(educaim20 = case_when(
    is.na(W7SAim) | W7SAim %in% c(-999, -998, -997, -995, -94, -92, -91, -99, -100, -97, -1) ~ -3,
    W7SAim == -91 ~ 5,
    W7SAim %in% c(10, 11, 12, 13) ~ 0,
    W7SAim %in% c(1, 2, 3, 6, 7, 8, 9) ~ 1,
    W7SAim %in% c(4, 5) ~ 2,
    W7SAim %in% c(14) ~ 3,
    TRUE ~ 4
  ))

# Wave 8 (Age 25) - W8ACTIVITY05 and qualifications
cohort <- get_qual_cat_8(cohort)
cohort <- cohort %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 %in% c(1, -1) ~ result,
    W8ACTIVITY05 == 0 ~ 4,
    W8ACTIVITY05 == -9 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    TRUE ~ 4
  )) %>%
  mutate(result = NULL)  # Clean up

# Wave 9 (Age 32) - W9ECONACT2 and qualifications
cohort <- get_qual_cat_9(cohort)
cohort <- cohort %>%
  mutate(educaim32 = case_when(
    W9ECONACT2 %in% c(6, 7, 12) ~ result,
    W9ECONACT2 == -1 ~ 4,
    W9ECONACT2 == -3 ~ 4,
    W9ECONACT2 == -9 ~ -9,
    W9ECONACT2 == -8 ~ -8,
    TRUE ~ 4
  )) %>%
  mutate(result = NULL)

# Remove raw source variables, keep only derived variables
cleaned_data <- cohort %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Output to CSV
write_csv(cleaned_data, 'data/output/cleaned_data.csv')
