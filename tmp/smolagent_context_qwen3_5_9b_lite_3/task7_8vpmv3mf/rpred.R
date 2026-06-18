library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Define common labels
labels <- list(
  `1` = 'NVQ 1/Foundation',
  `2` = 'GCSE/A-Level',
  `3` = 'NVQ 2-4/Intermediate',
  `4` = 'Higher Education',
  `5` = 'Degree Level',
  `6` = 'Not studying/Other',
  `-3` = 'Not asked at fieldwork stage',
  `-1` = 'Item not applicable'
)

# Wave 4 (age 17) - w4saim to educaim17
wave4_clean <- wave4 %>%
  mutate(
    educaim17 = case_when(
      w4saim == 1 ~ 1,
      w4saim == 2 ~ 3,
      w4saim == 3 ~ 3,
      w4saim == 4 ~ 3,
      w4saim == 5 ~ 1,
      w4saim == 6 ~ 1,
      w4saim == 7 ~ 2,
      w4saim == 8 ~ 2,
      w4saim == 9 ~ 1,
      w4saim == 10 ~ 1,
      w4saim == 11 ~ 2,
      w4saim == 12 ~ 6,
      w4saim == 13 ~ 6,
      w4saim == 14 ~ 6,
      w4saim %in% c(-999, -998, -997, -995, -94, -99, -92, -91, -100, -97, -9, -8, -3, -2, -1) ~ -3,
      TRUE ~ NA_real_
    )
  )

attr(wave4_clean$educaim17, 'label') <- 'Educational aims at age 17'
attr(wave4_clean$educaim17, 'labels') <- labels

# Wave 6 (age 19) - W6Saim to educaim19
wave6_clean <- wave6 %>%
  mutate(
    educaim19 = case_when(
      W6Saim == 1 ~ 3,
      W6Saim == 2 ~ 4,
      W6Saim == 3 ~ 4,
      W6Saim == 4 ~ 4,
      W6Saim == 5 ~ 3,
      W6Saim == 6 ~ 3,
      W6Saim == 7 ~ 3,
      W6Saim == 8 ~ 3,
      W6Saim == 9 ~ 2,
      W6Saim == 10 ~ 2,
      W6Saim == 11 ~ 2,
      W6Saim == 12 ~ 2,
      W6Saim == 13 ~ 2,
      W6Saim == 14 ~ 6,
      W6Saim >= 15 & W6Saim <= 16 ~ 6,
      W6Saim %in% c(-999, -998, -997, -995, -94, -99, -92, -91, -100, -97, -9, -8, -3, -2, -1) ~ -3,
      TRUE ~ NA_real_
    )
  )

attr(wave6_clean$educaim19, 'label') <- 'Educational aims at age 19'
attr(wave6_clean$educaim19, 'labels') <- labels

# Wave 7 (age 20) - W7SAim to educaim20
wave7_clean <- wave7 %>%
  mutate(
    educaim20 = case_when(
      W7SAim == 1 ~ 1,
      W7SAim == 2 ~ 2,
      W7SAim == 3 ~ 1,
      W7SAim == 4 ~ 2,
      W7SAim == 5 ~ 2,
      W7SAim == 6 ~ 3,
      W7SAim == 7 ~ 3,
      W7SAim == 8 ~ 3,
      W7SAim == 9 ~ 3,
      W7SAim == 10 ~ 4,
      W7SAim == 11 ~ 4,
      W7SAim == 12 ~ 4,
      W7SAim == 13 ~ 5,
      W7SAim == 14 ~ 6,
      W7SAim == -91 ~ 6,
      W7SAim %in% c(-999, -998, -997, -995, -94, -99, -92, -100, -97, -9, -8, -3, -2, -1) ~ -3,
      TRUE ~ NA_real_
    )
  )

attr(wave7_clean$educaim20, 'label') <- 'Educational aims at age 20'
attr(wave7_clean$educaim20, 'labels') <- labels

# Wave 8 (age 25) - Derive from qualification variables
wave8_clean <- wave8 %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 1 ~ 6,
      W8ACQUC0A == 1 ~ 4,
      W8ACQUC0B == 1 ~ 4,
      W8ACQUC0C == 1 ~ 4,
      W8ACQUC0D == 1 ~ 4,
      W8ACQUC0E == 1 ~ 4,
      W8ACQUC0F == 1 ~ 2,
      W8ACQUC0G == 1 ~ 2,
      W8ACQUC0H == 1 ~ 2,
      W8ACQUC0I == 1 ~ 2,
      W8ACQUC0J == 1 ~ 2,
      W8ACQUC0K == 1 ~ 2,
      W8ACQUC0L == 1 ~ 2,
      W8ACQUC0M == 1 ~ 2,
      W8ACQUC0N == 1 ~ 2,
      W8VCQUC0J == 1 ~ 3,
      W8VCQUC0K == 1 ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(educaim25 = ifelse(educaim25 == 2, 3, educaim25))

attr(wave8_clean$educaim25, 'label') <- 'Educational aims at age 25'
attr(wave8_clean$educaim25, 'labels') <- labels

# Wave 9 (age 32) - Derive from current study variables
wave9_clean <- wave9 %>%
  mutate(
    educaim32 = case_when(
      W9ACQUC0A == 1 ~ 4,
      W9ACQUC0B == 1 ~ 4,
      W9ACQUC0D == 1 ~ 4,
      W9ACQUC0E == 1 ~ 4,
      W9VCQUC0C == 1 ~ 3,
      W9VCQUC0D == 1 ~ 3,
      W9VCQUC0E == 1 ~ 3,
      W9VCQUC0F == 1 ~ 3,
      W9VCQUC0I == 1 ~ 3,
      W9VCQUC0O == 1 ~ 3,
      W9VCQUC0R == 1 ~ 3,
      W9ACQUC0G == 1 ~ 2,
      W9ACQUC0H == 1 ~ 2,
      W9ACQUC0I == 1 ~ 2,
      W9VCQUC0W == 1 ~ 3,
      W9VCQUC0X == 1 ~ 3,
      W9VCQUC0Y == 1 ~ 3,
      W9VCQUC0Z == 1 ~ 3,
      W9VCQUCAA == 1 ~ 3,
      W9VCQUCAB == 1 ~ 3,
      W9VCQUCAC == 1 ~ 3,
      W9VCQUCAD == 1 ~ 3,
      W9VCQUCAE == 1 ~ 2,
      W9VCQUCAF == 1 ~ 3,
      TRUE ~ NA_real_
    )
  )

attr(wave9_clean$educaim32, 'label') <- 'Educational aims at age 32'
attr(wave9_clean$educaim32, 'labels') <- labels

# Combine all waves
full_data <- full_join(wave1, wave4_clean, by = 'NSID') %>%
  full_join(wave6_clean, by = 'NSID') %>%
  full_join(wave7_clean, by = 'NSID') %>%
  full_join(wave8_clean, by = 'NSID') %>%
  full_join(wave9_clean, by = 'NSID')

# Select final variables
output_vars <- c('NSID', 'educaim17', 'educaim19', 'educaim20', 'educaim25', 'educaim32')
final_data <- full_data %>% select(all_of(output_vars))

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')
