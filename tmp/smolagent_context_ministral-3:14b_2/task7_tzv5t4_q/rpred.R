library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load all datasets
wave_one_data <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four_data <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six_data <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven_data <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight_data <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_nine_data <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Recode educaim17 (Wave 4)
wave_four_data <- wave_four_data %>%
  mutate(educaim17 = case_when(
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,
    w4saim %in% c(10, 11) ~ 2,
    w4saim %in% c(12, 13) ~ 3,
    w4saim == 14 ~ 5,
    w4saim == -999 ~ -3,
    w4saim == -99 ~ -9,
    w4saim == -98 ~ -8,
    w4saim == -97 ~ -1,
    w4saim == -96 ~ -2,
    TRUE ~ -3
  ))

# Recode educaim19 (Wave 6)
wave_six_data <- wave_six_data %>%
  mutate(educaim19 = case_when(
    W6Saim %in% c(1, 2, 3, 4) ~ 0,
    W6Saim %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 1,
    W6Saim %in% c(13, 14, 15) ~ 3,
    W6Saim == 16 ~ 5,
    W6Saim == -999 ~ -3,
    W6Saim == -99 ~ -9,
    W6Saim == -98 ~ -8,
    W6Saim == -97 ~ -1,
    W6Saim == -96 ~ -2,
    TRUE ~ -3
  ))

# Recode educaim20 (Wave 7)
wave_seven_data <- wave_seven_data %>%
  mutate(educaim20 = case_when(
    W7SAim == -94 ~ -1,
    W7SAim == -91 ~ 5,
    W7SAim %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 1,
    W7SAim == 2 ~ 2,
    W7SAim %in% c(10, 11, 12, 13) ~ 0,
    W7SAim == 14 ~ 3,
    W7SAim == -999 ~ -3,
    W7SAim == -99 ~ -9,
    W7SAim == -98 ~ -8,
    W7SAim == -97 ~ -1,
    W7SAim == -96 ~ -2,
    TRUE ~ -3
  ))

# Recode educaim25 (Wave 8)
wave_eight_data <- wave_eight_data %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 1 & (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0A == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1) ~ 0,
      W8ACTIVITY05 == 1 & (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1 | W8VCQUC0E == 1) ~ 1,
      W8ACTIVITY05 == 1 ~ 3,
      TRUE ~ 5
    )
  )

# Recode educaim32 (Wave 9)
wave_nine_data <- wave_nine_data %>%
  mutate(
    educaim32 = case_when(
      W9ECONACT2 %in% c(6, 7) & (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0T == 1 | W9VCQUC0U == 1 | W9VCQUC0V == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1 | W9VCQUCAC == 1 | W9VCQUCAD == 1) ~ 0,
      W9ECONACT2 %in% c(6, 7) & (W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9VCQUC0B == 1 | W9VCQUC0C == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1) ~ 1,
      W9ECONACT2 %in% c(6, 7) ~ 3,
      TRUE ~ 5
    )
  )

# Merge datasets by NSID
merged_data <- wave_one_data %>%
  select(NSID) %>%
  full_join(wave_four_data %>% select(NSID, educaim17), by = "NSID") %>%
  full_join(wave_six_data %>% select(NSID, educaim19), by = "NSID") %>%
  full_join(wave_seven_data %>% select(NSID, educaim20), by = "NSID") %>%
  full_join(wave_eight_data %>% select(NSID, educaim25), by = "NSID") %>%
  full_join(wave_nine_data %>% select(NSID, educaim32), by = "NSID")

# Write output to CSV
write_csv(merged_data, "data/output/cleaned_data.csv")