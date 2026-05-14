library(dplyr)
library(readr)
library(purrr)
library(haven)

# The previous error indicates a many-to-many join overflow.
# This usually happens if NSID is not unique in the files or is being read incorrectly (e.g., as NA).
# We should ensure NSID is treated as a character and duplicates are removed before joining.

load_data <- function(f) {
  # Read with NSID as character to avoid precision issues or NA errors
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'))
}

S1 <- load_data('wave_one_lsype_young_person_2020.tab') %>% 
  select(NSID) %>% 
  distinct(NSID, .keep_all = TRUE)

S4 <- load_data('wave_four_lsype_young_person_2020.tab') %>% 
  select(NSID, w4saim) %>% 
  distinct(NSID, .keep_all = TRUE)

S6 <- load_data('wave_six_lsype_young_person_2020.tab') %>% 
  select(NSID, W6Saim) %>% 
  distinct(NSID, .keep_all = TRUE)

S7 <- load_data('wave_seven_lsype_young_person_2020.tab') %>% 
  select(NSID, W7SAim) %>% 
  distinct(NSID, .keep_all = TRUE)

S8 <- load_data('ns8_2015_main_interview.tab') %>%
  select(NSID, W8ACTIVITY05, starts_with('W8ACQUC'), starts_with('W8VCQUC')) %>%
  distinct(NSID, .keep_all = TRUE)

S9 <- load_data('ns9_2022_main_interview.tab') %>%
  select(NSID, W9ECONACT2, starts_with('W9ACQUC'), starts_with('W9VCQUC')) %>%
  distinct(NSID, .keep_all = TRUE)

# Merge with distinct IDs
cohort <- S1 %>%
  full_join(S4, by = 'NSID') %>%
  full_join(S6, by = 'NSID') %>%
  full_join(S7, by = 'NSID') %>%
  full_join(S8, by = 'NSID') %>%
  full_join(S9, by = 'NSID')

# Recoding Logic

# Age 17 (w4saim)
cohort <- cohort %>%
  mutate(educaim17 = case_when(
    w4saim %in% 1:11 ~ 1,
    w4saim %in% c(12, 13) ~ 3,
    w4saim == 14 ~ 5,
    w4saim >= -1 & w4saim <= -999 ~ -3,
    TRUE ~ -3
  ))

# Age 19 (W6Saim)
cohort <- cohort %>%
  mutate(educaim19 = case_when(
    W6Saim %in% 1:4 ~ 0,
    W6Saim %in% 5:13 ~ 1,
    W6Saim %in% c(14, 15) ~ 3,
    W6Saim == 16 ~ 5,
    W6Saim >= -1 & W6Saim <= -999 ~ -3,
    TRUE ~ -3
  ))

# Age 20 (W7SAim)
cohort <- cohort %>%
  mutate(educaim20 = case_when(
    W7SAim %in% 10:13 ~ 0,
    W7SAim %in% 1:9 ~ 1,
    W7SAim == 14 ~ 3,
    W7SAim == -94 ~ -8,
    W7SAim == -91 ~ -1,
    W7SAim >= -1 & W7SAim <= -999 ~ -3,
    TRUE ~ -3
  ))

# Age 25 (S8)
cohort <- cohort %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 == 0 ~ 5,
    W8ACTIVITY05 == 1 & (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0K == 1) ~ 0,
    W8ACTIVITY05 == 1 & (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0E == 1 | W8VCQUC0J == 1) ~ 1,
    W8ACTIVITY05 == 1 & (W8VCQUC0C == 1 | W8VCQUC0D == 1) ~ 2,
    W8ACTIVITY05 == 1 & W8ACQUC0N == 1 ~ 3,
    W8ACTIVITY05 == 1 & W8ACQUC0O == 1 ~ 4,
    W8ACTIVITY05 == -9 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    W8ACTIVITY05 == -1 ~ -1,
    TRUE ~ -3
  ))

# Age 32 (S9)
cohort <- cohort %>%
  mutate(educaim32 = case_when(
    W9ECONACT2 %in% c(1, 2, 3, 4, 5, 8, 9, 10, 11, 13, 14) ~ 5,
    W9ECONACT2 %in% c(6, 7, 12) & (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0S == 1 | W9VCQUC0V == 1 | W9VCQUCAC == 1) ~ 0,
    W9ECONACT2 %in% c(6, 7, 12) & (W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9VCQUC0B == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0R == 1 | W9VCQUC0T == 1 | W9VCQUC0U == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1) ~ 1,
    W9ECONACT2 %in% c(6, 7, 12) & (W9VCQUC0K == 1 | W9VCQUCAD == 1 | W9VCQUCAE == 1) ~ 2,
    W9ECONACT2 %in% c(6, 7, 12) & W9ACQUC0R == 1 ~ 3,
    W9ECONACT2 %in% c(6, 7, 12) & W9ACQUC0S == 1 ~ 4,
    W9ECONACT2 == -9 ~ -9,
    W9ECONACT2 == -8 ~ -8,
    W9ECONACT2 == -3 ~ -3,
    W9ECONACT2 == -1 ~ -1,
    TRUE ~ -3
  ))

# Final Selection
final_data <- cohort %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

write_csv(final_data, 'data/output/cleaned_data.csv')
