library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

load_data <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric'))
}

# We need to read NSID as string
load_data_nsid <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'))
}

df1 <- load_data_nsid('wave_one_lsype_young_person_2020.tab')
df4 <- load_data_nsid('wave_four_lsype_young_person_2020.tab')
df6 <- load_data_nsid('wave_six_lsype_young_person_2020.tab')
df7 <- load_data_nsid('wave_seven_lsype_young_person_2020.tab')
df8 <- load_data_nsid('ns8_2015_main_interview.tab')
df9 <- load_data_nsid('ns9_2022_main_interview.tab')

full_frame <- df1 %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df6, by = 'NSID') %>%
  full_join(df7, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# Harmonisation Logic
# Categories:
# 0 = NVQ 4–5 equivalent
# 1 = NVQ 1–3 equivalent
# 2 = None / entry level
# 3 = Other
# 4 = None of these qualifications
# 5 = Not currently studying

# Wave 4 (Age 17)
full_frame <- full_frame %>%
  mutate(edu_17 = case_when(
    w4saim == 14 ~ 5,
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,
    w4saim %in% c(12, 13) ~ 3,
    w4saim >= -999 & w4saim <= -1 ~ -3, # Standard missing mapping
    TRUE ~ -3
  ))

# Wave 6 (Age 19)
full_frame <- full_frame %>%
  mutate(edu_19 = case_when(
    W6Saim == 16 ~ 5,
    W6Saim %in% c(1, 2, 3, 4) ~ 0,
    W6Saim %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13) ~ 1,
    W6Saim %in% c(14, 15) ~ 3,
    W6Saim >= -999 & W6Saim <= -1 ~ -3,
    TRUE ~ -3
  ))

# Wave 7 (Age 20)
full_frame <- full_frame %>%
  mutate(edu_20 = case_when(
    W7SAim == -91 ~ 5,
    W7SAim %in% c(10, 11, 12, 13) ~ 0,
    W7SAim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,
    W7SAim == 14 ~ 3,
    W7SAim == -94 ~ -8, # Insufficient info
    W7SAim >= -999 & W7SAim <= -1 ~ -3,
    TRUE ~ -3
  ))

# Wave 8 (Age 25)
# Priority: NVQ 4-5 -> NVQ 1-3 -> Entry -> Other -> None of these
# NVQ 4-5: W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E, W8VCQUC0J, W8VCQUC0K
# NVQ 1-3: W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J, W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8VCQUC0A, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E
# Entry: W8VCQUC0D (Wait, Wales entry is 2)

full_frame <- full_frame %>%
  mutate(edu_25 = case_when(
    W8ACTIVITY05 == 0 ~ 5,
    # NVQ 4-5
    (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1) ~ 0,
    # NVQ 1-3
    (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0E == 1) ~ 1,
    # Entry
    (W8VCQUC0D == 1) ~ 2,
    # None of these
    (W8ACQUC0O == 1) ~ 4,
    # Other/Unknown
    (W8ACQUC0P == 1 | W8ACQUC0Q == 1) ~ -3,
    W8ACTIVITY05 == -9 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    W8ACTIVITY05 == -1 ~ -1,
    TRUE ~ -3
  ))

# Wave 9 (Age 32)
# Priority: NVQ 4-5 -> NVQ 1-3 -> Entry -> Other -> None of these
# NVQ 4-5: W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E, W9VCQUC0A, W9VCQUC0C, W9VCQUC0R, W9VCQUC0S, W9VCQUC0V
# NVQ 1-3: W9ACQUC0F, W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J, W9ACQUC0K, W9ACQUC0L, W9ACQUC0M, W9ACQUC0N, W9ACQUC0O, W9ACQUC0P, W9VCQUC0B, W9VCQUC0D, W9VCQUC0E, W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I, W9VCQUC0J, W9VCQUC0P, W9VCQUC0Q, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y, W9VCQUC0Z, W9VCQUCAA, W9VCQUCAB
# Entry: W9VCQUC0K, W9VCQUCAE

full_frame <- full_frame %>%
  mutate(edu_32 = case_when(
    W9ECONACT2 %in% c(1, 2, 3, 4, 5, 9, 10, 11, 13, 14) ~ 5,
    # NVQ 4-5
    (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0V == 1) ~ 0,
    # NVQ 1-3
    (W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9VCQUC0B == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1) ~ 1,
    # Entry
    (W9VCQUC0K == 1 | W9VCQUCAE == 1) ~ 2,
    # None of these
    (W9ACQUC0S == 1 | W9VCQUCAG == 1) ~ 4,
    # Others
    (W9ACQUC0R == 1 | W9VCQUCAF == 1) ~ 3,
    # Missing
    W9ECONACT2 == -9 ~ -9,
    W9ECONACT2 == -8 ~ -8,
    W9ECONACT2 == -3 ~ -3,
    W9ECONACT2 == -1 ~ -1,
    TRUE ~ -3
  ))

# Final selection
final_data <- full_frame %>%
  select(NSID, edu_17, edu_19, edu_20, edu_25, edu_32)

# Create labels
edu_labels <- c(
  '0' = 'NVQ 4–5 equivalent',
  '1' = 'NVQ 1–3 equivalent',
  '2' = 'None / entry level',
  '3' = 'Other',
  '4' = 'None of these qualifications',
  '5' = 'Not currently studying',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Schedule not applicable',
  '-1' = 'Item not applicable'
)

# Apply labels to factors
final_data <- final_data %>%
  mutate(across(starts_with('edu'), ~ factor(.x, levels = as.numeric(names(edu_labels)), labels = edu_labels)))

write_csv(final_data, 'data/output/cleaned_data.csv')
