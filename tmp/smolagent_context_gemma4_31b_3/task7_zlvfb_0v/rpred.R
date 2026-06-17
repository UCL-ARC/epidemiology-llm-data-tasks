library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load Files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'), trim_ws = TRUE)
})

names(data_list) <- files

# Merge datasets
full_frame <- data_list[[1]] %>%
  full_join(data_list[[2]], by = 'NSID') %>%
  full_join(data_list[[3]], by = 'NSID') %>%
  full_join(data_list[[4]], by = 'NSID') %>%
  full_join(data_list[[5]], by = 'NSID') %>%
  full_join(data_list[[6]], by = 'NSID')

# --- Recoding Logic ---

# Age 17: w4saim
full_frame <- full_frame %>%
  mutate(educaim17 = case_when(
    w4saim %in% 1:11 ~ 1,
    w4saim %in% 12:13 ~ 3,
    w4saim == 14 ~ 5,
    w4saim >= -999 & w4saim <= -1 ~ -3,
    TRUE ~ -3
  ))

# Age 19: W6Saim
full_frame <- full_frame %>%
  mutate(educaim19 = case_when(
    W6Saim %in% 1:4 ~ 0,
    W6Saim %in% 5:13 ~ 1,
    W6Saim %in% 14:15 ~ 3,
    W6Saim == 16 ~ 5,
    W6Saim >= -999 & W6Saim <= -1 ~ -3,
    TRUE ~ -3
  ))

# Age 20: W7SAim
full_frame <- full_frame %>%
  mutate(educaim20 = case_when(
    W7SAim %in% 10:13 ~ 0,
    W7SAim %in% 1:9 ~ 1,
    W7SAim == 14 ~ 3,
    W7SAim == -91 ~ 5,
    W7SAim == -94 ~ -8,
    W7SAim >= -999 & W7SAim <= -1 ~ -3,
    TRUE ~ -3
  ))

# Age 25: W8
full_frame <- full_frame %>%
  mutate(
    w8_high = (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1),
    w8_low = (W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0E == 1),
    w8_entry = (W8VCQUC0D == 1),
    w8_other = (W8ACQUC0N == 1),
    w8_none = (W8ACQUC0O == 1),
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5,
      w8_high ~ 0,
      w8_low ~ 1,
      w8_entry ~ 2,
      w8_other ~ 3,
      w8_none ~ 4,
      W8ACQUC0Q == 1 ~ -9,
      W8ACQUC0P == 1 ~ -8,
      TRUE ~ -3
    )
  )

# Age 32: W9
full_frame <- full_frame %>%
  mutate(
    w9_is_studying = (W9ECONACT2 %in% c(6, 7, 12)),
    w9_high = (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0S == 1 | W9VCQUC0V == 1),
    w9_low = (W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9VCQUC0B == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1),
    w9_entry = (W9VCQUC0K == 1),
    w9_other = (W9ACQUC0R == 1),
    w9_none = (W9ACQUC0S == 1),
    educaim32 = case_when(
      !w9_is_studying & !is.na(W9ECONACT2) ~ 5,
      w9_high ~ 0,
      w9_low ~ 1,
      w9_entry ~ 2,
      w9_other ~ 3,
      w9_none ~ 4,
      W9ACQUC0U == 1 ~ -9,
      W9ACQUC0T == 1 ~ -8,
      TRUE ~ -3
    )
  )

# Final selection and factor labeling
final_data <- full_frame %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

labels_educ <- c(
  '0' = 'NVQ 4–5 equivalent (higher / HE-level qualifications)',
  '1' = 'NVQ 1–3 equivalent (lower / mid-level qualifications)',
  '2' = 'None / entry level',
  '3' = 'Other (level unknown or unclassifiable)',
  '4' = 'None of these qualifications',
  '5' = 'Not currently studying',
  '-9' = 'Refusal',
  '-8' = 'Dont know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

for (var in c('educaim17', 'educaim19', 'educaim20', 'educaim25', 'educaim32')) {
  final_data[[var]] <- factor(final_data[[var]], levels = as.numeric(names(labels_educ)), labels = labels_educ)
}

write_csv(final_data, 'data/output/cleaned_data.csv')
