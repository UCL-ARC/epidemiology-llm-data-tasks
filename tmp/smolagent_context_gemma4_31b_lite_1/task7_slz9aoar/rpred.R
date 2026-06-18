library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))
names(data_list) <- files

# Merge datasets
full_frame <- data_list[[1]] %>% select(NSID)
for (i in 2:length(data_list)) {
  full_frame <- full_join(full_frame, data_list[[i]], by = 'NSID')
}

# Define the 6-category scheme based on NVQ levels
# 1: NVQ 1 / Level 1
# 2: NVQ 2 / Level 2
# 3: NVQ 3 / Level 3
# 4: NVQ 4 / Level 4
# 5: NVQ 5 / Level 5
# 6: Degree / Higher Education

# Wave 4 (Age 17)
full_frame <- full_frame %>%
  mutate(educaim17 = case_when(
    w4saim == 9 | w4saim == 10 | w4saim == 11 ~ 1,
    w4saim == 5 | w4saim == 6 | w4saim == 7 | w4saim == 8 ~ 2,
    w4saim == 1 | w4saim == 2 | w4saim == 3 | w4saim == 4 ~ 3,
    w4saim == 12 ~ 3, # Other - likely L3 if at 17 and not L1/2
    w4saim == 13 ~ -8, # No detail
    w4saim == 14 ~ -1, # Not studying
    w4saim >= -1 & w4saim <= -999 ~ -2,
    TRUE ~ -3
  ))

# Wave 6 (Age 19)
full_frame <- full_frame %>%
  mutate(educaim19 = case_when(
    W6Saim == 12 | W6Saim == 13 ~ 1,
    W6Saim == 9 | W6Saim == 10 | W6Saim == 11 ~ 2,
    W6Saim == 5 | W6Saim == 6 | W6Saim == 7 | W6Saim == 8 ~ 3,
    W6Saim == 3 ~ 4,
    W6Saim == 1 ~ 5,
    W6Saim == 2 | W6Saim == 4 ~ 6,
    W6Saim == 14 ~ 3, # Other level unknown
    W6Saim == 15 ~ -8, # No detail
    W6Saim == 16 ~ -1, # Not studying
    W6Saim >= -1 & W6Saim <= -999 ~ -2,
    TRUE ~ -3
  ))

# Wave 7 (Age 20)
full_frame <- full_frame %>%
  mutate(educaim20 = case_when(
    W7SAim == 1 | W7SAim == 2 ~ 1,
    W7SAim == 3 | W7SAim == 4 | W7SAim == 5 ~ 2,
    W7SAim == 6 | W7SAim == 7 | W7SAim == 8 | W7SAim == 9 ~ 3,
    W7SAim == 10 ~ 4,
    W7SAim == 13 ~ 5,
    W7SAim == 11 | W7SAim == 12 ~ 6,
    W7SAim == 14 ~ 3, # Other unknown
    W7SAim == -94 ~ -8, # Insufficient info
    W7SAim == -91 ~ -1, # Not studying
    W7SAim >= -1 & W7SAim <= -999 ~ -2,
    TRUE ~ -3
  ))

# Wave 8 (Age 25)
# Logic: Highest qualification currently studying
full_frame <- full_frame %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 == 0 ~ -1, # Not in education
    W8ACQUC0A == 1 | W8ACQUC0B == 1 ~ 6,
    W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 ~ 5, # HE Diplomas etc
    W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 ~ 3,
    W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 ~ 2,
    W8VCQUC0K == 1 ~ 5, # HNC/HND
    W8VCQUC0J == 1 ~ 3, # NVQ 3-5 (lowest is 3)
    W8VCQUC0E == 1 ~ 2, # Apprenticeship
    W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1 ~ 1,
    W8ACQUC0O == 1 ~ 3, # None of above (but in education)
    W8ACTIVITY05 == -9 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    W8ACTIVITY05 == -1 ~ -1,
    TRUE ~ -3
  ))

# Wave 9 (Age 32)
# Logic: Highest qualification currently studying
full_frame <- full_frame %>%
  mutate(educaim32 = case_when(
    W9ECONACT2 == 6 | W9ECONACT2 == 7 | W9ECONACT2 == 12 ~ 1, # Start with education
    TRUE ~ -1 # Default for not in education
  )) %>%
  mutate(educaim32 = case_when(
    educaim32 == -1 ~ -1,
    W9ACQUC0A == 1 | W9ACQUC0B == 1 ~ 6,
    W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 ~ 5,
    W9ACQUC0F == 1 | W9ACQUC0G == 1 ~ 3,
    W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 ~ 2,
    W9VCQUC0A == 1 ~ 6,
    W9VCQUC0B == 1 ~ 5,
    W9VCQUC0C == 1 ~ 4,
    W9VCQUC0D == 1 | W9VCQUC0I == 1 | W9VCQUC0O == 1 ~ 3,
    W9VCQUC0E == 1 | W9VCQUC0J == 1 | W9VCQUC0P == 1 ~ 2,
    W9VCQUC0F == 1 | W9VCQUC0Q == 1 ~ 1,
    W9VCQUC0G == 1 ~ 3, # GNVQ Adv
    W9VCQUC0H == 1 ~ 2, # GNVQ Int
    W9VCQUC0K == 1 ~ 1, # Foundation
    W9ACQUC0S == 1 ~ 3, # None of these but in education
    W9ECONACT2 == -9 ~ -9,
    W9ECONACT2 == -8 ~ -8,
    W9ECONACT2 == -3 ~ -3,
    W9ECONACT2 == -1 ~ -1,
    TRUE ~ -3
  ))

# Final formatting
final_data <- full_frame %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Factor labels for the 6-category scheme
edu_labels <- c(
  '1' = 'NVQ 1',
  '2' = 'NVQ 2',
  '3' = 'NVQ 3',
  '4' = 'NVQ 4',
  '5' = 'NVQ 5',
  '6' = 'Degree/HE',
  '-1' = 'Not applicable',
  '-2' = 'Schedule not applicable',
  '-3' = 'Not asked',
  '-8' = 'Don\'t know',
  '-9' = 'Refusal'
)

# Apply labels to factors
final_data <- final_data %>%
  mutate(across(starts_with('educaim'), ~factor(.x, levels = as.numeric(names(edu_labels)), labels = edu_labels)))

write_csv(final_data, 'data/output/cleaned_data.csv')
