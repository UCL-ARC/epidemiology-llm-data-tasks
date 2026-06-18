library(haven)
library(dplyr)
library(tidyr)
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

data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric'))
})

# The ID NSID is a string, we need to ensure it is handled correctly. 
# Since read_delim was set to numeric default, let's re-read with NSID as character
data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'))
})

names(data_list) <- files

# Merge datasets
full_frame <- data_list[[1]] %>% 
  full_join(data_list[[2]], by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID') %>% 
  full_join(data_list[[6]], by = 'NSID')

# 2. Derivation Logic

# Wave 4 (Age 17)
full_frame <- full_frame %>% mutate(
  educaim17 = case_when(
    w4saim == 1 | w4saim == 2 | w4saim == 3 | w4saim == 4 ~ 1,
    w4saim == 5 | w4saim == 6 | w4saim == 7 | w4saim == 8 ~ 1,
    w4saim == 9 | w4saim == 10 | w4saim == 11 ~ 2,
    w4saim == 12 | w4saim == 13 ~ 3,
    w4saim == 14 ~ 5,
    w4saim == -92 ~ -9, # Standard pattern sanity check
    w4saim == -99 ~ -3,
    is.na(w4saim) ~ -3,
    TRUE ~ -3
  )
)
# Correction for w4saim based on labels: 
# 1-4 (NVQ3, AVCE, A/AS, Other L3) -> 1
# 5-8 (NVQ2, Int GNVQ, Other L2, GCSE) -> 1 (Wait, NVQ 1-3 equivalent is category 1)
# 9-11 (NVQ1, Foundation, Other L1) -> 2 (Entry level)
# 12-13 (Other, No detail) -> 3
# 14 (Not studying) -> 5
# Re-mapping based on metadata labels strictly:
full_frame <- full_frame %>% mutate(
  educaim17 = case_when(
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1, # NVQ 3 to GCSE (L2/3)
    w4saim %in% c(9, 10, 11) ~ 2, # NVQ 1, Foundation, Other L1
    w4saim %in% c(12, 13) ~ 3, # Other/No detail
    w4saim == 14 ~ 5, # Not studying
    w4saim >= -1 & w4saim < 0 ~ -3, # user_missing_values: -999 thru -1
    is.na(w4saim) ~ -3,
    TRUE ~ -3
  )
)

# Wave 6 (Age 19)
full_frame <- full_frame %>% mutate(
  educaim19 = case_when(
    W6Saim %in% c(1, 2, 3, 4) ~ 0, # NVQ5, Degree, NVQ4, Other HE
    W6Saim %in% c(5, 6, 7, 8) ~ 1, # NVQ3, AVCE, A/AS, Other L3
    W6Saim %in% c(9, 10, 11) ~ 1, # NVQ2, Other L2, GCSE (L2 is mid-level)
    W6Saim %in% c(12, 13) ~ 2, # NVQ1, Other L1
    W6Saim %in% c(14, 15) ~ 3, # Other, No detail
    W6Saim == 16 ~ 5, # Not studying
    W6Saim >= -1 & W6Saim < 0 ~ -3,
    is.na(W6Saim) ~ -3,
    TRUE ~ -3
  )
)

# Wave 7 (Age 20)
full_frame <- full_frame %>% mutate(
  educaim20 = case_when(
    W7SAim == -94 ~ -8, # Insufficient information
    W7SAim == -91 ~ 5, # Not applicable (not studying)
    W7SAim %in% c(10, 11, 12, 13) ~ 0, # NVQ4, Degree, Other HE, NVQ5
    W7SAim %in% c(6, 7, 8, 9) ~ 1, # NVQ3, A/AS, AVCE, Other L3
    W7SAim %in% c(3, 4, 5) ~ 1, # NVQ2, GCSE, Other L2
    W7SAim %in% c(1, 2) ~ 2, # NVQ1, Other L1
    W7SAim == 14 ~ 3, # Other
    W7SAim >= -1 & W7SAim < 0 ~ -3,
    is.na(W7SAim) ~ -3,
    TRUE ~ -3
  )
)

# Wave 8 (Age 25)
# Priority: Not studying (W8ACTIVITY05 == 0) -> 5
# Then NVQ 4-5 -> 0, NVQ 1-3 -> 1, Entry -> 2, Other -> 3, None -> 4
# For W8, we need to define which columns are which level.
# HE/NVQ 4-5: W8ACQUC0A, B, C, D, E, W8VCQUC0J, K
# Mid/Low (NVQ 1-3): W8ACQUC0F, G, H, I, J, K, L, W8VCQUC0E
# Entry: W8ACQUC0M, N, W8VCQUC0A, B, C, D
# Other: W8ACQUC0O (None of these) -> 4

full_frame <- full_frame %>% mutate(
  is_studying_25 = W8ACTIVITY05 == 1,
  he_25 = (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1),
  mid_25 = (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8VCQUC0E == 1),
  entry_25 = (W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1),
  none_25 = (W8ACQUC0O == 1),
  # Refusal/DK: W8ACQUC0P (DK), W8ACQUC0Q (Refused)
  dk_25 = (W8ACQUC0P == 1),
  ref_25 = (W8ACQUC0Q == 1)
)

full_frame <- full_frame %>% mutate(
  educaim25 = case_when(
    is.na(W8ACTIVITY05) ~ -3,
    W8ACTIVITY05 == 0 ~ 5,
    he_25 ~ 0,
    mid_25 ~ 1,
    entry_25 ~ 2,
    none_25 ~ 4,
    ref_25 ~ -9,
    dk_25 ~ -8,
    TRUE ~ -3
  )
)

# Wave 9 (Age 32)
# Economic activity: 6, 7, 12 are studying. Others are not.
full_frame <- full_frame %>% mutate(
  is_studying_32 = W9ECONACT2 %in% c(6, 7, 12),
  he_32 = (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0S == 1 | W9VCQUC0V == 1),
  # Note: W9VCQUC0S is Higher Diploma, V is Higher BTEC. C is L4/5.
  mid_32 = (W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9VCQUC0D == 1 | W9VCQUC0I == 1 | W9VCQUC0O == 1),
  entry_32 = (W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0J == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0K == 1),
  none_32 = (W9ACQUC0S == 1 | W9VCQUCAG == 1),
  dk_32 = (W9ACQUC0T == 1 | W9VCQUCAH == 1),
  ref_32 = (W9ACQUC0U == 1 | W9VCQUCAI == 1)
)

full_frame <- full_frame %>% mutate(
  educaim32 = case_when(
    is.na(W9ECONACT2) ~ -3,
    !is_studying_32 ~ 5,
    he_32 ~ 0,
    mid_32 ~ 1,
    entry_32 ~ 2,
    none_32 ~ 4,
    ref_32 ~ -9,
    dk_32 ~ -8,
    TRUE ~ -3
  )
)

# Final Selection
final_df <- full_frame %>% 
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Factor Labels
labels_vec <- c(
  '0' = 'NVQ 4–5 equivalent (higher / HE-level qualifications)',
  '1' = 'NVQ 1–3 equivalent (lower / mid-level qualifications)',
  '2' = 'None / entry level',
  '3' = 'Other (level unknown or unclassifiable)',
  '4' = 'None of these qualifications',
  '5' = 'Not currently studying',
  '-9' = 'Refusal',
  '-8' = "Don't know / insufficient information",
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Apply factors
final_df <- final_df %>% 
  mutate(across(starts_with('educaim'), ~ factor(.x, levels = as.numeric(names(labels_vec)), labels = labels_vec)))

write_csv(final_df, 'data/output/cleaned_data.csv')
