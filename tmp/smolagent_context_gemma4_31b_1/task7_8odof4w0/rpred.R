library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
files <- c('wave_one_lsype_young_person_2020.tab', 'wave_four_lsype_young_person_2020.tab', 'wave_six_lsype_young_person_2020.tab', 'wave_seven_lsype_young_person_2020.tab', 'ns8_2015_main_interview.tab', 'ns9_2022_main_interview.tab')

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))
names(data_list) <- files

# Merge all datasets by NSID
full_frame <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]], by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID') %>% 
  full_join(data_list[[6]], by = 'NSID')

# --- Age 17 (w4saim) ---
# Scheme: 0=HE, 1=L/M, 2=Entry, 3=Other, 4=None of these, 5=Not studying
full_frame <- full_frame %>% 
  mutate(educaim17 = case_when(
    w4saim == 14 ~ 5,
    w4saim %in% c(1, 2, 3, 4) ~ 1, # NVQ3, AVCE, A/AS, Other L3
    w4saim %in% c(5, 6, 7, 8) ~ 1, # NVQ2, Int GNVQ, Other L2, GCSE
    w4saim %in% c(9, 10, 11) ~ 2, # NVQ1, Foundation, Other L1
    w4saim %in% c(12, 13) ~ 3,    # Other, No detail
    TRUE ~ -3
  ))
# Correcting the above: NVQ 1-3 is category 1. Entry level is 2.
# Re-mapping based on labels:
# 1.0: NVQ 3, 2.0: AVCE, 3.0: A/AS, 4.0: Other level 3 -> 1
# 5.0: NVQ 2, 6.0: Intermediate GNVQ, 7.0: Other level 2, 8.0: GCSE -> 1
# 9.0: NVQ 1, 10.0: Foundation, 11.0: Other level 1 -> 2
# 12.0: Other, 13.0: No detail -> 3
# 14.0: Not studying -> 5
full_frame <- full_frame %>% 
  mutate(educaim17 = case_when(
    w4saim == 14 ~ 5,
    w4saim %in% 1:8 ~ 1,
    w4saim %in% 9:11 ~ 2,
    w4saim %in% 12:13 ~ 3,
    w4saim >= -999 & w4saim <= -1 ~ -3, # Default missing for this wave
    TRUE ~ -3
  ))

# --- Age 19 (W6Saim) ---
# 1: NVQ5, 2: Degree, 3: NVQ4, 4: Other HE -> 0
# 5: NVQ3, 6: AVCE, 7: A/AS, 8: Other L3 -> 1
# 9: NVQ2, 10: Other L2, 11: GCSE -> 1
# 12: NVQ1, 13: Other L1 -> 2
# 14: Other (level unknown), 15: No detail -> 3
# 16: Not studying -> 5
full_frame <- full_frame %>% 
  mutate(educaim19 = case_when(
    W6Saim == 16 ~ 5,
    W6Saim %in% 1:4 ~ 0,
    W6Saim %in% 5:11 ~ 1,
    W6Saim %in% 12:13 ~ 2,
    W6Saim %in% 14:15 ~ 3,
    W6Saim >= -999 & W6Saim <= -1 ~ -3,
    TRUE ~ -3
  ))

# --- Age 20 (W7SAim) ---
# -94: Insufficient info -> -8
# -91: Not applicable (not studying) -> 5
# 1: NVQ1, 2: Other L1 -> 2
# 3: NVQ2, 4: GCSE, 5: Other L2 -> 1
# 6: NVQ3, 7: A/AS, 8: AVCE, 9: Other L3 -> 1
# 10: NVQ4, 11: Degree, 12: Other HE, 13: NVQ5 -> 0
# 14: Other (level unknown) -> 3
full_frame <- full_frame %>% 
  mutate(educaim20 = case_when(
    W7SAim == -94 ~ -8,
    W7SAim == -91 ~ 5,
    W7SAim %in% 1:2 ~ 2,
    W7SAim %in% 3:9 ~ 1,
    W7SAim %in% 10:13 ~ 0,
    W7SAim == 14 ~ 3,
    W7SAim >= -999 & W7SAim <= -1 ~ -3,
    TRUE ~ -3
  ))

# --- Age 25 (Wave 8) ---
# Logic: if activity not studying -> 5
# priority: NVQ 4-5 -> 0; NVQ 1-3 -> 1; Entry -> 2; Other -> 3; None -> 4
# W8ACTIVITY05: 1=Yes (studying), 0=No
full_frame <- full_frame %>% 
  mutate(
    # Check for studying
    is_studying_25 = case_when(
      W8ACTIVITY05 == 1 ~ TRUE,
      W8ACTIVITY05 == 0 ~ FALSE,
      W8ACTIVITY05 == -9 ~ -9,
      W8ACTIVITY05 == -8 ~ -8,
      TRUE ~ -3
    ),
    # NVQ 4-5 indicators
    nvq45_25 = if_else(W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1, 1, 0),
    # NVQ 1-3 indicators
    nvq13_25 = if_else(W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8VCQUC0E == 1, 1, 0),
    # Entry indicators
    entry_25 = if_else(W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0D == 1, 1, 0),
    # Other indicators
    other_25 = if_else(W8ACQUC0D == 1 | W8ACQUC0E == 1, 1, 0),
    # None indicators
    none_25 = if_else(W8ACQUC0O == 1, 1, 0),
    # Missing check
    refused_25 = if_else(W8ACQUC0Q == 1, 1, 0),
    dk_25 = if_else(W8ACQUC0P == 1, 1, 0),
    
    educaim25 = case_when(
      is_studying_25 == FALSE ~ 5,
      nvq45_25 == 1 ~ 0,
      nvq13_25 == 1 ~ 1,
      entry_25 == 1 ~ 2,
      other_25 == 1 ~ 3,
      none_25 == 1 ~ 4,
      refused_25 == 1 ~ -9,
      dk_25 == 1 ~ -8,
      TRUE ~ -3
    )
  )

# --- Age 32 (Wave 9) ---
# W9ECONACT2: 6, 7, 12 = studying. Others = not studying (5)
full_frame <- full_frame %>% 
  mutate(
    is_studying_32 = case_when(
      W9ECONACT2 %in% c(6, 7, 12) ~ TRUE,
      W9ECONACT2 %in% c(1, 2, 3, 4, 5, 8, 9, 10, 11, 13, 14) ~ FALSE,
      W9ECONACT2 == -9 ~ -9,
      W9ECONACT2 == -8 ~ -8,
      W9ECONACT2 == -3 ~ -3,
      W9ECONACT2 == -1 ~ -1,
      TRUE ~ -3
    ),
    # NVQ 4-5: W9ACQUC0A, B, C, D, E, W9VCQUC0A, C, S, V
    nvq45_32 = if_else(W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0S == 1 | W9VCQUC0V == 1, 1, 0),
    # NVQ 1-3: W9ACQUC0F, G, H, I, J, K, L, M, N, O, P, Q, R, W9VCQUC0B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, T, U, X, Y, Z, AA, AB
    nvq13_32 = if_else(W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9ACQUC0R == 1 | W9VCQUC0B == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0R == 1 | W9VCQUC0T == 1 | W9VCQUC0U == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1, 1, 0),
    # Entry: W9VCQUCAE, AF (Junior cert)
    entry_32 = if_else(W9VCQUCAE == 1 | W9VCQUCAF == 1, 1, 0),
    # Other
    other_32 = if_else(W9ACQUC0R == 1, 1, 0),
    # None
    none_32 = if_else(W9ACQUC0S == 1 | W9VCQUCAG == 1, 1, 0),
    # Missing
    refused_32 = if_else(W9ACQUC0U == 1 | W9VCQUCAI == 1, 1, 0),
    dk_32 = if_else(W9ACQUC0T == 1 | W9VCQUCAH == 1, 1, 0),

    educaim32 = case_when(
      is_studying_32 == FALSE ~ 5,
      nvq45_32 == 1 ~ 0,
      nvq13_32 == 1 ~ 1,
      entry_32 == 1 ~ 2,
      other_32 == 1 ~ 3,
      none_32 == 1 ~ 4,
      refused_32 == 1 ~ -9,
      dk_32 == 1 ~ -8,
      TRUE ~ -3
    )
  )

# Final selection
final_data <- full_frame %>% 
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Factor labelling
labels_map <- c("0" = "NVQ 4–5 equivalent", "1" = "NVQ 1–3 equivalent", "2" = "None / entry level", "3" = "Other", "4" = "None of these qualifications", "5" = "Not currently studying", "-9" = "Refusal", "-8" = "Don't know", "-3" = "Not asked")

# Apply labels
final_data <- final_data %>% 
  mutate(across(starts_with("educaim"), ~as.factor(.x))) # Simple factor for now, detailed labels can be added via labelled package

# Save as CSV
write_csv(final_data, "data/output/cleaned_data.csv")
