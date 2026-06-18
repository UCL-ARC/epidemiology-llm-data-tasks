library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_six <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Combine all datasets with full join
data_full <- full_join(wave_one, wave_four, by = 'NSID')
data_full <- full_join(data_full, wave_six, by = 'NSID')
data_full <- full_join(data_full, wave_seven, by = 'NSID')
data_full <- full_join(data_full, ns8, by = 'NSID')
data_full <- full_join(data_full, ns9, by = 'NSID')

# Convert R NA values to -3
data_full <- data_full %>%
  mutate(across(everything(), ~ifelse(is.na(.), -3, .)))

# ========================================
# Age 17 (educaim17) - from w4saim
# ========================================
w4saim_values <- data_full$w4saim

# NVQ 1-3 equivalent -> 1: NVQ 3 (1), AVCE (2), A/AS (3), NVQ 2 (5), NVQ 1 (9), Other level 2 (7), Other level 3 (4)
# Entry level -> 2: GCSE (8), Foundation (10), Other level 1 (11)
# Other (level unknown) -> 3: Other (12), No detail (13)
# Not studying -> 5: Not studying (14)

educaim17 <- case_when(
  w4saim_values %in% c(1, 2, 3, 5, 9, 7, 4) ~ 1,
  w4saim_values %in% c(8, 10, 11) ~ 2,
  w4saim_values %in% c(12, 13) ~ 3,
  w4saim_values == 14 ~ 5,
  TRUE ~ -3
)
data_full <- data_full %>% mutate(educaim17 = educaim17)

# ========================================
# Age 19 (educaim19) - from W6Saim
# ========================================
W6Saim_values <- data_full$W6Saim

# NVQ 4-5 equivalent -> 0: NVQ 5 (1), First/Other Degree (2), NVQ 4 (3), Other HE (4)
# NVQ 1-3 equivalent -> 1: NVQ 3 (5), NVQ 2 (9), NVQ 1 (12), AVCE (6), A/AS (7), Other level 3 (8)
# Entry level -> 2: GCSE (11), Other level 1 (13)
# Other -> 3: Other (level unknown) (14), No detail (15)
# Not studying -> 5: Not studying (16)

educaim19 <- case_when(
  W6Saim_values %in% c(1, 2, 3, 4) ~ 0,
  W6Saim_values %in% c(5, 9, 12, 6, 7, 8) ~ 1,
  W6Saim_values %in% c(11, 13) ~ 2,
  W6Saim_values %in% c(14, 15) ~ 3,
  W6Saim_values == 16 ~ 5,
  TRUE ~ -3
)
data_full <- data_full %>% mutate(educaim19 = educaim19)

# ========================================
# Age 20 (educaim20) - from W7SAim
# ========================================
W7SAim_values <- data_full$W7SAim

# NVQ 4-5 equivalent -> 0: NVQ 4 (10), NVQ 5 (13), First/Other Degree (11), Other HE (12)
# NVQ 1-3 equivalent -> 1: NVQ 2 (3), NVQ 3 (6), A/AS (7), AVCE (8), Other level 3 (9)
# Entry level -> 2: NVQ 1 (1), Other level 1 (2), GCSE (4), Other level 2 (5)
# Other (level unknown) -> 3: Other (level unknown) (14)
# Not studying -> 5: Not applicable (not studying) (-91)
# Insufficient information -> -8: Insufficient information (-94)

educaim20 <- case_when(
  W7SAim_values %in% c(10, 13, 11, 12) ~ 0,
  W7SAim_values %in% c(3, 6, 7, 8, 9) ~ 1,
  W7SAim_values %in% c(1, 2, 4, 5) ~ 2,
  W7SAim_values == 14 ~ 3,
  W7SAim_values == -91 ~ 5,
  W7SAim_values == -94 ~ -8,
  TRUE ~ -3
)
data_full <- data_full %>% mutate(educaim20 = educaim20)

# ========================================
# Age 25 (educaim25) - from ns8
# ========================================
W8ACTIVITY05_values <- data_full$W8ACTIVITY05
W8VCQUC0J_values <- data_full$W8VCQUC0J
W8VCQUC0D_values <- data_full$W8VCQUC0D
W8VCQUC0A_values <- data_full$W8VCQUC0A
W8VCQUC0O_values <- data_full$W8VCQUC0O

not_studying_25 <- W8ACTIVITY05_values == 1
has_nvq4_5 <- W8VCQUC0J_values == 1
has_entry <- W8VCQUC0D_values == 1
has_other <- W8VCQUC0A_values == 1
has_none <- W8VCQUC0O_values == 1

educaim25 <- case_when(
  not_studying_25 ~ 5,
  has_nvq4_5 ~ 0,
  has_entry ~ 2,
  has_other ~ 3,
  has_none ~ 4,
  TRUE ~ -3
)
data_full <- data_full %>% mutate(educaim25 = educaim25)

# ========================================
# Age 32 (educaim32) - from ns9
# ========================================
W9ECONACT2_values <- data_full$W9ECONACT2
W9VCQUC0C_values <- data_full$W9VCQUC0C
W9VCQUC0D_values <- data_full$W9VCQUC0D
W9VCQUC0F_values <- data_full$W9VCQUC0F
W9ACQUC0S_values <- data_full$W9ACQUC0S

not_studying_32 <- W9ECONACT2_values %in% c(6, 7, 12)
has_nvq_he <- W9VCQUC0C_values == 1
has_nvq_mid <- W9VCQUC0D_values == 1
has_entry <- W9VCQUC0F_values == 1
has_none <- W9ACQUC0S_values == 1

educaim32 <- case_when(
  not_studying_32 ~ 5,
  has_nvq_he ~ 0,
  has_nvq_mid ~ 1,
  has_entry ~ 2,
  has_none ~ 4,
  TRUE ~ -3
)
data_full <- data_full %>% mutate(educaim32 = educaim32)

# Select only ID and derived variables
output <- data_full %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written successfully to data/output/cleaned_data.csv\n')
cat('Dimensions:', nrow(output), 'rows x', ncol(output), 'columns\n')