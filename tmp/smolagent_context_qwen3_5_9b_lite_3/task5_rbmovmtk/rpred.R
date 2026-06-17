library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
data_wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
data_wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
data_wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
data_wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
data_wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Combine all data using full_join by NSID
combined <- full_join(data_wave1, data_wave4, by = 'NSID')
combined <- full_join(combined, data_wave6, by = 'NSID')
combined <- full_join(combined, data_wave8, by = 'NSID')
combined <- full_join(combined, data_wave9, by = 'NSID')

# Define missing value codes
# For W6MarStatYP (wave 6, age 19):
# -997 = -2 (script error)
# -97 = -2 (respondent declined self completion)
# -92 = -9 (refused)
# -91 = -1 (not applicable)
# -1 = -8 (don't know)
# 1 = Single
# 2 = Married
# 3 = Separated
# 4 = Divorced
# 5 = Widowed

# For W8DMARSTAT (wave 8, age 25):
# -9 = -9 (refused)
# -8 = -8 (insufficient information)
# -1 = -1 (not applicable)
# 1 = Single and never married or in a CP
# 2 = Married
# 3 = Separated but still legally married
# 4 = Divorced
# 5 = Widowed
# 6 = A Civil Partner
# 7 = Separated but still legally in a CP
# 8 = A former Civil Partner
# 9 = A surviving Civil Partner

# For W9DMARSTAT (wave 9, age 32):
# -9 = -9 (refused)
# -8 = -8 (insufficient information)
# 1 = Single that is never married or never in a CP
# 2 = Married
# 3 = Divorced
# 4 = Legally separated
# 5 = Widowed
# 6 = A Civil Partner in a legally recognised Civil Partnership
# 7 = A former Civil Partner (where Civil Partnership legally dissolved)
# 8 = A surviving Civil Partner (where Civil Partner has died)

# Transform W6MarStatYP (age 19)
combined <- combined %>%
  mutate(
    partnr19_raw = W6MarStatYP,
    partnr19 = case_when(
      is.na(W6MarStatYP) | W6MarStatYP %in% c(-997, -97, -92, -91, -1) ~ -9,
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      TRUE ~ NA_real_
    )
  )

# Transform W8DMARSTAT (age 25)
combined <- combined %>%
  mutate(
    partnradu25_raw = W8DMARSTAT,
    partnradu25 = case_when(
      is.na(W8DMARSTAT) | W8DMARSTAT %in% c(-9, -8, -1) ~ -9,
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 6 ~ 6,
      W8DMARSTAT == 7 ~ 7,
      W8DMARSTAT == 8 ~ 8,
      W8DMARSTAT == 9 ~ 9,
      TRUE ~ NA_real_
    )
  )

# For partnr25, we need to collapse partnradu25 categories
# Map adult categories to basic categories
combined <- combined %>%
  mutate(
    partnr25 = case_when(
      partnradu25 == 1 ~ 1,  # Single and never married or in a CP -> Single
      partnradu25 == 2 ~ 2,  # Married -> Married
      partnradu25 == 3 ~ 3,  # Separated but still legally married -> Separated
      partnradu25 == 4 ~ 4,  # Divorced -> Divorced
      partnradu25 == 5 ~ 5,  # Widowed -> Widowed
      partnradu25 == 6 ~ 6,  # A Civil Partner -> (new category, keep separate)
      partnradu25 == 7 ~ 7,  # Separated but still legally in a CP -> (new category)
      partnradu25 == 8 ~ 8,  # A former Civil Partner -> (new category)
      partnradu25 == 9 ~ 9,  # A surviving Civil Partner -> (new category)
      is.na(partnradu25) | partnradu25 %in% c(-9, -8, -1) ~ -9,
      TRUE ~ NA_real_
    )
  )

# Transform W9DMARSTAT (age 32)
combined <- combined %>%
  mutate(
    partnradu32_raw = W9DMARSTAT,
    partnradu32 = case_when(
      is.na(W9DMARSTAT) | W9DMARSTAT %in% c(-9, -8) ~ -9,
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 3,
      W9DMARSTAT == 4 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 6 ~ 6,
      W9DMARSTAT == 7 ~ 7,
      W9DMARSTAT == 8 ~ 8,
      TRUE ~ NA_real_
    )
  )

# For partnr32, collapse partnradu32 categories
# Need to decide on mapping strategy
# Looking at the categories, we should maintain comparability with partnr25
# Let's map civil partnership categories to similar categories
combined <- combined %>%
  mutate(
    partnr32 = case_when(
      partnradu32 == 1 ~ 1,  # Single -> Single
      partnradu32 == 2 ~ 2,  # Married -> Married
      partnradu32 == 3 ~ 4,  # Divorced -> Divorced
      partnradu32 == 4 ~ 3,  # Legally separated -> Separated
      partnradu32 == 5 ~ 5,  # Widowed -> Widowed
      partnradu32 == 6 ~ 6,  # Civil Partner -> Civil Partner
      partnradu32 == 7 ~ 8,  # Former Civil Partner -> Former Civil Partner
      partnradu32 == 8 ~ 9,  # Surviving Civil Partner -> Surviving Civil Partner
      is.na(partnradu32) | partnradu32 %in% c(-9, -8) ~ -9,
      TRUE ~ NA_real_
    )
  )

# Remove raw variables
combined <- combined %>%
  select(-ends_with('_raw'))

# Write output
write_csv(combined, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
