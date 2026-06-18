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
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols())
})

names(data_list) <- files

# Merge datasets
full_df <- data_list[[1]] %>% 
  full_join(data_list[[2]], by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID')

# Harmonisation logic for partnership status
# Target: partnr19, partnr25, partnr32, partnradu25, partnradu32

# W6 (Age 19): W6MarStatYP
# Labels: 1: Single, 2: Married, 3: Separated, 4: Divorced, 5: Widowed
# Missing: -997: Script error (-2), -97: Refused (-9), -92: Refused (-9), -91: N/A (-1), -1: DK (-8)

full_df <- full_df %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      W6MarStatYP == -997 ~ -2,
      W6MarStatYP == -97 ~ -9,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      TRUE ~ -3
    )
  )

# W8 (Age 25): W8DMARSTAT
# Labels: 1: Single/CP, 2: Married, 3: Sep married, 4: Divorced, 5: Widowed, 6: CP, 7: Sep CP, 8: Former CP, 9: Surviving CP
# Missing: -9: Refused, -8: Insufficient (-8), -1: N/A

# partnr25 (Harmonised): 
# 1: Single/CP, 2: Married/CP, 3: Separated, 4: Divorced/Former CP, 5: Widowed/Surviving CP

full_df <- full_df %>%
  mutate(
    partnr25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT %in% c(2, 6) ~ 2,
      W8DMARSTAT %in% c(3, 7) ~ 3,
      W8DMARSTAT %in% c(4, 8) ~ 4,
      W8DMARSTAT %in% c(5, 9) ~ 5,
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    ),
    partnradu25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 6 ~ 6,
      W8DMARSTAT == 7 ~ 7,
      W8DMARSTAT == 8 ~ 8,
      W8DMARSTAT == 9 ~ 9,
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    )
  )

# W9 (Age 32): W9DMARSTAT
# Labels: 1: Single/CP, 2: Married, 3: Divorced, 4: Legally separated, 5: Widowed, 6: CP, 7: Former CP, 8: Surviving CP
# Missing: -9: Refused, -8: Insufficient

full_df <- full_df %>%
  mutate(
    partnr32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT %in% c(2, 6) ~ 2,
      W9DMARSTAT == 4 ~ 3,
      W9DMARSTAT %in% c(3, 7) ~ 4,
      W9DMARSTAT %in% c(5, 8) ~ 5,
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      TRUE ~ -3
    ),
    partnradu32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 3,
      W9DMARSTAT == 4 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 6 ~ 6,
      W9DMARSTAT == 7 ~ 7,
      W9DMARSTAT == 8 ~ 8,
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      TRUE ~ -3
    )
  )

# Factor labels
val_labels_partnr <- c(
  '1' = 'Single',
  '2' = 'Married/Civil Partner',
  '3' = 'Separated',
  '4' = 'Divorced/Former CP',
  '5' = 'Widowed/Surviving CP',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know/Insufficient',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Schedule not applicable',
  '-1' = 'Not applicable'
)

# Assign labels to harmonised variables
full_df$partnr19 <- factor(full_df$partnr19, levels = as.numeric(names(val_labels_partnr)), labels = val_labels_partnr)
full_df$partnr25 <- factor(full_df$partnr25, levels = as.numeric(names(val_labels_partnr)), labels = val_labels_partnr)
full_df$partnr32 <- factor(full_df$partnr32, levels = as.numeric(names(val_labels_partnr)), labels = val_labels_partnr)

# Detailed labels (W8)
val_labels_radu25 <- c(
  '1' = 'Single and never married or in a CP',
  '2' = 'Married',
  '3' = 'Separated but still legally married',
  '4' = 'Divorced',
  '5' = 'Widowed',
  '6' = 'A Civil Partner',
  '7' = 'Separated but still legally in a CP',
  '8' = 'A former Civil Partner',
  '9' = 'A surviving Civil Partner',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know/Insufficient',
  '-1' = 'Not applicable',
  '-3' = 'Not asked'
)
full_df$partnradu25 <- factor(full_df$partnradu25, levels = as.numeric(names(val_labels_radu25)), labels = val_labels_radu25)

# Detailed labels (W9)
val_labels_radu32 <- c(
  '1' = 'Single that is never married or never in a Civil Partnership',
  '2' = 'Married',
  '3' = 'Divorced',
  '4' = 'Legally separated',
  '5' = 'Widowed',
  '6' = 'A Civil Partner in a legally recognised Civil Partnership',
  '7' = 'A former Civil Partner (where Civil Partnership legally dissolved)',
  '8' = 'A surviving Civil Partner (where Civil Partner has died)',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know/Insufficient',
  '-3' = 'Not asked'
)
full_df$partnradu32 <- factor(full_df$partnradu32, levels = as.numeric(names(val_labels_radu32)), labels = val_labels_radu32)

# Final selection
final_df <- full_df %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

write_csv(final_df, 'data/output/cleaned_data.csv')
