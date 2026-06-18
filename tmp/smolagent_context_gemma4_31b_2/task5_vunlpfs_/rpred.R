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
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'c'))
})

# Convert to list of names for easier access
names(data_list) <- files

# Merge datasets
full_frame <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]] %>% select(NSID), by = 'NSID') %>% 
  full_join(data_list[[3]] %>% select(NSID, W6MarStatYP), by = 'NSID') %>% 
  full_join(data_list[[4]] %>% select(NSID, W8DMARSTAT), by = 'NSID') %>% 
  full_join(data_list[[5]] %>% select(NSID, W9DMARSTAT), by = 'NSID')

# Convert source variables to numeric for processing
full_frame <- full_frame %>%
  mutate(
    W6MarStatYP = as.numeric(W6MarStatYP),
    W8DMARSTAT = as.numeric(W8DMARSTAT),
    W9DMARSTAT = as.numeric(W9DMARSTAT)
  )

# Harmonisation logic
# Standard Missing Codes:
# -9: Refusal, -8: Don't know/insufficient, -7: Prefer not to say, -3: Not asked/NA, -2: Script error/lost, -1: Not applicable

# Process W6MarStatYP -> partnr19
# -997: Script error -> -2
# -97: Declined -> -7
# -92: Refused -> -9
# -91: Not applicable -> -1
# -1: Don't know -> -8
full_frame <- full_frame %>%
  mutate(partnr19 = case_when(
    W6MarStatYP == -997 ~ -2,
    W6MarStatYP == -97  ~ -7,
    W6MarStatYP == -92  ~ -9,
    W6MarStatYP == -91  ~ -1,
    W6MarStatYP == -1   ~ -8,
    W6MarStatYP >= 1    ~ W6MarStatYP,
    TRUE               ~ -3
  ))

# Process W8DMARSTAT -> partnradu25 and partnr25
# -9: Refused -> -9
# -8: Insufficient -> -8
# -1: Not applicable -> -1
full_frame <- full_frame %>%
  mutate(partnradu25 = case_when(
    W8DMARSTAT == -9  ~ -9,
    W8DMARSTAT == -8  ~ -8,
    W8DMARSTAT == -1  ~ -1,
    W8DMARSTAT >= 1   ~ W8DMARSTAT,
    TRUE              ~ -3
  ))

# Collapse partnradu25 to partnr25 (harmonised categories)
# W6 uses: 1:Single, 2:Married, 3:Separated, 4:Divorced, 5:Widowed
# W8 detailed: 1:Single/CP, 2:Married, 3:Sep Married, 4:Divorced, 5:Widowed, 6:CP, 7:Sep CP, 8:Former CP, 9:Surv CP
# Harmonised mapping:
# Single/CP (1, 6) -> 1
# Married (2) -> 2
# Separated (3, 7) -> 3
# Divorced/Former CP (4, 8) -> 4
# Widowed/Surv CP (5, 9) -> 5
full_frame <- full_frame %>%
  mutate(partnr25 = case_when(
    partnradu25 == 1 | partnradu25 == 6 ~ 1,
    partnradu25 == 2 ~ 2,
    partnradu25 == 3 | partnradu25 == 7 ~ 3,
    partnradu25 == 4 | partnradu25 == 8 ~ 4,
    partnradu25 == 5 | partnradu25 == 9 ~ 5,
    partnradu25 < 0 ~ partnradu25,
    TRUE ~ -3
  ))

# Process W9DMARSTAT -> partnradu32 and partnr32
# -9: Refused -> -9
# -8: Insufficient -> -8
full_frame <- full_frame %>%
  mutate(partnradu32 = case_when(
    W9DMARSTAT == -9  ~ -9,
    W9DMARSTAT == -8  ~ -8,
    W9DMARSTAT >= 1   ~ W9DMARSTAT,
    TRUE              ~ -3
  ))

# Collapse partnradu32 to partnr32
# W9 detailed: 1:Single, 2:Married, 3:Divorced, 4:Sep, 5:Widowed, 6:CP, 7:Former CP, 8:Surv CP
# Mapping:
# Single/CP (1, 6) -> 1
# Married (2) -> 2
# Separated (4) -> 3
# Divorced/Former CP (3, 7) -> 4
# Widowed/Surv CP (5, 8) -> 5
full_frame <- full_frame %>%
  mutate(partnr32 = case_when(
    partnradu32 == 1 | partnradu32 == 6 ~ 1,
    partnradu32 == 2 ~ 2,
    partnradu32 == 4 ~ 3,
    partnradu32 == 3 | partnradu32 == 7 ~ 4,
    partnradu32 == 5 | partnradu32 == 8 ~ 5,
    partnradu32 < 0 ~ partnradu32,
    TRUE ~ -3
  ))

# Create Factor Labels
common_labels <- c(
  '1' = 'Single',
  '2' = 'Married',
  '3' = 'Separated',
  '4' = 'Divorced',
  '5' = 'Widowed',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Script error',
  '-1' = 'Not applicable'
)

full_frame <- full_frame %>%
  mutate(
    partnr19 = factor(partnr19, levels = names(common_labels), labels = common_labels),
    partnr25 = factor(partnr25, levels = names(common_labels), labels = common_labels),
    partnr32 = factor(partnr32, levels = names(common_labels), labels = common_labels)
  )

# Detailed labels for adult variables
labels_w8 <- c(
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
  '-8' = 'Insufficient information',
  '-1' = 'Not applicable',
  '-3' = 'Not asked'
)

labels_w9 <- c(
  '1' = 'Single that is never married or never in a Civil Partnership',
  '2' = 'Married',
  '3' = 'Divorced',
  '4' = 'Legally separated',
  '5' = 'Widowed',
  '6' = 'A Civil Partner in a legally recognised Civil Partnership',
  '7' = 'A former Civil Partner (where Civil Partnership legally dissolved)',
  '8' = 'A surviving Civil Partner (where Civil Partner has died)',
  '-9' = 'Refusal',
  '-8' = 'Insufficient information',
  '-3' = 'Not asked'
)

full_frame <- full_frame %>%
  mutate(
    partnradu25 = factor(partnradu25, levels = names(labels_w8), labels = labels_w8),
    partnradu32 = factor(partnradu32, levels = names(labels_w9), labels = labels_w9)
  )

# Final selection
final_data <- full_frame %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

write_csv(final_data, 'data/output/cleaned_data.csv')
