library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load data
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = 'character'))
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = 'character'))
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = 'character'))
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = 'character'))
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = 'character'))
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = 'character'))
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = 'character'))

# Merge datasets
data <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Mapping functions for missing values
harmonise_missing <- function(val, wave_map) {
  if (is.na(val)) return(-3)
  if (val %in% names(wave_map)) return(wave_map[[as.character(val)]])
  return(val)
}

# --------------------------------------------------------------------------
# Wave 4 (Age 17) - ecoact17
# --------------------------------------------------------------------------
# 1 = Paid work (1,2), 2 = App/Train (4), 3 = Educ (5), 4 = Unemp (3), 5 = Home/Fam (6), 6 = Other (7,8,9)
# Missing: -999 (lost -> -2), -94 (insufficient -> -8), -92 (refused -> -9), -91 (N/A school -> -1)
map_w4_cat <- c('1'='1', '2'='1', '3'='4', '4'='2', '5'='3', '6'='5', '7'='6', '8'='6', '9'='6')
map_w4_miss <- c('-999'='-2', '-94'='-8', '-92'='-9', '-91'='-1')

data <- data %>%
  mutate(ecoact17 = case_when(
    W4empsYP %in% c(1, 2) ~ 1,
    W4empsYP == 4 ~ 2,
    W4empsYP == 5 ~ 3,
    W4empsYP == 3 ~ 4,
    W4empsYP == 6 ~ 5,
    W4empsYP %in% c(7, 8, 9) ~ 6,
    W4empsYP == -999 ~ -2,
    W4empsYP == -94 ~ -8,
    W4empsYP == -92 ~ -9,
    W4empsYP == -91 ~ -1,
    is.na(W4empsYP) ~ -3,
    TRUE ~ -3
  ))

# --------------------------------------------------------------------------
# Wave 5 (Age 18) - ecoact18
# --------------------------------------------------------------------------
# 1 = Paid work (3), 2 = App/Train (1,2,5,6), 3 = Educ (4), 4 = Unemp (7), 5 = Home/Fam (8), 6 = Other (9,10,11)
# Missing: -94 (insufficient -> -8)
map_w5_cat <- c('3'='1', '1'='2', '2'='2', '5'='2', '6'='2', '4'='3', '7'='4', '8'='5', '9'='6', '10'='6', '11'='6')
map_w5_miss <- c('-94'='-8')

data <- data %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 3 ~ 1,
    W5mainactYP %in% c(1, 2, 5, 6) ~ 2,
    W5mainactYP == 4 ~ 3,
    W5mainactYP == 7 ~ 4,
    W5mainactYP == 8 ~ 5,
    W5mainactYP %in% c(9, 10, 11) ~ 6,
    W5mainactYP == -94 ~ -8,
    is.na(W5mainactYP) ~ -3,
    TRUE ~ -3
  ))

# --------------------------------------------------------------------------
# Wave 6 (Age 19) - ecoact19
# --------------------------------------------------------------------------
# 1 = Paid work (3), 2 = App/Train (4,5,10), 3 = Educ (1,2), 4 = Unemp (8), 5 = Home/Fam (7), 6 = Other (6,9,11)
# Missing: -91 (unable to classify -> -8)
map_w6_cat <- c('3'='1', '4'='2', '5'='2', '10'='2', '1'='3', '2'='3', '8'='4', '7'='5', '6'='6', '9'='6', '11'='6')
map_w6_miss <- c('-91'='-8')

data <- data %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 3 ~ 1,
    W6TCurrentAct %in% c(4, 5, 10) ~ 2,
    W6TCurrentAct %in% c(1, 2) ~ 3,
    W6TCurrentAct == 8 ~ 4,
    W6TCurrentAct == 7 ~ 5,
    W6TCurrentAct %in% c(6, 9, 11) ~ 6,
    W6TCurrentAct == -91 ~ -8,
    is.na(W6TCurrentAct) ~ -3,
    TRUE ~ -3
  ))

# --------------------------------------------------------------------------
# Wave 7 (Age 20) - ecoact20
# --------------------------------------------------------------------------
# 1 = Paid work (3), 2 = App/Train (4,5,11), 3 = Educ (1,2,9), 4 = Unemp (8), 5 = Home/Fam (7), 6 = Other (6,10,12,13,14,15)
# Missing: -91 (N/A -> -1)
map_w7_cat <- c('3'='1', '4'='2', '5'='2', '11'='2', '1'='3', '2'='3', '9'='3', '8'='4', '7'='5', '6'='6', '10'='6', '12'='6', '13'='6', '14'='6', '15'='6')
map_w7_miss <- c('-91'='-1')

data <- data %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 3 ~ 1,
    W7TCurrentAct %in% c(4, 5, 11) ~ 2,
    W7TCurrentAct %in% c(1, 2, 9) ~ 3,
    W7TCurrentAct == 8 ~ 4,
    W7TCurrentAct == 7 ~ 5,
    W7TCurrentAct %in% c(6, 10, 12, 13, 14, 15) ~ 6,
    W7TCurrentAct == -91 ~ -1,
    is.na(W7TCurrentAct) ~ -3,
    TRUE ~ -3
  ))

# --------------------------------------------------------------------------
# Wave 8 (Age 25) - ecoact25, ecoactadu25
# --------------------------------------------------------------------------
# Collapsed:
# 1 = Paid work (1,2), 2 = App/Train (6,7), 3 = Educ (5), 4 = Unemp (4), 5 = Home/Fam (9), 6 = Other (3,8,10)
# Missing: -9(Ref/-9), -8(Insuf/-8), -1(N/A/-1)

data <- data %>%
  mutate(
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(1, 2) ~ 1,
      W8DACTIVITYC %in% c(6, 7) ~ 2,
      W8DACTIVITYC == 5 ~ 3,
      W8DACTIVITYC == 4 ~ 4,
      W8DACTIVITYC == 9 ~ 5,
      W8DACTIVITYC %in% c(3, 8, 10) ~ 6,
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      is.na(W8DACTIVITYC) ~ -3,
      TRUE ~ -3
    ),
    ecoactadu25 = case_when(
      W8DACTIVITYC >= 1 & W8DACTIVITYC <= 10 ~ W8DACTIVITYC,
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      is.na(W8DACTIVITYC) ~ -3,
      TRUE ~ -3
    )
  )

# --------------------------------------------------------------------------
# Wave 9 (Age 32) - ecoact32, ecoactadu32
# --------------------------------------------------------------------------
# Collapsed:
# 1 = Paid work (1,2), 2 = App/Train (6,7), 3 = Educ (5), 4 = Unemp (4), 5 = Home/Fam (9), 6 = Other (3,8,10)
# Missing: -9(Ref/-9), -8(Insuf/-8), -1(N/A/-1)

data <- data %>%
  mutate(
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(1, 2) ~ 1,
      W9DACTIVITYC %in% c(6, 7) ~ 2,
      W9DACTIVITYC == 5 ~ 3,
      W9DACTIVITYC == 4 ~ 4,
      W9DACTIVITYC == 9 ~ 5,
      W9DACTIVITYC %in% c(3, 8, 10) ~ 6,
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      is.na(W9DACTIVITYC) ~ -3,
      TRUE ~ -3
    ),
    ecoactadu32 = case_when(
      W9DACTIVITYC >= 1 & W9DACTIVITYC <= 10 ~ W9DACTIVITYC,
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      is.na(W9DACTIVITYC) ~ -3,
      TRUE ~ -3
    )
  )

# Final Variable selection
final_vars <- c('NSID', 'ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32', 'ecoactadu25', 'ecoactadu32')
output_data <- data %>%
  select(all_of(final_vars))

# Apply Factor Labels for Collapsed
collapsed_labels <- c(
  '1' = 'In paid work',
  '2' = 'Apprenticeship / government training scheme / training',
  '3' = 'Education',
  '4' = 'Unemployed',
  '5' = 'Looking after home / family',
  '6' = 'Other (including voluntary work, sick/disabled, waiting for course, travelling, and other residual categories)',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Apply labels to collapsed variables
for (var in c('ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32')) {
  output_data[[var]] <- factor(output_data[[var]], levels = as.numeric(names(collapsed_labels)), labels = collapsed_labels)
}

# Apply labels to detailed variables (1-10 as per metadata labels)
# Wave 8 and 9 detailed categories are the same
detailed_labels <- c(
  '1' = 'Employee - in paid work',
  '2' = 'Self employed',
  '3' = 'In unpaid/voluntary work',
  '4' = 'Unemployed',
  '5' = 'Education: School/college/university',
  '6' = 'Apprenticeship',
  '7' = "On gov't scheme for employment training",
  '8' = 'Sick or disabled',
  '9' = 'Looking after home or family',
  '10' = 'Something else',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

for (var in c('ecoactadu25', 'ecoactadu32')) {
  output_data[[var]] <- factor(output_data[[var]], levels = as.numeric(names(detailed_labels)), labels = detailed_labels)
}

# Write output
write_csv(output_data, 'data/output/cleaned_data.csv')
