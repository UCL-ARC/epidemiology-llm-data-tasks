library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

load_data <- function(file) {
  readr::read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols(.default = 'double'))
}

# Since NSID is string, we handle it specifically in a loop or map
data_list <- list()
for (f in files) {
  df <- readr::read_delim(paste0('data/input/', f), delim = '\t')
  # Ensure NSID is character
  df <- df %>% mutate(NSID = as.character(NSID))
  data_list[[f]] <- df
}

# Merge datasets
full_frame <- data_list[[1]] %>% 
  full_join(data_list[[2]], by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID') %>% 
  full_join(data_list[[6]], by = 'NSID') %>% 
  full_join(data_list[[7]], by = 'NSID')

# Harmonisation function for missing values
harmonise_missing <- function(val, labels) {
  # labels is the value_labels list from metadata
  # We need to map the raw codes to standard missing codes
  # -9 = Refusal, -8 = Don't know/insufficient, -7 = Prefer not to say, 
  # -3 = Not asked, -2 = Schedule not applicable/lost, -1 = Item not applicable
  
  res <- case_when(
    is.na(val) ~ -3,
    # Mapping based on labels provided in metadata
    # Note: This is a simplified approach, in real script we'd check every label
    # For Wave 4: -999 (lost) -> -2, -94 (insufficient) -> -8, -92 (refused) -> -9, -91 (not applicable) -> -1
    # For Wave 5: -94 (insufficient) -> -8
    # For Wave 6: -91 (unable to classify) -> -2 or -1 (usually -1 if not applicable)
    # For Wave 7: -91 (not applicable) -> -1
    # For Wave 8/9: -9 (refused) -> -9, -8 (insufficient) -> -8, -1 (not applicable) -> -1
    TRUE ~ val
  )
  return(res)
}

# Specific logic for each wave's economic activity

# Wave 4 (Age 17)
# 1=Paid work, 2=Apprenticeship/Training, 3=Education, 4=Unemployed, 5=Home/Family, 6=Other
# Raw: 1,2 -> Paid (1); 4 -> Training (2); 5 -> Education (3); 3 -> Unemployed (4); 6 -> Home (5); 7,8,9 -> Other (6)
# Missing: -999 -> -2, -94 -> -8, -92 -> -9, -91 -> -1
full_frame <- full_frame %>% mutate(
  ecoact17 = case_when(
    W4empsYP == 1 | W4empsYP == 2 ~ 1,
    W4empsYP == 4 ~ 2,
    W4empsYP == 5 ~ 3,
    W4empsYP == 3 ~ 4,
    W4empsYP == 6 ~ 5,
    W4empsYP == 7 | W4empsYP == 8 | W4empsYP == 9 ~ 6,
    W4empsYP == -999 ~ -2,
    W4empsYP == -94 ~ -8,
    W4empsYP == -92 ~ -9,
    W4empsYP == -91 ~ -1,
    is.na(W4empsYP) ~ -3,
    TRUE ~ -3
  )
)

# Wave 5 (Age 18)
# Raw: 1,2,5,6 -> Training/Apprenticeship (2); 3 -> Paid (1); 4 -> Education (3); 7 -> Unemployed (4); 8 -> Home (5); 9,10,11 -> Other (6)
# Missing: -94 -> -8
full_frame <- full_frame %>% mutate(
  ecoact18 = case_when(
    W5mainactYP == 3 ~ 1,
    W5mainactYP == 1 | W5mainactYP == 2 | W5mainactYP == 5 | W5mainactYP == 6 ~ 2,
    W5mainactYP == 4 ~ 3,
    W5mainactYP == 7 ~ 4,
    W5mainactYP == 8 ~ 5,
    W5mainactYP == 9 | W5mainactYP == 10 | W5mainactYP == 11 ~ 6,
    W5mainactYP == -94 ~ -8,
    is.na(W5mainactYP) ~ -3,
    TRUE ~ -3
  )
)

# Wave 6 (Age 19)
# Raw: 3 -> Paid (1); 4,5 -> Training/Apprenticeship (2); 1,2 -> Education (3); 8 -> Unemployed (4); 7 -> Home (5); 6,9,10,11 -> Other (6)
# Missing: -91 -> -1
full_frame <- full_frame %>% mutate(
  ecoact19 = case_when(
    W6TCurrentAct == 3 ~ 1,
    W6TCurrentAct == 4 | W6TCurrentAct == 5 ~ 2,
    W6TCurrentAct == 1 | W6TCurrentAct == 2 ~ 3,
    W6TCurrentAct == 8 ~ 4,
    W6TCurrentAct == 7 ~ 5,
    W6TCurrentAct == 6 | W6TCurrentAct == 9 | W6TCurrentAct == 10 | W6TCurrentAct == 11 ~ 6,
    W6TCurrentAct == -91 ~ -1,
    is.na(W6TCurrentAct) ~ -3,
    TRUE ~ -3
  )
)

# Wave 7 (Age 20)
# Raw: 3 -> Paid (1); 4,5,11 -> Training (2); 1,2,9 -> Education (3); 8 -> Unemployed (4); 7 -> Home (5); 6,10,12,13,14,15 -> Other (6)
# Missing: -91 -> -1
full_frame <- full_frame %>% mutate(
  ecoact20 = case_when(
    W7TCurrentAct == 3 ~ 1,
    W7TCurrentAct == 4 | W7TCurrentAct == 5 | W7TCurrentAct == 11 ~ 2,
    W7TCurrentAct == 1 | W7TCurrentAct == 2 | W7TCurrentAct == 9 ~ 3,
    W7TCurrentAct == 8 ~ 4,
    W7TCurrentAct == 7 ~ 5,
    W7TCurrentAct == 6 | W7TCurrentAct == 10 | W7TCurrentAct == 12 | W7TCurrentAct == 13 | W7TCurrentAct == 14 | W7TCurrentAct == 15 ~ 6,
    W7TCurrentAct == -91 ~ -1,
    is.na(W7TCurrentAct) ~ -3,
    TRUE ~ -3
  )
)

# Wave 8 (Age 25)
# Raw: 1,2 -> Paid (1); 6,7 -> Training (2); 5 -> Education (3); 4 -> Unemployed (4); 9 -> Home (5); 3,8,10 -> Other (6)
# Missing: -9 -> -9, -8 -> -8, -1 -> -1
full_frame <- full_frame %>% mutate(
  ecoact25 = case_when(
    W8DACTIVITYC == 1 | W8DACTIVITYC == 2 ~ 1,
    W8DACTIVITYC == 6 | W8DACTIVITYC == 7 ~ 2,
    W8DACTIVITYC == 5 ~ 3,
    W8DACTIVITYC == 4 ~ 4,
    W8DACTIVITYC == 9 ~ 5,
    W8DACTIVITYC == 3 | W8DACTIVITYC == 8 | W8DACTIVITYC == 10 ~ 6,
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    is.na(W8DACTIVITYC) ~ -3,
    TRUE ~ -3
  ),
  ecoactadu25 = case_when(
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    is.na(W8DACTIVITYC) ~ -3,
    TRUE ~ W8DACTIVITYC
  )
)

# Wave 9 (Age 32)
# Raw: 1,2 -> Paid (1); 6,7 -> Training (2); 5 -> Education (3); 4 -> Unemployed (4); 9 -> Home (5); 3,8,10 -> Other (6)
# Missing: -9 -> -9, -8 -> -8, -1 -> -1
full_frame <- full_frame %>% mutate(
  ecoact32 = case_when(
    W9DACTIVITYC == 1 | W9DACTIVITYC == 2 ~ 1,
    W9DACTIVITYC == 6 | W9DACTIVITYC == 7 ~ 2,
    W9DACTIVITYC == 5 ~ 3,
    W9DACTIVITYC == 4 ~ 4,
    W9DACTIVITYC == 9 ~ 5,
    W9DACTIVITYC == 3 | W9DACTIVITYC == 8 | W9DACTIVITYC == 10 ~ 6,
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    is.na(W9DACTIVITYC) ~ -3,
    TRUE ~ -3
  ),
  ecoactadu32 = case_when(
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    is.na(W9DACTIVITYC) ~ -3,
    TRUE ~ W9DACTIVITYC
  )
)

# Final selection
final_data <- full_frame %>% 
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Create factors for collapsed variables
collapsed_vars <- c('ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32')
collapsed_labels <- c(
  '1' = 'In paid work',
  '2' = 'Apprenticeship / government training scheme / training',
  '3' = 'Education',
  '4' = 'Unemployed',
  '5' = 'Looking after home / family',
  '6' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

for (v in collapsed_vars) {
  final_data[[v]] <- factor(final_data[[v]], levels = as.numeric(names(collapsed_labels)), labels = collapsed_labels)
}

# For detailed variables, labels from metadata
# W8/W9 labels: 1: Employee, 2: Self employed, 3: Unpaid, 4: Unemployed, 5: Education, 6: Apprent, 7: Gov scheme, 8: Sick, 9: Home, 10: Other
adu_labels <- c(
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
  '-8' = 'Insufficient information',
  '-1' = 'Not applicable',
  '-3' = 'Not asked at the fieldwork stage / not interviewed'
)

final_data$ecoactadu25 <- factor(final_data$ecoactadu25, levels = as.numeric(names(adu_labels)), labels = adu_labels)
final_data$ecoactadu32 <- factor(final_data$ecoactadu32, levels = as.numeric(names(adu_labels)), labels = adu_labels)

readr::write_csv(final_data, 'data/output/cleaned_data.csv')
