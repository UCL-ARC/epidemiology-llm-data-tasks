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
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

data_list <- map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character())))
names(data_list) <- files

# Merge datasets
full_df <- data_list[[1]] %>% 
  full_join(data_list[[2]], by = 'NSID') %>%
  full_join(data_list[[3]], by = 'NSID') %>%
  full_join(data_list[[4]], by = 'NSID') %>%
  full_join(data_list[[5]], by = 'NSID') %>%
  full_join(data_list[[6]], by = 'NSID') %>%
  full_join(data_list[[7]], by = 'NSID')

# Harmonisation logic for ecoact (6-category)
# Categories: 1. Paid work, 2. Unemployed, 3. Education/Training, 4. Home/Family, 5. Sick/Disabled, 6. Other

# Age 17 (W4empsYP)
full_df <- full_df %>%
  mutate(ecoact17 = case_when(
    W4empsYP == 1 | W4empsYP == 2 ~ 1, # Paid work
    W4empsYP == 3 ~ 2,               # Unemployed
    W4empsYP == 4 | W4empsYP == 5 ~ 3, # Education/Training
    W4empsYP == 6 ~ 4,               # Home/Family
    W4empsYP == 8 ~ 5,               # Sick/Disabled
    W4empsYP == 7 | W4empsYP == 9 ~ 6, # Other (Retired/Other)
    W4empsYP == -92 ~ -9,            # Refusal
    W4empsYP == -94 ~ -8,            # Insufficient
    W4empsYP == -91 ~ -1,            # Not applicable
    W4empsYP == -999 ~ -2,           # Lost
    TRUE ~ -3
  ))

# Age 18 (W5mainactYP)
full_df <- full_df %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == 3 ~ 1,            # Paid work
    W5mainactYP == 7 ~ 2,            # Unemployed
    W5mainactYP == 1 | W5mainactYP == 2 | W5mainactYP == 4 | W5mainactYP == 5 | W5mainactYP == 6 ~ 3, # Education/Training
    W5mainactYP == 8 ~ 4,            # Home/Family
    W5mainactYP == 9 | W5mainactYP == 10 | W5mainactYP == 11 ~ 6, # Other
    W5mainactYP == -94 ~ -8,         # Insufficient
    TRUE ~ -3
  ))

# Age 19 (W6TCurrentAct)
full_df <- full_df %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 3 ~ 1,          # Paid work
    W6TCurrentAct == 8 ~ 2,          # Unemployed
    W6TCurrentAct == 1 | W6TCurrentAct == 2 | W6TCurrentAct == 4 | W6TCurrentAct == 5 ~ 3, # Education/Training
    W6TCurrentAct == 7 ~ 4,          # Home/Family
    W6TCurrentAct == 6 | W6TCurrentAct == 9 | W6TCurrentAct == 10 | W6TCurrentAct == 11 ~ 6, # Other
    W6TCurrentAct == -91 ~ -8,       # Unable to classify -> Insufficient
    TRUE ~ -3
  ))

# Age 20 (W7TCurrentAct)
full_df <- full_df %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 3 ~ 1,          # Paid work
    W7TCurrentAct == 8 ~ 2,          # Unemployed
    W7TCurrentAct == 1 | W7TCurrentAct == 2 | W7TCurrentAct == 4 | W7TCurrentAct == 5 | W7TCurrentAct == 9 | W7TCurrentAct == 11 ~ 3, # Education/Training
    W7TCurrentAct == 7 ~ 4,          # Home/Family
    W7TCurrentAct == 14 ~ 5,         # Sick/Disabled
    W7TCurrentAct == 6 | W7TCurrentAct == 10 | W7TCurrentAct == 12 | W7TCurrentAct == 13 | W7TCurrentAct == 15 ~ 6, # Other
    W7TCurrentAct == -91 ~ -1,       # Not applicable
    TRUE ~ -3
  ))

# Age 25 (W8DACTIVITYC)
full_df <- full_df %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1 | W8DACTIVITYC == 2 ~ 1, # Paid work
    W8DACTIVITYC == 4 ~ 2,                     # Unemployed
    W8DACTIVITYC == 5 | W8DACTIVITYC == 6 | W8DACTIVITYC == 7 ~ 3, # Education/Training
    W8DACTIVITYC == 9 ~ 4,                     # Home/Family
    W8DACTIVITYC == 8 ~ 5,                     # Sick/Disabled
    W8DACTIVITYC == 3 | W8DACTIVITYC == 10 ~ 6, # Other
    W8DACTIVITYC == -9 ~ -9,                    # Refusal
    W8DACTIVITYC == -8 ~ -8,                    # Insufficient
    W8DACTIVITYC == -1 ~ -1,                    # Not applicable
    TRUE ~ -3
  ),
  ecoactadu25 = W8DACTIVITYC) # Detailed

# Age 32 (W9DACTIVITYC)
full_df <- full_df %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1 | W9DACTIVITYC == 2 ~ 1, # Paid work
    W9DACTIVITYC == 4 ~ 2,                     # Unemployed
    W9DACTIVITYC == 5 | W9DACTIVITYC == 6 | W9DACTIVITYC == 7 ~ 3, # Education/Training
    W9DACTIVITYC == 9 ~ 4,                     # Home/Family
    W9DACTIVITYC == 8 ~ 5,                     # Sick/Disabled
    W9DACTIVITYC == 3 | W9DACTIVITYC == 10 ~ 6, # Other
    W9DACTIVITYC == -9 ~ -9,                    # Refusal
    W9DACTIVITYC == -8 ~ -8,                    # Insufficient
    W9DACTIVITYC == -1 ~ -1,                    # Not applicable
    TRUE ~ -3
  ),
  ecoactadu32 = W9DACTIVITYC) # Detailed

# Cleaning detailed variables missing values
full_df <- full_df %>%
  mutate(ecoactadu25 = case_when(ecoactadu25 == -9 ~ -9, ecoactadu25 == -8 ~ -8, ecoactadu25 == -1 ~ -1, TRUE ~ coalesce(ecoactadu25, -3)),
         ecoactadu32 = case_when(ecoactadu32 == -9 ~ -9, ecoactadu32 == -8 ~ -8, ecoactadu32 == -1 ~ -1, TRUE ~ coalesce(ecoactadu32, -3)))

# Final selection
final_vars <- c('NSID', 'ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32', 'ecoactadu25', 'ecoactadu32')
output_df <- full_df %>% select(all_of(final_vars))

# Set labels using labelled package. 
# The error was caused by the names of the vector being characters, but the vector itself being characters.
# set_value_labels requires a named vector where names are the labels and values are the codes (numeric).
eco_labels_vec <- c(
  'Paid work' = 1,
  'Unemployed' = 2,
  'Education/Training' = 3,
  'Home/Family' = 4,
  'Sick/Disabled' = 5,
  'Other' = 6,
  'Refusal' = -9,
  'Don\'t know' = -8,
  'Prefer not to say' = -7,
  'Not asked' = -3,
  'Schedule not applicable' = -2,
  'Not applicable' = -1
)

# Apply value labels to collapsed variables
collapsed_vars <- c('ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32')
for (var in collapsed_vars) {
  output_df[[var]] <- set_value_labels(output_df[[var]], eco_labels_vec)
}

write_csv(output_df, 'data/output/cleaned_data.csv')
