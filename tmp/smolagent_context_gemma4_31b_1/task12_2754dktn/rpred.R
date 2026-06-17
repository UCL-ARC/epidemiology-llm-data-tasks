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
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab'
)

# Read datasets
data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))
names(data_list) <- files

# ID Frame
frame <- data_list[['wave_one_lsype_young_person_2020.tab']] %>% select(NSID)

# Helper to safely extract and process NS-SEC
process_nssec_safe <- function(df, source_var, target_var) {
  if(!'NSID' %in% names(df)) return(data.frame(NSID = character()))
  
  if(!source_var %in% names(df)) {
    res <- df %>% select(NSID) %>% mutate(tmp = NA_real_)
    names(res)[names(res) == 'tmp'] <- target_var
    return(res)
  }

  df %>%
    mutate(val = as.numeric(!!sym(source_var))) %>%
    mutate(tmp = floor(val),
           tmp = ifelse(val < 0, case_when(val == -91.0 ~ -1, val == -99.0 ~ -3, TRUE ~ -2), tmp)) %>%
    select(NSID, tmp) %>%
    rename(!!target_var := tmp)
}

# 1. Age 17 (Wave 4)
res17 <- process_nssec_safe(data_list[['wave_four_lsype_young_person_2020.tab']], 'W4nsseccatYP', 'nssec17')

# 2. Age 18 (Wave 5)
res18 <- process_nssec_safe(data_list[['wave_five_lsype_young_person_2020.tab']], 'W5nsseccatYP', 'nssec18')

# 3. Age 19 (Wave 6)
res19 <- process_nssec_safe(data_list[['wave_six_lsype_young_person_2020.tab']], 'w6nsseccatYP', 'nssec19')

# 4. Age 20 (Wave 7)
res20 <- process_nssec_safe(data_list[['wave_seven_lsype_young_person_2020.tab']], 'W7NSSECCat', 'nssec20')

# 5. Age 25 (Wave 8) - Special logic
df8 <- data_list[['ns8_2015_derived.tab']]
if('NSID' %in% names(df8)) {
  res25 <- df8 %>%
    mutate(val = as.numeric(W8DNSSEC17), act = as.numeric(W8DACTIVITYC)) %>%
    mutate(nssec25 = floor(val),
           nssec25 = ifelse(val < 0, case_when(val == -1.0 ~ -1, val == -9.0 ~ -9, val == -8.0 ~ -8, TRUE ~ -2), nssec25),
           nssec25 = ifelse(act == 5, 15, nssec25)) %>%
    select(NSID, nssec25)
} else {
  res25 <- data.frame(NSID = character(), nssec25 = numeric())
}

# 6. Age 32 (Wave 9)
res32 <- process_nssec_safe(data_list[['ns9_2022_main_interview.tab']], 'W9NSSEC', 'nssec32')

# Merge
final_df <- frame %>%
  full_join(res17, by = "NSID") %>%
  full_join(res18, by = "NSID") %>%
  full_join(res19, by = "NSID") %>%
  full_join(res20, by = "NSID") %>%
  full_join(res25, by = "NSID") %>%
  full_join(res32, by = "NSID")

# Missing Values
final_df <- final_df %>%
  mutate(across(starts_with("nssec"), ~replace_na(.x, -3)))

# Labeling
nssec_labels <- c(
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial and administrative occupations',
  '3' = 'Higher professional occupations',
  '4' = 'Lower professional and higher technical occupations',
  '5' = 'Lower managerial and administrative occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate occupations',
  '8' = 'Employers in small establishments',
  '9' = 'Own account workers',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical occupations',
  '12' = 'Semi-routine occupations',
  '13' = 'Routine occupations',
  '14' = 'Never worked and Long-term unemployed',
  '15' = 'Full-time students',
  '16' = 'Occupations not stated or inadequately described',
  '17' = 'Not classifiable for other reasons',
  '-1' = 'Not applicable',
  '-2' = 'Schedule not applicable',
  '-3' = 'Not asked/NA',
  '-7' = 'Prefer not to say',
  '-8' = 'Don\'t know',
  '-9' = 'Refusal'
)

final_df <- final_df %>%
  mutate(across(starts_with("nssec"), ~factor(.x, levels = as.numeric(names(nssec_labels)), labels = nssec_labels)))

write_csv(final_df, 'data/output/cleaned_data.csv')
