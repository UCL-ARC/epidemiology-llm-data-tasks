library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
load_data <- function(fname) {
  readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(.default = 'c'))
}

w1 <- load_data('wave_one_lsype_young_person_2020.tab')
w2 <- load_data('wave_two_lsype_family_background_2020.tab')
w3 <- load_data('wave_three_lsype_family_background_2020.tab')
w4 <- load_data('wave_four_lsype_young_person_2020.tab')
w8 <- load_data('ns8_2015_derived.tab')
w9_der <- load_data('ns9_2022_derived_variables.tab')
w9_main <- load_data('ns9_2022_main_interview.tab')

# Extract specific variables to avoid clashes
regub15_raw <- w2 %>% select(NSID, urbind) %>% rename(urbind15 = urbind)
regov15_raw <- w2 %>% select(NSID, gor) %>% rename(gor15 = gor)
regub16_raw <- w3 %>% select(NSID, urbind) %>% rename(urbind16 = urbind)
regov16_raw <- w3 %>% select(NSID, gor) %>% rename(gor16 = gor)
regor25_raw <- w8 %>% select(NSID, W8DGOR) %>% rename(gor25 = W8DGOR)
regor32_raw <- w9_der %>% select(NSID, W9DRGN) %>% rename(gor32 = W9DRGN)
regint32_raw <- w9_main %>% select(NSID, W9NATIONRES) %>% rename(nation32 = W9NATIONRES)

# Merge
final_df <- w1 %>%
  full_join(regub15_raw, by = 'NSID') %>%
  full_join(regov15_raw, by = 'NSID') %>%
  full_join(regub16_raw, by = 'NSID') %>%
  full_join(regov16_raw, by = 'NSID') %>%
  full_join(regor25_raw, by = 'NSID') %>%
  full_join(regor32_raw, by = 'NSID') %>%
  full_join(regint32_raw, by = 'NSID')

# Harmonise missing values and create final variables
final_df <- final_df %>%
  mutate(
    # regub15
    regub15 = as.numeric(urbind15),
    regub15 = case_when(regub15 == -94 ~ -8, regub15 <= -1 ~ -2, TRUE ~ regub15),
    
    # regub16
    regub16 = as.numeric(urbind16),
    regub16 = case_when(regub16 == -94 ~ -8, regub16 <= -1 ~ -2, TRUE ~ regub16),
    
    # regov15
    regov15 = as.numeric(gor15),
    regov15 = case_when(regov15 == -94 ~ -8, regov15 <= -1 ~ -2, TRUE ~ regov15),
    
    # regov16
    regov16 = as.numeric(gor16),
    regov16 = case_when(regov16 == -94 ~ -8, regov16 <= -1 ~ -2, TRUE ~ regov16),
    
    # regor25 (W8DGOR)
    regor25 = as.numeric(gor25),
    regor25 = case_when(regor25 == -9 ~ -9, regor25 == -8 ~ -8, regor25 == -1 ~ -1, TRUE ~ regor25),
    
    # regor32 (W9DRGN)
    regor32 = as.numeric(gor32),
    regor32 = case_when(regor32 == -9 ~ -9, regor32 == -8 ~ -8, regor32 == -1 ~ -1, TRUE ~ regor32),
    
    # regint32 (W9NATIONRES)
    regint32 = as.numeric(nation32),
    regint32 = case_when(regint32 == -9 ~ -9, regint32 == -8 ~ -8, regint32 == -3 ~ -3, regint32 == -1 ~ -1, TRUE ~ regint32)
  ) %>%
  mutate(across(starts_with('reg'), ~replace_na(., -3)))

# Select only final variables
final_output <- final_df %>% 
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write output
readr::write_csv(final_output, 'data/output/cleaned_data.csv')