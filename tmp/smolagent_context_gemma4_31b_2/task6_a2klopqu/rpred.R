library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files_to_load <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab',
  'ns9_2022_main_interview.tab'
)

# Read tab function
read_tab <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'), guess_max = 10000)
}

# Read all files into a list
data_list <- map(files_to_load, read_tab)
names(data_list) <- files_to_load

# Create the full cohort frame by merging all datasets
full_frame <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_frame <- full_join(full_frame, data_list[[i]], by = 'NSID')
}

# To handle columns with same names across files (like urbind, gor), 
# let's explicitly extract variables from the original list items to avoid .x .y suffixes

data_w2 <- data_list[['wave_two_lsype_family_background_2020.tab']]
data_w3 <- data_list[['wave_three_lsype_family_background_2020.tab']]
data_w8 <- data_list[['ns8_2015_derived.tab']]
data_w9d <- data_list[['ns9_2022_derived_variables.tab']]
data_w9m <- data_list[['ns9_2022_main_interview.tab']]

# Initialize final dataframe with the full cohort ID list
final_df <- data.frame(NSID = full_frame$NSID, stringsAsFactors = FALSE)

# Harmonise function for urban/gov (W2, W3)
harmonise_ug <- function(val) {
  case_when(
    val == -94 ~ -8, # Insufficient information
    val >= 1 & val <= 12 ~ val, # Valid categories
    is.na(val) ~ -3, # Not asked/missing
    TRUE ~ -3
  )
}

# regub15 and regov15 from W2
final_df <- final_df %>%
  left_join(data_w2 %>% select(NSID, urbind), by = 'NSID') %>%
  mutate(regub15 = harmonise_ug(urbind)) %>%
  select(-urbind) %>%
  left_join(data_w2 %>% select(NSID, gor), by = 'NSID') %>%
  mutate(regov15 = harmonise_ug(gor)) %>%
  select(-gor)

# regub16 and regov16 from W3
final_df <- final_df %>%
  left_join(data_w3 %>% select(NSID, urbind), by = 'NSID') %>%
  mutate(regub16 = harmonise_ug(urbind)) %>%
  select(-urbind) %>%
  left_join(data_w3 %>% select(NSID, gor), by = 'NSID') %>%
  mutate(regov16 = harmonise_ug(gor)) %>%
  select(-gor)

# regor25 from W8DGOR
final_df <- final_df %>%
  left_join(data_w8 %>% select(NSID, W8DGOR), by = 'NSID') %>%
  mutate(regor25 = case_when(
    W8DGOR == 13 ~ -2, # Unknown due to faulty/missing postcode
    W8DGOR == -9 ~ -9, # Refused
    W8DGOR == -8 ~ -8, # Insufficient information
    W8DGOR == -1 ~ -1, # Not applicable
    W8DGOR >= 1 & W8DGOR <= 12 ~ W8DGOR,
    TRUE ~ -3
  )) %>%
  select(-W8DGOR)

# regor32 from W9DRGN
final_df <- final_df %>%
  left_join(data_w9d %>% select(NSID, W9DRGN), by = 'NSID') %>%
  mutate(regor32 = case_when(
    W9DRGN == 13 ~ -2, # Unknown due to faulty/missing postcode
    W9DRGN == -9 ~ -9, # Refused
    W9DRGN == -8 ~ -8, # Insufficient information
    W9DRGN == -1 ~ -1, # Not applicable
    W9DRGN >= 1 & W9DRGN <= 12 ~ W9DRGN,
    TRUE ~ -3
  )) %>%
  select(-W9DRGN)

# regint32 from W9NATIONRES
# 1 = In the UK (1,2,3,4) and 2 = Abroad (5)
final_df <- final_df %>%
  left_join(data_w9m %>% select(NSID, W9NATIONRES), by = 'NSID') %>%
  mutate(regint32 = case_when(
    W9NATIONRES >= 1 & W9NATIONRES <= 4 ~ 1,
    W9NATIONRES == 5 ~ 2,
    W9NATIONRES == -9 ~ -9, # Refused
    W9NATIONRES == -8 ~ -8, # Don't know
    W9NATIONRES == -3 ~ -3, # Not asked
    W9NATIONRES == -1 ~ -1, # Not applicable
    TRUE ~ -3
  )) %>%
  select(-W9NATIONRES)

# Final selection and cleanup
final_output <- final_df %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

write_csv(final_output, 'data/output/cleaned_data.csv')
