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
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))
names(data_list) <- files

# Create the full cohort frame
full_frame <- data_list[[1]] %>% select(NSID)
for (i in 2:length(data_list)) {
  full_frame <- full_frame %>% full_join(data_list[[i]] %>% select(NSID), by = 'NSID')
}

# Standard Missing Value Mapper
map_missing <- function(val, label) {
  if (is.na(val)) return(-3)
  lbl <- tolower(label)
  if (grepl('refused', lbl)) return(-9)
  if (grepl('insufficient information|don\'t know', lbl)) return(-8)
  if (grepl('prefer not to say', lbl)) return(-7)
  if (grepl('not asked', lbl)) return(-3)
  if (grepl('not applicable|script error|information lost', lbl)) return(-2)
  return(as.numeric(val))
}

# Process variables

# regub15 and regov15 (Wave 2)
w2_data <- data_list[['wave_two_lsype_family_background_2020.tab']]
regub15_labels <- c('-94.0' = 'Insufficient information', '1.0' = 'Urban >= 10k - sparse', '2.0' = 'Town & Fringe - sparse', '3.0' = 'Village - sparse', '4.0' = 'Hamlet and Isolated Dwelling - sparse', '5.0' = 'Urban >= 10k - less sparse', '6.0' = 'Town & Fringe - less sparse', '7.0' = 'Village - less sparse', '8.0' = 'Hamlet & Isolated Dwelling')
regov15_labels <- c('-94.0' = 'Insufficient information', '1.0' = 'North East', '2.0' = 'North West', '3.0' = 'Yorkshire and The Humber', '4.0' = 'East Midlands', '5.0' = 'West Midlands', '6.0' = 'East of England', '7.0' = 'London', '8.0' = 'South East', '9.0' = 'South West')

w2_clean <- w2_data %>% 
  mutate(
    regub15 = map2_dbl(urbind, regub15_labels[urbind], map_missing),
    regov15 = map2_dbl(gor, regov15_labels[gor], map_missing)
  ) %>% 
  select(NSID, regub15, regov15)

# regub16 and regov16 (Wave 3)
w3_data <- data_list[['wave_three_lsype_family_background_2020.tab']]
regub16_labels <- c('-94.0' = 'Insufficient information', '1.0' = 'Urban >= 10k - sparse', '2.0' = 'Town & Fringe - sparse', '3.0' = 'Village - sparse', '4.0' = 'Hamlet and Isolated Dwelling - sparse', '5.0' = 'Urban >= 10k - less sparse', '6.0' = 'Town & Fringe - less sparse', '7.0' = 'Village - less sparse', '8.0' = 'Hamlet & Isolated Dwelling')
regov16_labels <- c('-94.0' = 'Insufficient information', '1.0' = 'North East', '2.0' = 'North West', '3.0' = 'Yorkshire and the Humber', '4.0' = 'East Midlands', '5.0' = 'West Midlands', '6.0' = 'East of England', '7.0' = 'London', '8.0' = 'South East', '9.0' = 'South West')

w3_clean <- w3_data %>% 
  mutate(
    regub16 = map2_dbl(urbind, regub16_labels[urbind], map_missing),
    regov16 = map2_dbl(gor, regov16_labels[gor], map_missing)
  ) %>% 
  select(NSID, regub16, regov16)

# regor25 (Wave 8)
w8_data <- data_list[['ns8_2015_derived.tab']]
regor25_labels <- c('-9.0' = 'Refused', '-8.0' = 'Insufficient information', '-1.0' = 'Not applicable', '1.0' = 'North East', '2.0' = 'North West', '3.0' = 'Yorkshire and the Humber', '4.0' = 'East Midlands', '5.0' = 'West Midlands', '6.0' = 'East of England', '7.0' = 'London', '8.0' = 'South East', '9.0' = 'South West', '10.0' = 'Wales', '11.0' = 'Scotland', '12.0' = 'Northern Ireland', '13.0' = 'Unknown due to faulty/missing postcode')

w8_clean <- w8_data %>% 
  mutate(regor25 = map_dbl(W8DGOR, ~map_missing(.x, regor25_labels[.x]))) %>% 
  select(NSID, regor25)

# regor32 (Wave 9 Derived)
w9_der_data <- data_list[['ns9_2022_derived_variables.tab']]
regor32_labels <- c('-9.0' = 'Refused', '-8.0' = 'Insufficient information', '-1.0' = 'Not applicable', '1.0' = 'North East', '2.0' = 'North West', '3.0' = 'Yorkshire and the Humber', '4.0' = 'East Midlands', '5.0' = 'West Midlands', '6.0' = 'East of England', '7.0' = 'London', '8.0' = 'South East', '9.0' = 'South West', '10.0' = 'Wales', '11.0' = 'Scotland', '12.0' = 'Northern Ireland', '13.0' = 'Unknown due to faulty/missing postcode')

w9_der_clean <- w9_der_data %>% 
  mutate(regor32 = map_dbl(W9DRGN, ~map_missing(.x, regor32_labels[.x]))) %>% 
  select(NSID, regor32)

# regint32 (Wave 9 Main)
w9_main_data <- data_list[['ns9_2022_main_interview.tab']]
regint32_labels <- c('-9.0' = 'Refused', '-8.0' = "Don't know", '-3.0' = 'Not asked at fieldwork stage', '-1.0' = 'Not applicable', '1.0' = 'England', '2.0' = 'Scotland', '3.0' = 'Wales', '4.0' = 'Northern Ireland', '5.0' = 'Outside of UK or unknown')

w9_main_clean <- w9_main_data %>% 
  mutate(regint32 = map_dbl(W9NATIONRES, ~map_missing(.x, regint32_labels[.x]))) %>% 
  select(NSID, regint32)

# Final Merge
final_output <- full_frame %>% 
  select(NSID) %>% 
  full_join(w2_clean, by = 'NSID') %>% 
  full_join(w3_clean, by = 'NSID') %>% 
  full_join(w8_clean, by = 'NSID') %>% 
  full_join(w9_der_clean, by = 'NSID') %>% 
  full_join(w9_main_clean, by = 'NSID')

# Convert all NAs to -3
final_output <- final_output %>% 
  mutate(across(starts_with('reg'), ~if_else(is.na(.x), -3, .x)))

write_csv(final_output, 'data/output/cleaned_data.csv')
