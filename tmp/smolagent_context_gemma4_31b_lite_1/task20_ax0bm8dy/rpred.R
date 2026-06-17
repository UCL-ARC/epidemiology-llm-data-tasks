library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# File paths
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

# Load data into a list
data_list <- map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', show_col_types = FALSE))

# Merge all datasets using a full join on NSID
# Start with the first file and iteratively join the rest
cohort_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  cohort_df <- full_join(cohort_df, data_list[[i]], by = 'NSID')
}

# Function to check if they have drunk alcohol at a specific wave
# Returns 1 if Yes, 0 if No, NA if missing/don't know
check_alc <- function(val, wave_type = 'ever') {
  if (is.na(val)) return(NA)
  if (wave_type == 'ever') {
    if (val == 1) return(1) # Yes
    if (val == 2) return(0) # No
  } else if (wave_type == 'audit') {
    # AUDIT1: 1 = Never, 2-5 = Consumed
    if (val == 1) return(0) # Never
    if (val >= 2 && val <= 5) return(1) # Consumed
  }
  return(NA)
}

# Calculate alcfst
cohort_df <- cohort_df %>% 
  rowwise() %>% 
  mutate(
    alc14 = check_alc(W1alceverYP),
    alc15 = check_alc(W2alceverYP),
    alc16 = check_alc(W3alceverYP),
    alc17 = check_alc(W4AlcEverYP),
    alc19 = check_alc(W6AlcEverYP),
    alc20 = check_alc(W7AlcEverYP),
    alc25 = check_alc(W8AUDIT1, 'audit'),
    alc32 = check_alc(W9AUDIT1, 'audit')
  ) %>% 
  ungroup()

# Determine earliest age
cohort_df <- cohort_df %>% 
  rowwise() %>% 
  mutate(
    alcfst = case_when(
      alc14 == 1 ~ 14,
      alc15 == 1 ~ 15,
      alc16 == 1 ~ 16,
      alc17 == 1 ~ 17,
      alc19 == 1 ~ 19,
      alc20 == 1 ~ 20,
      alc25 == 1 ~ 25,
      alc32 == 1 ~ 32,
      (alc14 == 0 | alc15 == 0 | alc16 == 0 | alc17 == 0 | alc19 == 0 | alc20 == 0 | alc25 == 0 | alc32 == 0) ~ 99,
      TRUE ~ -3
    )
  ) %>% 
  ungroup()

# Final output
final_df <- cohort_df %>% 
  select(NSID, alcfst)

write_csv(final_df, 'data/output/cleaned_data.csv')