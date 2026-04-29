library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
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

load_data <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(.default = 'c'))
}

# Load and convert numeric columns needed for the task
# We load all as character first to avoid issues, then convert specific ones
data_list <- map(files, load_data)
names(data_list) <- files

# Merge all datasets
full_df <- data_list %>% reduce(full_join, by = 'NSID')

# Convert relevant variables to numeric
vars_to_numeric <- c(
  'W1alceverYP', 'W1alcmonYP', 'W2alceverYP', 'W3alceverYP', 
  'W4AlcEverYP', 'W6AlcEverYP', 'W7AlcEverYP', 'W8AUDIT1', 'W9AUDIT1'
)

full_df <- full_df %>% 
  mutate(across(all_of(vars_to_numeric), as.numeric))

# 2. Derivation Logic
# Define Wave-to-Age mapping
# S1:14, S2:15, S3:16, S4:17, S6:19, S7:20, S8:25, S9:32

full_df <- full_df %>% 
  mutate(
    # Candidate ages
    age14 = if_else(W1alceverYP == 1 & W1alcmonYP == 1, 14, NA_real_),
    age15 = if_else(W2alceverYP == 1, 15, NA_real_),
    age16 = if_else(W3alceverYP == 1, 16, NA_real_),
    age17 = if_else(W4AlcEverYP == 1, 17, NA_real_),
    age19 = if_else(W6AlcEverYP == 1, 19, NA_real_),
    age20 = if_else(W7AlcEverYP == 1, 20, NA_real_),
    age25 = if_else(W8AUDIT1 > 1, 25, NA_real_),
    age32 = if_else(W9AUDIT1 > 1, 32, NA_real_)
  )

# Identify if drinking is observed in any wave
# Valid substantive responses for 'Ever' indicators: 1 (Yes), 2 (No)
# For AUDIT: 1 (Never), >1 (Drinking)

full_df <- full_df %>% 
  mutate(
    # Indicators for drinking (1 = drank, 0 = didn't drink, NA = missing/unknown)
    ind14 = case_when(W1alceverYP == 1 & W1alcmonYP == 1 ~ 1, W1alceverYP == 2 | W1alcmonYP == 2 ~ 0, TRUE ~ NA_real_),
    ind15 = case_when(W2alceverYP == 1 ~ 1, W2alceverYP == 2 ~ 0, TRUE ~ NA_real_),
    ind16 = case_when(W3alceverYP == 1 ~ 1, W3alceverYP == 2 ~ 0, TRUE ~ NA_real_),
    ind17 = case_when(W4AlcEverYP == 1 ~ 1, W4AlcEverYP == 2 ~ 0, TRUE ~ NA_real_),
    ind19 = case_when(W6AlcEverYP == 1 ~ 1, W6AlcEverYP == 2 ~ 0, TRUE ~ NA_real_),
    ind20 = case_when(W7AlcEverYP == 1 ~ 1, W7AlcEverYP == 2 ~ 0, TRUE ~ NA_real_),
    ind25 = case_when(W8AUDIT1 > 1 ~ 1, W8AUDIT1 == 1 ~ 0, TRUE ~ NA_real_),
    ind32 = case_when(W9AUDIT1 > 1 ~ 1, W9AUDIT1 == 1 ~ 0, TRUE ~ NA_real_)
  )

# Earliest age
full_df <- full_df %>% 
  rowwise() %>% 
  mutate(
    earliest_age = min(c_across(starts_with('age')), na.rm = TRUE),
    # Handle cases where min() returns Inf for all NA
    earliest_age = if(is.infinite(earliest_age)) NA_real_ else earliest_age,
    
    # Never-drinker flag
    # 1 if all observed indicators show not-drinking (0) and none are missing
    # 0 if any indicator shows drinking (1)
    # NA otherwise
    all_obs = list(c(ind14, ind15, ind16, ind17, ind19, ind20, ind25, ind32)),
    never_drinker = case_when(
      any(unlist(all_obs) == 1, na.rm = TRUE) ~ 0,
      all(!is.na(unlist(all_obs)) & unlist(all_obs) == 0) ~ 1,
      TRUE ~ NA_real_
    )
  ) %>% 
  ungroup()

# Final alcfst variable
full_df <- full_df %>% 
  mutate(
    alcfst_num = case_when(
      !is.na(earliest_age) ~ earliest_age,
      never_drinker == 1 ~ 99,
      TRUE ~ -8
    )
  )

# Convert to factor
levels_vals <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
levels_labs <- c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")

full_df$alcfst <- factor(full_df$alcfst_num, levels = levels_vals, labels = levels_labs)

# Final output selection
final_output <- full_df %>% select(NSID, alcfst)

write_csv(final_output, 'data/output/cleaned_data.csv')