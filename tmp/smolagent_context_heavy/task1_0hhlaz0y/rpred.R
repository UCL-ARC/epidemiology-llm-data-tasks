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
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

load_file <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = "c"))
}

data_list <- map(files, load_file)

# Merge all datasets by NSID
full_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# 2. Harmonisation function
harmonise_sex <- function(val, wave_name) {
  if (is.na(val)) return(-3)
  val_num <- as.numeric(val)
  if (is.na(val_num)) return(-3)

  if (val_num == 1) return(1)
  if (val_num == 2) return(2)

  if (wave_name == 'W1sexYP') {
    if (val_num == -92) return(-9)
    if (val_num == -91) return(-1)
    if (val_num == -99) return(-3)
  } else if (wave_name == 'W2SexYP') {
    if (val_num == -92) return(-9)
    if (val_num == -91) return(-1)
    if (val_num == -1) return(-8)
    if (val_num %in% c(-998, -997, -995)) return(-2)
    if (val_num == -99) return(-3)
  } else if (wave_name == 'W3sexYP') {
    if (val_num == -92) return(-9)
    if (val_num == -91) return(-1)
    if (val_num == -99) return(-3)
  } else if (wave_name == 'W4SexYP') {
    if (val_num == -92) return(-9)
    if (val_num == -91) return(-1)
    if (val_num == -1) return(-8)
    if (val_num == -99) return(-3)
  } else if (wave_name == 'W5SexYP') {
    if (val_num == -1) return(-8)
  } else if (wave_name == 'W6Sex') {
    if (val_num == -92) return(-9)
    if (val_num == -91) return(-1)
  } else if (wave_name == 'W7Sex') {
    if (val_num == -91) return(-1)
  } else if (wave_name == 'W8CMSEX') {
    if (val_num == -9) return(-9)
    if (val_num == -8) return(-8)
    if (val_num == -1) return(-1)
  }
  
  return(-3)
}

sex_vars <- c('W1sexYP', 'W2SexYP', 'W3sexYP', 'W4SexYP', 'W5SexYP', 'W6Sex', 'W7Sex', 'W8CMSEX', 'W9DSEX')

# Create a dataframe with harmonised values
sex_harmonised <- full_df %>% 
  select(NSID, all_of(sex_vars)) %>% 
  mutate(across(all_of(sex_vars), ~ sapply(.x, function(x) harmonise_sex(x, cur_column()))))

# Logic: Most recent valid (1 or 2) first, then most recent missing
get_sex_final <- function(row) {
  # row is W1...W9
  # Check for substantive valid responses from W9 back to W1
  for (i in 9:1) {
    if (!is.na(row[i]) && row[i] %in% c(1, 2)) return(row[i])
  }
  # Fallback to most recent missing code
  for (i in 9:1) {
    if (!is.na(row[i])) return(row[i])
  }
  return(-3)
}

sex_matrix <- as.matrix(sex_harmonised %>% select(-NSID))
final_sex_vec <- apply(sex_matrix, 1, get_sex_final)

final_df <- data.frame(NSID = full_df$NSID, sex = final_sex_vec)

# Apply labels
final_df$sex <- set_value_labels(final_df$sex, 
                                c('Male' = 1, 'Female' = 2, 'Refusal' = -9, 'Don\'t know' = -8, 
                                   'Prefer not to say' = -7, 'Not asked' = -3, 
                                   'Schedule not applicable' = -2, 'Not applicable' = -1))

readr::write_csv(final_df, 'data/output/cleaned_data.csv')
