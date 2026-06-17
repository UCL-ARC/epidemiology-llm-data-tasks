library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define files to load
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

# Load datasets
data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(.default = 'c'))
})

# Ensure names match for merging
names(data_list) <- files

# Merge all datasets by NSID
cohort_df <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]], by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID') %>% 
  full_join(data_list[[6]], by = 'NSID') %>% 
  full_join(data_list[[7]], by = 'NSID') %>% 
  full_join(data_list[[8]], by = 'NSID') %>% 
  full_join(data_list[[9]], by = 'NSID')

# Helper function to map missing values based on label meaning
map_sex_missing <- function(val, wave) {
  # Handle NA input explicitly
  if (is.na(val) || is.null(val)) return(-3)
  
  # Convert to numeric
  num_val <- as.numeric(val)
  if (is.na(num_val)) return(-3)
  
  # Mapping logic based on metadata labels
  if (wave == 'W1') {
    if (num_val == -99) return(-3) 
    if (num_val == -92) return(-9) 
    if (num_val == -91) return(-1) 
  } else if (wave == 'W2') {
    if (num_val == -998) return(-2) 
    if (num_val == -997) return(-2) 
    if (num_val == -995) return(-2) 
    if (num_val == -99) return(-3) 
    if (num_val == -92) return(-9) 
    if (num_val == -91) return(-1) 
    if (num_val == -1) return(-8)  
  } else if (wave == 'W3') {
    if (num_val == -99) return(-3) 
    if (num_val == -92) return(-9) 
    if (num_val == -91) return(-1) 
  } else if (wave == 'W4') {
    if (num_val == -99) return(-3) 
    if (num_val == -92) return(-9) 
    if (num_val == -91) return(-1) 
    if (num_val == -1) return(-8)  
  } else if (wave == 'W5') {
    if (num_val == -1) return(-8)  
  } else if (wave == 'W6') {
    if (num_val == -92) return(-9) 
    if (num_val == -91) return(-1) 
  } else if (wave == 'W7') {
    if (num_val == -91) return(-1) 
  } else if (wave == 'W8') {
    if (num_val == -9) return(-9) 
    if (num_val == -8) return(-8) 
    if (num_val == -1) return(-1) 
  } else if (wave == 'W9') {
    # No specific missing labels in metadata for W9
  }
  
  return(num_val)
}

# Process each sex variable
sex_vars <- c('W1sexYP', 'W2SexYP', 'W3sexYP', 'W4SexYP', 'W5SexYP', 'W6Sex', 'W7Sex', 'W8CMSEX', 'W9DSEX')
waves <- c('W1', 'W2', 'W3', 'W4', 'W5', 'W6', 'W7', 'W8', 'W9')

# Use a list to collect results to avoid matrix assignment issues with empty/incorrect lengths
cleaned_sex_list <- list()
for(i in 1:length(sex_vars)) {
  var_name <- sex_vars[i]
  wave_id <- waves[i]
  
  # Ensure the column exists in cohort_df, otherwise fill with NA
  if (var_name %in% names(cohort_df)) {
    col_data <- cohort_df[[var_name]]
    cleaned_sex_list[[i]] <- sapply(col_data, map_sex_missing, wave = wave_id)
  } else {
    cleaned_sex_list[[i]] <- rep(-3, nrow(cohort_df))
  }
}

# Combine into a data frame
sex_matrix <- as.data.frame(cleaned_sex_list)

# Most-recent-valid-first consolidation for sex
consolidate_sex <- function(row) {
  # Reverse the order to check most recent first
  row_rev <- rev(row)
  for (val in row_rev) {
    if (!is.na(val) && (val == 1 || val == 2)) return(val)
  }
  # Fall back to most recent missing code
  for (val in row_rev) {
    if (!is.na(val)) return(val)
  }
  return(-3)
}

final_sex_values <- apply(sex_matrix, 1, consolidate_sex)

# Create final dataframe
output_df <- data.frame(NSID = cohort_df$NSID, sex = final_sex_values)

# Convert sex to factor with explicit labels
output_df$sex <- factor(output_df$sex, 
                        levels = c(1, 2, -9, -8, -7, -3, -2, -1), 
                        labels = c('Male', 'Female', 'Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Schedule not applicable', 'Not applicable'))

# Write to CSV
write_csv(output_df, 'data/output/cleaned_data.csv')
