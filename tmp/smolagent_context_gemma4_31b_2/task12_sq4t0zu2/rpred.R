library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Define files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab'
)

# 2. Load and preprocess each file to minimize memory
load_and_prep <- function(f) {
  path <- paste0('data/input/', f)
  # Define needed cols for this file
  needed <- c('NSID')
  if (f == 'wave_four_lsype_young_person_2020.tab') needed <- c('NSID', 'W4nsseccatYP')
  if (f == 'wave_five_lsype_young_person_2020.tab') needed <- c('NSID', 'W5nsseccatYP')
  if (f == 'wave_six_lsype_young_person_2020.tab') needed <- c('NSID', 'w6nsseccatYP')
  if (f == 'wave_seven_lsype_young_person_2020.tab') needed <- c('NSID', 'W7NSSECCat')
  if (f == 'ns8_2015_derived.tab') needed <- c('NSID', 'W8DNSSEC17', 'W8DACTIVITYC')
  if (f == 'ns9_2022_main_interview.tab') needed <- c('NSID', 'W9NSSEC')
  
  # Read file
  df <- read_delim(path, delim = '\t')
  
  # Ensure NSID is the first column and character
  if (!'NSID' %in% colnames(df)) {
    colnames(df)[1] <- 'NSID'
  }
  df$NSID <- as.character(df$NSID)
  
  # Filter columns and ensure numeric for the values
  cols_present <- intersect(colnames(df), needed)
  df <- df %>% select(all_of(cols_present))
  df <- df %>% mutate(across(-NSID, as.numeric))
  
  # Deduplicate NSID to avoid many-to-many join memory explosion
  df <- df %>% distinct(NSID, .keep_all = TRUE)
  
  return(df)
}

data_list <- map(files, load_and_prep)

# 3. Merge datasets
full_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# 4. Define labels
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
  '17' = 'Not classifiable for other reasons'
)

# 5. Harmonization function
clean_nssec <- function(var) {
  if (is.null(var)) return(rep(-3, 1))
  
  # Integer part for major categories
  res <- floor(var)
  
  # Mapping based on general guidance and metadata
  res <- case_when(
    var == -91.0 ~ -1, # Not applicable
    var == -99.0 ~ -3, # Not interviewed
    var < -90 & var != -91.0 ~ -3, 
    is.na(var) ~ -3,
    TRUE ~ res
  )
  
  # Keep within 1-17 or keep the negative missing codes
  res <- ifelse(res >= 1 & res <= 17, res, 
                ifelse(res < 0, res, -3))
  return(res)
}

# 6. Derive variables
full_df$nssec17 <- clean_nssec(full_df$W4nsseccatYP)
full_df$nssec18 <- clean_nssec(full_df$W5nsseccatYP)
full_df$nssec19 <- clean_nssec(full_df$w6nsseccatYP)
full_df$nssec20 <- clean_nssec(full_df$W7NSSECCat)
full_df$nssec25 <- clean_nssec(full_df$W8DNSSEC17)

# Specific rule for nssec25
if ('W8DACTIVITYC' %in% colnames(full_df)) {
  full_df$nssec25 <- ifelse(!is.na(full_df$W8DACTIVITYC) & full_df$W8DACTIVITYC == 5, 15, full_df$nssec25)
}

full_df$nssec32 <- clean_nssec(full_df$W9NSSEC)

# 7. Final selection and labeling
final_vars <- c('NSID', 'nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25', 'nssec32')
output_df <- full_df %>% select(all_of(final_vars))

missing_levels <- c(-9, -8, -7, -3, -2, -1)
missing_labels <- c('Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Schedule not applicable', 'Not applicable')

for(col in grep('nssec', final_vars, value = TRUE)) {
  all_levels <- c(missing_levels, 1:17)
  all_labels <- c(missing_labels, nssec_labels)
  output_df[[col]] <- factor(output_df[[col]], levels = all_levels, labels = all_labels)
}

write_csv(output_df, 'data/output/cleaned_data.csv')
