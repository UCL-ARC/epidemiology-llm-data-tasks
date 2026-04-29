library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- list(
  wave1 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave4 = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave5 = 'data/input/wave_five_lsype_young_person_2020.tab',
  wave6 = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave7 = 'data/input/wave_seven_lsype_young_person_2020.tab',
  wave8 = 'data/input/ns8_2015_derived.tab',
  wave9 = 'data/input/ns9_2022_main_interview.tab'
)

# Use read_delim without the 'guess' shortcut which caused the error
data_list <- map(files, ~read_delim(.x, delim = '\t'))

# Merge datasets
full_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# Define standard labels for NS-SEC 17 categories
nssec_labels <- c(
  'Employers in large organisations',
  'Higher managerial and administrative occupations',
  'Higher professional occupations',
  'Lower professional and higher technical occupations',
  'Lower managerial and administrative occupations',
  'Higher supervisory occupations',
  'Intermediate occupations',
  'Employers in small establishments',
  'Own account workers',
  'Lower supervisory occupations',
  'Lower technical occupations',
  'Semi-routine occupations',
  'Routine occupations',
  'Never worked and Long-term unemployed',
  'Full-time students',
  'Occupations not stated or inadequately described',
  'Not classifiable for other reasons'
)

# Helper to clean and collapse NS-SEC
process_nssec <- function(var_name, df) {
  vec <- df[[var_name]]
  
  res <- map_dbl(vec, function(x) {
    if (is.na(x)) return(-3)
    if (x == -91) return(-1)
    if (x == -99) return(-3)
    if (x < 0) return(x)
    return(floor(x))
  })
  
  return(res)
}

# Process each wave
full_df$nssec17 <- process_nssec('W4nsseccatYP', full_df)
full_df$nssec18 <- process_nssec('W5nsseccatYP', full_df)
full_df$nssec19 <- process_nssec('w6nsseccatYP', full_df)
full_df$nssec20 <- process_nssec('W7NSSECCat', full_df)

# Age 25: W8DNSSEC17 and W8DACTIVITYC
full_df$nssec25 <- map2_dbl(full_df$W8DNSSEC17, full_df$W8DACTIVITYC, function(sec, act) {
  if (!is.na(act) && act == 5) return(15)
  if (is.na(sec) && is.na(act)) return(-3)
  if (is.na(sec)) return(-3)
  if (sec < 0) return(sec)
  return(floor(sec))
})

# Age 32: W9NSSEC
full_df$nssec32 <- process_nssec('W9NSSEC', full_df)

# Final cleaning and factor conversion
nssec_vars <- c('nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25', 'nssec32')
for (v in nssec_vars) {
  full_df[[v]] <- as.numeric(full_df[[v]])
  # Harmonise missing values based on Rule 6/7
  full_df[[v]][full_df[[v]] == -91] <- -1
  full_df[[v]][full_df[[v]] == -99] <- -3
  
  levels_vec <- c(-9, -8, -7, -3, -2, -1, 1:17)
  labels_vec <- c('Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Schedule not applicable', 'Not applicable', nssec_labels)
  
  full_df[[v]] <- factor(full_df[[v]], levels = levels_vec, labels = labels_vec)
}

# Output: ID and derived variables only
final_output <- full_df %>% select(NSID, all_of(nssec_vars))

write_csv(final_output, 'data/output/cleaned_data.csv')