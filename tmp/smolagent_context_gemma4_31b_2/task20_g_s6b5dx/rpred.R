library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
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

data_list <- map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))) 

# Merge datasets using full_join by NSID
full_df <- data_list[[1]]
for(i in 2:length(data_list)){
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# Define a helper to check drinking status
# Returns: 1 for drinking, 0 for not drinking, NA for missing
check_drinking <- function(val, wave) {
  if (is.na(val)) return(NA)
  
  # For S1-S7, 1=Yes, 2=No. 
  # For S8-S9, 1=Never, 2-5=Drinking
  if (wave %in% c(1, 2, 3, 4, 6, 7)) {
    if (val == 1) return(1)
    if (val == 2) return(0)
    return(NA)
  } else if (wave %in% c(8, 9)) {
    if (val == 1) return(0)
    if (val >= 2 && val <= 5) return(1)
    return(NA)
  }
  return(NA)
}

# Create a dataframe of drinking indicators per age
# Age mapping: S1:14, S2:15, S3:16, S4:17, S6:19, S7:20, S8:25, S9:32

results <- full_df %>% 
  rowwise() %>% 
  mutate(
    # Sweep 1 special rule: W1alceverYP == 1 AND W1alcmonYP == 1
    d14 = if(!is.na(W1alceverYP) && !is.na(W1alcmonYP) && W1alceverYP == 1 && W1alcmonYP == 1) 1 
          else if(!is.na(W1alceverYP) && W1alceverYP == 2) 0 
          else if(!is.na(W1alcmonYP) && W1alcmonYP == 2) 0
          else NA,
    d15 = check_drinking(W2alceverYP, 2),
    d16 = check_drinking(W3alceverYP, 3),
    d17 = check_drinking(W4AlcEverYP, 4),
    d19 = check_drinking(W6AlcEverYP, 6),
    d20 = check_drinking(W7AlcEverYP, 7),
    d25 = check_drinking(W8AUDIT1, 8),
    d32 = check_drinking(W9AUDIT1, 9)
  ) %>% 
  ungroup()

# Calculate alcfst
ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
cols <- c('d14', 'd15', 'd16', 'd17', 'd19', 'd20', 'd25', 'd32')

# Use a loop or apply to determine alcfst for each row
final_alcfst <- apply(results[, cols], 1, function(row) {
  # Find first index where drinking is 1
  first_drink <- which(row == 1)
  if (length(first_drink) > 0) {
    return(ages[first_drink[1]])
  }
  
  # If no drinking observed (all are 0 or NA)
  # Condition 1: All observed are 0 and none are NA -> 99
  if (all(!is.na(row)) && all(row == 0)) {
    return(99)
  }
  
  # Condition 2: All observed are 0, but at least one is NA -> -8
  if (all(row %in% c(0, NA)) && any(!is.na(row))) {
    return(-8)
  }
  
  # All are NA
  return(-8)
})

final_df <- data.frame(NSID = full_df$NSID, alcfst = final_alcfst)

# Convert to factor with specified levels and labels
levels_val <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
levels_lab <- c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")

final_df$alcfst <- factor(final_df$alcfst, levels = levels_val, labels = levels_lab)

write_csv(final_df, 'data/output/cleaned_data.csv')
