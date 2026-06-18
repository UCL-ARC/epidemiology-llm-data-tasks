library(haven)
library(dplyr)
library(readr)

# Load all files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_five <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave_six <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave_eight <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave_nine <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Create a function to clean and map missing values
clean_sex_var <- function(df, var_name) {
  if (var_name %in% names(df)) {
    df[[var_name]] <- as.numeric(df[[var_name]])
    df[[var_name]][is.na(df[[var_name]])] <- -3
    df[[var_name]][df[[var_name]] %in% c(-999, -998, -997, -995)] <- -2
    df[[var_name]][df[[var_name]] == -92] <- -9
    df[[var_name]][df[[var_name]] == -91] <- -1
    df[[var_name]][df[[var_name]] == -99] <- -3
    df[[var_name]][df[[var_name]] == -1] <- -1
  }
  return(df)
}

# Start with wave one to get all IDs
base_data <- wave_one %>% select(NSID)

# Add each sex variable with proper cleaning
base_data <- base_data %>% 
  left_join(clean_sex_var(wave_nine, 'W9DSEX') %>% select(NSID, W9DSEX), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_eight, 'W8CMSEX') %>% select(NSID, W8CMSEX), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_seven, 'W7Sex') %>% select(NSID, W7Sex), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_six, 'W6Sex') %>% select(NSID, W6Sex), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_five, 'W5SexYP') %>% select(NSID, W5SexYP), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_four, 'W4SexYP') %>% select(NSID, W4SexYP), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_three, 'W3sexYP') %>% select(NSID, W3sexYP), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_two, 'W2SexYP') %>% select(NSID, W2SexYP), by = 'NSID')

base_data <- base_data %>% 
  left_join(clean_sex_var(wave_one, 'W1sexYP') %>% select(NSID, W1sexYP), by = 'NSID')

# Derive consolidated sex variable
base_data <- base_data %>% 
  mutate(sex = coalesce(
    ifelse(W9DSEX %in% c(1, 2), W9DSEX, NA),
    ifelse(W8CMSEX %in% c(1, 2), W8CMSEX, NA),
    ifelse(W7Sex %in% c(1, 2), W7Sex, NA),
    ifelse(W6Sex %in% c(1, 2), W6Sex, NA),
    ifelse(W5SexYP %in% c(1, 2), W5SexYP, NA),
    ifelse(W4SexYP %in% c(1, 2), W4SexYP, NA),
    ifelse(W3sexYP %in% c(1, 2), W3sexYP, NA),
    ifelse(W2SexYP %in% c(1, 2), W2SexYP, NA),
    ifelse(W1sexYP %in% c(1, 2), W1sexYP, NA),
    -3
  ))

# Create labelled factor for sex
base_data$sex <- factor(
  base_data$sex,
  levels = c(-9, -8, -7, -3, -2, -1, 1, 2),
  labels = c('Refusal', 'Don\'t know', 'Prefer not to say', 
             'Not interviewed', 'Schedule error', 'Not applicable', 
             'Male', 'Female')
)

# Select only NSID and sex for output
final_data <- base_data %>% select(NSID, sex)

# Write output to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation
message('Data cleaning complete. Output written to data/output/cleaned_data.csv')