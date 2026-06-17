library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load data files using read_delim for tab-delimited files
df1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
df2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
df3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
df4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
df5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
df6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
df7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
df8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
df9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all datasets by NSID using full_join
df_merged <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df5, by = 'NSID') %>%
  full_join(df6, by = 'NSID') %>%
  full_join(df7, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

cat('Merged dataset dimensions:', nrow(df_merged), 'rows,', ncol(df_merged), 'columns\n')

# Identify the sex variables from each wave
sex_vars <- c('W1sexYP', 'W2SexYP', 'W3sexYP', 'W4SexYP', 'W5SexYP', 'W6Sex', 'W7Sex', 'W8CMSEX', 'W9DSEX')

# Check for missing values in sex variables
cat('\nMissing value counts in sex variables:\n')
for (v in sex_vars) {
  cat(v, ':', sum(is.na(df_merged[[v]])), '\n')
}

# Convert raw sex variables to standard format and check for valid values
# Valid values are 1 (Male) and 2 (Female)

# Apply standard missing value codes to derived variables
convert_to_standard_missing <- function(x) {
  # Map various missing value codes to standard codes based on label meaning
  # -999, -998, -997, -995: schedule not applicable / script error / info lost -> -2
  # -94: insufficient info -> -8 (but check metadata)
  # -92: Refused -> -9
  # -91: Not applicable -> -1
  # -99: Not asked / not interviewed -> -3
  # -100, -97: depend on labels
  # -9: Refused -> -9
  # -8: Don't know -> -8
  # -1: Don't know or Not applicable -> depends on label
  
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[x == -9] <- -9
  x[x == -8] <- -8
  # For -1, need to check label meaning
  return(x)
}

# For sex variables, -1 means either "Don't Know" (-8) or "Not applicable" (-1)
# Need to handle this based on wave-specific metadata

# Create a function to process sex variables
process_sex_var <- function(var_name, wave_num) {
  x <- df_merged[[var_name]]
  
  # Apply standard missing value conversion
  x <- convert_to_standard_missing(x)
  
  # Handle -1 specially: in most waves, -1 is "Not applicable" (-1)
  # but in wave 2 and 4, -1 is "Don't know" (-8)
  if (wave_num %in% c(2, 4)) {
    x[x == -1] <- -8  # "Don't know" in these waves
  } else {
    x[x == -1] <- -1  # "Not applicable" in other waves
  }
  
  # Convert NA to -3 (not asked / not interviewed)
  x[is.na(x)] <- -3
  
  return(x)
}

# Process each wave's sex variable
sex_processed <- list()
sex_processed[['W1sexYP']] <- process_sex_var('W1sexYP', 1)
sex_processed[['W2SexYP']] <- process_sex_var('W2SexYP', 2)
sex_processed[['W3sexYP']] <- process_sex_var('W3sexYP', 3)
sex_processed[['W4SexYP']] <- process_sex_var('W4SexYP', 4)
sex_processed[['W5SexYP']] <- process_sex_var('W5SexYP', 5)
sex_processed[['W6Sex']] <- process_sex_var('W6Sex', 6)
sex_processed[['W7Sex']] <- process_sex_var('W7Sex', 7)
sex_processed[['W8CMSEX']] <- process_sex_var('W8CMSEX', 8)
sex_processed[['W9DSEX']] <- process_sex_var('W9DSEX', 9)

# Consolidate sex variable using most-recent-valid-first (as per additional requirements for sex)
# Start from wave 9 (age 32) and work backwards
consolidated_sex <- rep(-3, nrow(df_merged))  # Initialize with "not asked" missing code

# Work from most recent to earliest
for (wave_num in 9:1) {
  if (wave_num == 1) {
    var_name <- 'W1sexYP'
  } else if (wave_num == 2) {
    var_name <- 'W2SexYP'
  } else if (wave_num == 3) {
    var_name <- 'W3sexYP'
  } else if (wave_num == 4) {
    var_name <- 'W4SexYP'
  } else if (wave_num == 5) {
    var_name <- 'W5SexYP'
  } else if (wave_num == 6) {
    var_name <- 'W6Sex'
  } else if (wave_num == 7) {
    var_name <- 'W7Sex'
  } else if (wave_num == 8) {
    var_name <- 'W8CMSEX'
  } else if (wave_num == 9) {
    var_name <- 'W9DSEX'
  }
  
  # Update only where consolidated_sex is still -3 (not yet assigned)
  # Valid values are 1 (Male) and 2 (Female)
  valid_mask <- consolidated_sex == -3
  valid_values <- sex_processed[[var_name]][valid_mask] %in% c(1, 2)
  consolidated_sex[valid_mask & valid_values] <- sex_processed[[var_name]][valid_mask & valid_values]
}

# Create the final sex variable as a labelled factor
sex_labelled <- factor(consolidated_sex, 
                       levels = c(1, 2, -9, -8, -1, -3),
                       labels = c('Male', 'Female', 'Refused', "Don't know", 'Not applicable', 'Not asked'))

# Add label for the variable
labelled::var_label(sex_labelled) <- 'Sex of cohort member (consolidated across all waves)'

# Create final output dataframe with NSID and sex
df_output <- df_merged %>%
  mutate(sex = sex_labelled) %>%
  select(NSID, sex)

cat('\nFinal output dimensions:', nrow(df_output), 'rows,', ncol(df_output), 'columns\n')

# Write the output CSV
write_csv(df_output, 'data/output/cleaned_data.csv')

cat('\nOutput written to data/output/cleaned_data.csv\n')

# Display summary statistics
cat('\nSex distribution:\n')
print(table(df_output$sex, useNA = 'always'))
