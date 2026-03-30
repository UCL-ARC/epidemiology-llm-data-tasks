
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing value codes
standard_missing_codes <- list(
  refusal = -9,
  dont_know = -8,
  not_applicable = -1,
  not_asked = -3
)

# Load and merge datasets
file_names <- c(
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

# Load each file and merge by NSID
data_list <- map(file_names, ~ {
  file_path <- paste0('data/input/', .x)
  read_delim(file_path, delim = '\t', col_types = cols(NSID = col_character()))
})

# Merge datasets
merged_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  merged_data <- full_join(merged_data, data_list[[i]], by = 'NSID')
}

# Function to harmonize missing values
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999 | x == -998 | x == -997 | x == -995 | x == -99] <- standard_missing_codes$not_asked
  x[x == -92] <- standard_missing_codes$refusal
  x[x == -91] <- standard_missing_codes$not_applicable
  x[x == -8] <- standard_missing_codes$dont_know
  x[x == -1] <- standard_missing_codes$not_applicable
  x
}

# Create consolidated sex variable (time-invariant)
sex_vars <- c(
  'W1sexYP', 'W2SexYP', 'W3sexYP', 'W4SexYP', 'W5SexYP', 'W6Sex', 'W7Sex',
  'W8CMSEX', 'W9DSEX'
)

# Prioritize the most recent valid response for sex
consolidated_sex <- function(df) {
  sex_priority <- c('W9DSEX', 'W8CMSEX', 'W7Sex', 'W6Sex', 'W5SexYP',
                    'W4SexYP', 'W3sexYP', 'W2SexYP', 'W1sexYP')

  # Apply harmonize_missing to each sex variable
  for (var in sex_priority) {
    if (var %in% names(df)) {
      df[[var]] <- harmonize_missing(df[[var]])
    }
  }

  # Create a new variable for consolidated sex
  df$sex <- NA_real_

  for (i in 1:nrow(df)) {
    for (var in sex_priority) {
      if (!is.na(df[[var]][i]) && df[[var]][i] > 0) {
        df$sex[i] <- df[[var]][i]
        break
      }
    }
    if (is.na(df$sex[i])) {
      df$sex[i] <- standard_missing_codes$not_asked
    }
  }

  # Create factor variable with labels
  labels_sex <- c('Refusal', "Don't know", 'Not asked', 'Not applicable', 'Male', 'Female')
  levels_sex <- c(-9, -8, -3, -1, 1, 2)

  df$sex <- factor(df$sex, levels = levels_sex, labels = labels_sex)
  return(df)
}

# Apply the function to create consolidated sex variable
merged_data <- consolidated_sex(merged_data)

# Select only the ID and derived variables for output
output_vars <- c('NSID', 'sex')

# Write output to CSV
write_csv(merged_data[, output_vars], 'data/output/cleaned_data.csv')

# Print confirmation
message('Data cleaning and preprocessing complete. Output saved to data/output/cleaned_data.csv.')
