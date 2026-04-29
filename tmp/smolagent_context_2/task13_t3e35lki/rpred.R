library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define paths
input_path <- 'data/input/'
output_path <- 'data/output/cleaned_data.csv'

# Files to load from metadata
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

# Load datasets
load_tab_with_nsid <- function(filename) {
  readr::read_delim(paste0(input_path, filename), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'numeric'))
}

df1 <- load_tab_with_nsid('wave_one_lsype_family_background_2020.tab')
df2 <- load_tab_with_nsid('wave_two_lsype_family_background_2020.tab')
df3 <- load_tab_with_nsid('wave_three_lsype_family_background_2020.tab')
df4 <- load_tab_with_nsid('wave_four_lsype_family_background_2020.tab')
df5 <- load_tab_with_nsid('wave_five_lsype_family_background_2020.tab')

# Merge datasets
full_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df5, by = 'NSID')

# NS-SEC labels for 1-17
nssec_labels <- c(
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial occupations',
  '3' = 'Higher professional',
  '4' = 'Lower professional',
  '5' = 'Lower managerial occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate clerical and administrative',
  '8' = 'Employers in small orgs',
  '9' = 'Own account workers',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical craft',
  '12' = 'Semi routine',
  '13' = 'Routine',
  '14' = 'Never worked/unemployed',
  '15' = 'Full-time students',
  '16' = 'Not classified or inadequately stated',
  '17' = 'Not classifiable for other reasons'
)

# Standard missing value labels (Escaping the single quote in "Don't")
missing_labels <- c(
  '-9' = 'Refusal',
  '-8' = "Don't know / insufficient information",
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Helper function to process NS-SEC variables
process_nssec <- function(var_name, df) {
  vals <- df[[var_name]]
  
  # Handle NA first
  vals[is.na(vals)] <- -3
  
  # Mapping missing codes
  # -98 -> -3 (Parent not present)
  vals[vals == -98] <- -3
  # -99 -> -3 (Default for missing/not interviewed)
  vals[vals == -99] <- -3
  # -94 -> -8 (Insufficient information)
  vals[vals == -94] <- -8
  # -999 -> -2 (Household data lost/Missing household info)
  vals[vals == -999] <- -2
  
  # Process substantive categories: take integer part of fractional codes
  # Only for values >= 1
  substantive_idx <- vals >= 1
  vals[substantive_idx] <- floor(vals[substantive_idx])
  
  # Ensure categories are within 1-17
  vals[vals > 17] <- 17
  
  # Create factor with labels
  all_labels <- c(missing_labels, nssec_labels)
  # Sort keys numerically for the factor levels
  levels_numeric <- as.numeric(names(all_labels))
  sorted_levels <- sort(levels_numeric)
  sorted_labels <- all_labels[as.character(sorted_levels)]
  
  # Map values to factor
  res <- factor(vals, levels = sorted_levels, labels = sorted_labels)
  return(res)
}

# Define source to target mapping
mapping <- list(
  nssecma14 = 'W1nsseccatmum',
  nssecpa14 = 'W1nsseccatdad',
  nssecma15 = 'W2nsseccatmum',
  nssecpa15 = 'W2nsseccatdad',
  nssecma16 = 'W3cnsseccatmum',
  nssecpa16 = 'W3cnsseccatdad',
  nssecma17 = 'w4cnsseccatmum',
  nssecpa17 = 'w4cnsseccatdad',
  nssecma18 = 'w5Cnsseccatmum',
  nssecpa18 = 'w5Cnsseccatdad'
)

# Apply processing
final_df <- full_df %>%
  select(NSID)

for (target in names(mapping)) {
  source <- mapping[[target]]
  final_df[[target]] <- process_nssec(source, full_df)
}

# Write output
write_csv(final_df, output_path)