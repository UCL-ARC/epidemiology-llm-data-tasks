library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

# Load datasets
load_data <- function(fname) {
  read_delim(paste0('data/input/', fname), delim = '\t', col_types = cols(.default = "numeric"), trim_ws = TRUE) %>% 
    mutate(NSID = as.character(NSID))
}

# Since NSID is string, we need to ensure read_delim handles it correctly
# Overriding the load function to handle NSID as character
load_data_fixed <- function(fname) {
  df <- read_delim(paste0('data/input/', fname), delim = '\t', col_types = cols(NSID = col_character(), .default = col_double()), trim_ws = TRUE)
  return(df)
}

data1 <- load_data_fixed('wave_one_lsype_family_background_2020.tab')
data2 <- load_data_fixed('wave_two_lsype_family_background_2020.tab')
data3 <- load_data_fixed('wave_three_lsype_family_background_2020.tab')
data4 <- load_data_fixed('wave_four_lsype_family_background_2020.tab')
data5 <- load_data_fixed('wave_five_lsype_family_background_2020.tab')

# Merge datasets
full_df <- data1 %>%
  full_join(data2, by = 'NSID') %>%
  full_join(data3, by = 'NSID') %>%
  full_join(data4, by = 'NSID') %>%
  full_join(data5, by = 'NSID')

# Define standard labels
nssec_labels <- c(
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial occupations',
  '3' = 'Higher professional',
  '4' = 'Lower professional',
  '5' = 'Lower managerial occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate occupations',
  '8' = 'Small employers',
  '9' = 'Own account workers',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical craft',
  '12' = 'Semi routine',
  '13' = 'Routine',
  '14' = 'Never worked / Long-term unemployed',
  '15' = 'Full-time students',
  '16' = 'Not classified or inadequately stated',
  '17' = 'Not classifiable for other reasons'
)

missing_labels <- c(
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked / not interviewed / not present',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Processing function
process_nssec <- function(var_name, df) {
  vec <- df[[var_name]]
  
  # 1. Handle Wave-specific missing mapping: -98 -> -3
  vec[vec == -98] <- -3
  
  # 2. Handle Standard Missing-Value Codes from metadata
  # Based on rules: -999, -99, -94, etc.
  # -999.0 (household data lost) -> -2
  # -99.0 (not interviewed) -> -3
  # -94.0 (insufficient info) -> -8
  # -91.0 -> -1, -92.0 -> -9
  
  vec[vec == -999] <- -2
  vec[vec == -99] <- -3
  vec[vec == -94] <- -8
  vec[vec == -92] <- -9
  vec[vec == -91] <- -1
  
  # 3. Convert NA to -3
  vec[is.na(vec)] <- -3
  
  # 4. Collapse fractional codes to integer part for substantive values (1-17)
  # Only apply to values >= 1
  substantive_idx <- vec >= 1
  vec[substantive_idx] <- floor(vec[substantive_idx])
  
  # 5. Ensure any remaining non-standard negatives are mapped to -3 if not specified
  # This is a safety catch for other negative values not explicitly handled
  # but we should be careful not to overwrite our mapped -1, -2, -3, -8, -9
  
  # Create factor with labels
  all_labels <- c(nssec_labels, missing_labels)
  vec_fact <- factor(vec, levels = as.numeric(names(all_labels)), labels = all_labels)
  
  return(vec_fact)
}

# Apply processing
final_df <- full_df %>%
  mutate(
    nssecma14 = process_nssec('W1nsseccatmum', full_df),
    nssecpa14 = process_nssec('W1nsseccatdad', full_df),
    nssecma15 = process_nssec('W2nsseccatmum', full_df),
    nssecpa15 = process_nssec('W2nsseccatdad', full_df),
    nssecma16 = process_nssec('W3cnsseccatmum', full_df),
    nssecpa16 = process_nssec('W3cnsseccatdad', full_df),
    nssecma17 = process_nssec('w4cnsseccatmum', full_df),
    nssecpa17 = process_nssec('w4cnsseccatdad', full_df),
    nssecma18 = process_nssec('w5Cnsseccatmum', full_df),
    nssecpa18 = process_nssec('w5Cnsseccatdad', full_df)
  ) %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

write_csv(final_df, 'data/output/cleaned_data.csv')