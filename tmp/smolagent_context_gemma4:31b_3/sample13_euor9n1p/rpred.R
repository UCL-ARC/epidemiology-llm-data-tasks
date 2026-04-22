library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# The error suggests NSID is being read as numeric, and potentially
# there are parsing issues leading to NA or duplicate NSIDs.
# We must ensure NSID is read as character to avoid precision loss
# and check for duplicates before joining.

load_and_select <- function(file_name, mut_var, fat_var, mut_new, fat_new) {
  path <- paste0('data/input/', file_name)
  # Read NSID as character, others as numeric
  df <- read_delim(path, delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'))
  
  # Select and rename
  df <- df %>% select(NSID, !!sym(mut_var), !!sym(fat_var))
  df <- df %>% rename(!!sym(mut_new) := !!sym(mut_var), !!sym(fat_new) := !!sym(fat_var))
  
  # Remove duplicates of NSID if any to prevent cartesian product during join
  df <- df %>% distinct(NSID, .keep_all = TRUE)
  
  return(df)
}

df1 <- load_and_select('wave_one_lsype_family_background_2020.tab', 'W1nsseccatmum', 'W1nsseccatdad', 'nssecma14', 'nssecpa14')
df2 <- load_and_select('wave_two_lsype_family_background_2020.tab', 'W2nsseccatmum', 'W2nsseccatdad', 'nssecma15', 'nssecpa15')
df3 <- load_and_select('wave_three_lsype_family_background_2020.tab', 'W3cnsseccatmum', 'W3cnsseccatdad', 'nssecma16', 'nssecpa16')
df4 <- load_and_select('wave_four_lsype_family_background_2020.tab', 'w4cnsseccatmum', 'w4cnsseccatdad', 'nssecma17', 'nssecpa17')
df5 <- load_and_select('wave_five_lsype_family_background_2020.tab', 'w5Cnsseccatmum', 'w5Cnsseccatdad', 'nssecma18', 'nssecpa18')

# Merge datasets
final_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df5, by = 'NSID')

# Harmonization function
harmonize_nssec <- function(x) {
  res <- x
  # Missing value codes harmonization
  res[x == -999] <- -2
  res[x == -94] <- -8
  res[x == -99 | x == -98 | is.na(x)] <- -3
  
  # Valid range: 1-17. Collapse subcategories (integer part)
  valid_mask <- res >= 1
  res[valid_mask] <- floor(res[valid_mask])
  
  # Valid occupational classes are 1-17
  # Standard missing codes: -9, -8, -7, -3, -2, -1
  missing_codes <- c(-9, -8, -7, -3, -2, -1)
  invalid_mask <- !(res %in% 1:17) & !(res %in% missing_codes)
  res[invalid_mask] <- -3
  
  return(res)
}

nssec_vars <- c('nssecma14', 'nssecpa14', 'nssecma15', 'nssecpa15', 'nssecma16', 'nssecpa16', 'nssecma17', 'nssecpa17', 'nssecma18', 'nssecpa18')

final_df <- final_df %>% 
  mutate(across(all_of(nssec_vars), harmonize_nssec))

# Factor Labels
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
  '14' = 'Never worked/Long-term unemployed',
  '15' = 'Full-time students',
  '16' = 'Not classified or inadequately stated',
  '17' = 'Not classifiable for other reasons',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked/not interviewed',
  '-2' = 'Script error/information lost',
  '-1' = 'Not applicable'
)

final_df <- final_df %>%
  mutate(across(all_of(nssec_vars), ~ factor(.x, levels = as.numeric(names(nssec_labels)), labels = nssec_labels)))

# Final selection and output
final_df <- final_df %>% select(NSID, all_of(nssec_vars))
write_csv(final_df, 'data/output/cleaned_data.csv')
