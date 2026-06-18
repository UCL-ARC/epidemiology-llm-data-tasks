library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define files and variables
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

# Loading and merging
data_list <- lapply(files, function(f) {
  readr::read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(.default = 'numeric')) %>% 
    # We need NSID as string, but read_delim might guess numeric if it looks like one. 
    # However, the metadata says NSID is string. Let's force it.
    # Since we used col_types = 'numeric', we should actually specify NSID separately.
    # Let's refine the loading.
    return(.)
})

# Re-loading properly to ensure NSID is character
load_tab <- function(f) {
  df <- readr::read_delim(paste0('data/input/', f), delim = '\t')
  df <- df %>% mutate(NSID = as.character(NSID))
  return(df)
}

df1 <- load_tab('wave_one_lsype_family_background_2020.tab')
df2 <- load_tab('wave_two_lsype_family_background_2020.tab')
df3 <- load_tab('wave_three_lsype_family_background_2020.tab')
df4 <- load_tab('wave_four_lsype_family_background_2020.tab')
df5 <- load_tab('wave_five_lsype_family_background_2020.tab')

full_cohort <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df5, by = 'NSID')

# Labels mapping for NS-SEC 17 categories
# We take the integer part of the fractional codes
nssec_labels <- c(
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial occupations',
  '3' = 'Higher professional',
  '4' = 'Lower professional',
  '5' = 'Lower managerial occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate',
  '8' = 'Employers in small orgs',
  '9' = 'Own account workers',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical craft',
  '12' = 'Semi routine',
  '13' = 'Routine',
  '14' = 'Never worked/Unemployed',
  '15' = 'Full-time students',
  '16' = 'Not classified or inadequately stated',
  '17' = 'Not classifiable for other reasons'
)

# Processing function
process_nssec <- function(var_name, output_name) {
  # Use the variable from the dataframe
  vals <- full_cohort[[var_name]]
  
  # Convert R NA to -3 (not asked/interviewed)
  vals[is.na(vals)] <- -3
  
  # Specific missing value mapping
  # -98 ("Parent not present") -> -3
  vals[vals == -98] <- -3
  
  # Other missing value patterns from general guidance
  # -999, -99, etc. -> -2 or -3
  # Based on metadata: -999 (lost) -> -2, -99 (not interviewed) -> -3, -94 (insufficient) -> -8
  vals[vals == -999] <- -2
  vals[vals == -99] <- -3
  vals[vals == -94] <- -8
  
  # Substantive codes: Collapse fractional to integer
  # Only apply to values >= 1
  res <- rep(NA, length(vals))
  substantive_idx <- which(vals >= 1)
  res[substantive_idx] <- floor(vals[substantive_idx])
  
  # Keep the missing codes
  missing_idx <- which(vals < 1)
  res[missing_idx] <- vals[missing_idx]
  
  # Create factor with labels
  # We need to maintain the order of 1-17 then missing codes
  final_vals <- as.numeric(res)
  
  # Create labels for the factor
  # Valid: 1-17
  # Missing: -1, -2, -3, -7, -8, -9
  all_levels <- c(-9, -8, -7, -3, -2, -1, 1:17)
  
  # Map actual values to these levels
  # Any other negative value not explicitly handled? 
  # General guidance: -99 -> -3, -92 -> -9, etc. 
  # But metadata labels are primary. Let's refine the missing map again.
  
  return(final_vals)
}

# Apply to all 10 variables
vars_map <- list(
  c('W1nsseccatmum', 'nssecma14'), c('W1nsseccatdad', 'nssecpa14'),
  c('W2nsseccatmum', 'nssecma15'), c('W2nsseccatdad', 'nssecpa15'),
  c('W3cnsseccatmum', 'nssecma16'), c('W3cnsseccatdad', 'nssecpa16'),
  c('w4cnsseccatmum', 'nssecma17'), c('w4cnsseccatdad', 'nssecpa17'),
  c('w5Cnsseccatmum', 'nssecma18'), c('w5Cnsseccatdad', 'nssecpa18')
)

final_df <- full_cohort %>% select(NSID)

for (pair in vars_map) {
  src <- pair[1]
  out <- pair[2]
  
  # Extract values
  v <- full_cohort[[src]]
  
  # Missing mapping
  # 1. R NA -> -3
  v[is.na(v)] <- -3
  # 2. -98 -> -3
  v[v == -98] <- -3
  # 3. -999 -> -2
  v[v == -999] <- -2
  # 4. -99 -> -3
  v[v == -99] <- -3
  # 5. -94 -> -8
  v[v == -94] <- -8
  
  # Substantive: floor
  v_final <- rep(NA, length(v))
  sub_idx <- which(v >= 1)
  v_final[sub_idx] <- floor(v[sub_idx])
  
  # Restore missing codes
  mis_idx <- which(v < 1)
  v_final[mis_idx] <- v[mis_idx]
  
  # Ensure they are numeric for now, then we can label
  final_df[[out]] <- as.numeric(v_final)
}

# Final Labeling for all nssec variables
# According to Guidance 10: create labelled factors
# Labels for 1-17 and missing codes
missing_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

full_labels <- c(
  missing_labels, 
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional",
  "4" = "Lower professional",
  "5" = "Lower managerial occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate",
  "8" = "Employers in small orgs",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical craft",
  "12" = "Semi routine",
  "13" = "Routine",
  "14" = "Never worked/Unemployed",
  "15" = "Full-time students",
  "16" = "Not classified or inadequately stated",
  "17" = "Not classifiable for other reasons"
)

# Apply labels
for (col in names(final_df)) {
  if (col != 'NSID') {
    final_df[[col]] <- factor(final_df[[col]], levels = as.numeric(names(full_labels)), labels = full_labels)
  }
}

readr::write_csv(final_df, 'data/output/cleaned_data.csv')
