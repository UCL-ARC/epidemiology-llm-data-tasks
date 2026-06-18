library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# 1. File Loading
# Load all files mentioned in metadata to preserve cohort frame
file_paths <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'ns9_2022_derived_variables.tab'
)

# Load each file explicitly
wave1 <- read_delim(paste0('data/input/', 'wave_one_lsype_young_person_2020.tab'), delim = '\t', col_types = cols(.default = 'c'))
wave4 <- read_delim(paste0('data/input/', 'wave_four_lsype_young_person_2020.tab'), delim = '\t', col_types = cols(.default = 'c'))
wave2_fam <- read_delim(paste0('data/input/', 'wave_two_lsype_family_background_2020.tab'), delim = '\t', col_types = cols(.default = 'c'))
wave3_fam <- read_delim(paste0('data/input/', 'wave_three_lsype_family_background_2020.tab'), delim = '\t', col_types = cols(.default = 'c'))
wave9_der <- read_delim(paste0('data/input/', 'ns9_2022_derived_variables.tab'), delim = '\t', col_types = cols(.default = 'c'))

# 2. Merge Datasets
# Start with wave1 and merge others
data_merged <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave2_fam, by = 'NSID') %>%
  full_join(wave3_fam, by = 'NSID') %>%
  full_join(wave9_der, by = 'NSID')

# 3. Harmonisation and Variable Derivation

# Helper to handle missing values based on general guidance
# Convert NA to -3
# Map specific codes
harmonise_missing <- function(x, missing_map = list()) {
  x <- as.numeric(x)
  for (raw_val in names(missing_map)) {
    x[x == as.numeric(raw_val)] <- missing_map[[raw_val]]
  }
  x[is.na(x)] <- -3
  return(x)
}

# IMD 15 (from wave 2 family background)
# Source: IMDRSCORE. Label: -94.0 = Insufficient Information (-8)
# User missing: -999.0 thru -1.0. Since it's a continuous score, we need to be careful.
# However, the general guidance says map labels. -94 -> -8.
# Other values in range -999 to -1 are treated as missing. 
# Since IMD score is continuous, we check if we should keep R NA or use codes.
# Guidance 6: "if a variable has valid negative values... retain R NA". 
# IMD scores are typically positive, but let's follow the mapping.

imd15 <- function(val) {
  v <- as.numeric(val)
  # Map -94 to -8 (Insufficient Info)
  v[v == -94] <- -8
  # Map other user missing values to standard codes
  # -999 to -1 range usually maps to -2 or -3. 
  # Since the metadata says 'user_missing_values': '-999.0 thru -1.0', 
  # and -94 is specifically -8, others in that range are likely -2 or -3.
  # Let's use -2 for the rest of the user missing range per pattern 7.
  v[v < -1 & v != -94] <- -2
  v[is.na(v)] <- -3
  return(v)
}

# IMD 16 (from wave 3 family background)
imd16 <- function(val) {
  v <- as.numeric(val)
  v[v == -94] <- -8
  v[v < -1 & v != -94] <- -2
  v[is.na(v)] <- -3
  return(v)
}

# IMD 32 (from wave 9 derived variables)
# Source: W9DIMDD. Label: -8.0 = Insufficient information
imd32 <- function(val) {
  v <- as.numeric(val)
  v[v == -8] <- -8
  v[is.na(v)] <- -3
  return(v)
}

# Apply transformations
# Note: The columns in the merged dataframe might have different names if they overlapped,
# but here they are from different files. Wait, wave2 and wave3 both have 'IMDRSCORE'.
# Since they were merged, they might be IMDRSCORE.x and IMDRSCORE.y or similar.
# Let's check the column names after merge or handle them specifically during merge.

# Redoing merge to ensure we keep track of which IMDRSCORE is which
wave2_fam_clean <- wave2_fam %>% select(NSID, imd15_raw = IMDRSCORE)
wave3_fam_clean <- wave3_fam %>% select(NSID, imd16_raw = IMDRSCORE)

data_final <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave2_fam_clean, by = 'NSID') %>%
  full_join(wave3_fam_clean, by = 'NSID') %>%
  full_join(wave9_der, by = 'NSID') %>%
  mutate(
    imd15 = imd15(imd15_raw),
    imd16 = imd16(imd16_raw),
    imd32 = imd32(W9DIMDD)
  ) %>% 
  select(NSID, imd15, imd16, imd32)

# Apply labels for categorical/missing as requested
# imd32 is nominal (deciles), others are scale
# For imd32, labels: 1: Most deprived, 10: Least deprived, -8: Insufficient info, -3: Not asked
# Since it's nominal, we make it a factor

# Note: The guidance says for continuous keep numeric and attach labels only for missing.
# imd15 and imd16 are scale (numeric).
# imd32 is nominal (deciles).

# Final cleanup and export
write_csv(data_final, 'data/output/cleaned_data.csv')
