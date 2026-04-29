library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file_paths <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

data_list <- map(file_paths, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols()))
names(data_list) <- file_paths
full_df <- data_list %>% reduce(full_join, by = 'NSID')

# 2. Harmonisation Logic
harmonise_missing <- function(x, mapping) {
  res <- as.numeric(x)
  for (orig in names(mapping)) {
    res[x == as.numeric(orig)] <- mapping[[orig]]
  }
  res[is.na(res)] <- -3
  return(res)
}

# Age 19 (W6MarStatYP)
map_w6 <- c('-997' = -2, '-97' = -7, '-92' = -9, '-91' = -1, '-1' = -8)
partnr19_raw <- harmonise_missing(full_df$W6MarStatYP, map_w6)

# Age 25 (W8DMARSTAT)
map_w8 <- c('-9' = -9, '-8' = -8, '-1' = -1)
partnradu25_raw <- harmonise_missing(full_df$W8DMARSTAT, map_w8)

# Age 32 (W9DMARSTAT)
map_w9 <- c('-9' = -9, '-8' = -8)
partnradu32_raw <- harmonise_missing(full_df$W9DMARSTAT, map_w9)

# Collapsed variables (partnrXX)
# Age 19
partnr19_coll <- partnr19_raw

# Age 25
partnr25_coll <- partnradu25_raw
partnr25_coll[partnradu25_raw == 6] <- 2
partnr25_coll[partnradu25_raw == 7] <- 3
partnr25_coll[partnradu25_raw == 8] <- 4
partnr25_coll[partnradu25_raw == 9] <- 5

# Age 32
partnr32_coll <- partnradu32_raw
partnr32_coll[partnradu32_raw == 3] <- 4
partnr32_coll[partnradu32_raw == 4] <- 3
partnr32_coll[partnradu32_raw == 6] <- 2
partnr32_coll[partnradu32_raw == 7] <- 4
partnr32_coll[partnradu32_raw == 8] <- 5

# Labels
coll_labels <- c(
  '1' = 'Single/Never Married/CP',
  '2' = 'Married/CP',
  '3' = 'Separated',
  '4' = 'Divorced/Former CP',
  '5' = 'Widowed/Surviving CP',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Schedule NA/Error',
  '-1' = 'Not applicable'
)

labels_adu25 <- c(
  '1' = 'Single and never married or in a CP',
  '2' = 'Married',
  '3' = 'Separated but still legally married',
  '4' = 'Divorced',
  '5' = 'Widowed',
  '6' = 'A Civil Partner',
  '7' = 'Separated but still legally in a CP',
  '8' = 'A former Civil Partner',
  '9' = 'A surviving Civil Partner',
  '-9' = 'Refusal',
  '-8' = 'Insufficient information',
  '-1' = 'Not applicable',
  '-3' = 'Not asked'
)

labels_adu32 <- c(
  '1' = 'Single that is never married or never in a Civil Partnership',
  '2' = 'Married',
  '3' = 'Divorced',
  '4' = 'Legally separated',
  '5' = 'Widowed',
  '6' = 'A Civil Partner in a legally recognised Civil Partnership',
  '7' = 'A former Civil Partner (where Civil Partnership legally dissolved)',
  '8' = 'A surviving Civil Partner (where Civil Partner has died)',
  '-9' = 'Refusal',
  '-8' = 'Insufficient information',
  '-3' = 'Not asked'
)

# Final Data Frame
final_df <- data.frame(NSID = full_df$NSID)
final_df$partnr19 <- partnr19_coll
final_df$partnr25 <- partnr25_coll
final_df$partnr32 <- partnr32_coll
final_df$partnradu25 <- partnradu25_raw
final_df$partnradu32 <- partnradu32_raw

# Correct function for labelled package is val_labels()
final_df$partnr19 <- val_labels(final_df$partnr19, coll_labels)
final_df$partnr25 <- val_labels(final_df$partnr25, coll_labels)
final_df$partnr32 <- val_labels(final_df$partnr32, coll_labels)
final_df$partnradu25 <- val_labels(final_df$partnradu25, labels_adu25)
final_df$partnradu32 <- val_labels(final_df$partnradu32, labels_adu32)

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')