library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load data
data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))
names(data_list) <- files

# Merge datasets
full_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_data <- full_join(full_data, data_list[[i]], by = 'NSID')
}

# Define a helper function for missing value harmonisation based on metadata labels
harmonise_missing <- function(var, mapping) {
  res <- var
  for (old_val in names(mapping)) {
    res[var == as.numeric(old_val)] <- mapping[[old_val]]
  }
  res[is.na(res)] <- -3
  return(res)
}

# W1 Processing
# labels: -999: lost(-2), -94: insuff(-8), -92: refused(-9), -91: NA(-1), -1: DK(-8)
w1_map <- c('-999' = -2, '-94' = -8, '-92' = -9, '-91' = -1, '-1' = -8)
ethn_w1 <- harmonise_missing(full_data$W1ethnic2YP, w1_map)

# W2 Processing
# labels: -998: missed(-2), -997: script(-2), -995: unexplained(-2), -99: not interviewed(-3), -92: refused(-9), -91: NA(-1), -1: DK(-8)
w2_map <- c('-998' = -2, '-997' = -2, '-995' = -2, '-99' = -3, '-92' = -9, '-91' = -1, '-1' = -8)
ethn_w2 <- harmonise_missing(full_data$W2ethnicYP, w2_map)

# W4 Processing
# labels: -94: insuff(-8), -1: DK(-8)
w4_map <- c('-94' = -8, '-1' = -8)
ethn_w4 <- harmonise_missing(full_data$w4ethnic2YP, w4_map)

# W8 Processing
# labels: -9: refused(-9), -8: insuff(-8), -1: NA(-1)
w8_map <- c('-9' = -9, '-8' = -8, '-1' = -1)
ethn_w8 <- harmonise_missing(full_data$W8DETHN15, w8_map)

# W9 Processing
# labels: -8: insuff(-8)
w9_map <- c('-8' = -8)
ethn_w9 <- harmonise_missing(full_data$W9DETHN15, w9_map)

# Consolidated ethnicity: earliest valid positive response
# Valid substantive responses are 1-16
consolidated_ethn <- ethn_w1

# Helper to update consolidated if current is missing/non-substantive and new is substantive
update_consolidated <- function(current, new) {
  is_substantive <- function(x) x >= 1 & x <= 16
  res <- current
  idx <- !is_substantive(current) & is_substantive(new)
  res[idx] <- new[idx]
  return(res)
}

consolidated_ethn <- update_consolidated(consolidated_ethn, ethn_w2)
consolidated_ethn <- update_consolidated(consolidated_ethn, ethn_w4)
consolidated_ethn <- update_consolidated(consolidated_ethn, ethn_w8)
consolidated_ethn <- update_consolidated(consolidated_ethn, ethn_w9)

# If still not substantive, fill with the first available missing code (earliest wave)
# Since we already started with ethn_w1, we just need to ensure NAs are -3
consolidated_ethn[is.na(consolidated_ethn)] <- -3

# Create labels for ethnicity
ethn_labels <- c(
  '1' = 'White - British',
  '2' = 'White - Irish',
  '3' = 'Any other White background',
  '4' = 'Mixed - White and Black Caribbean',
  '5' = 'Mixed - White and Black African',
  '6' = 'Mixed - White and Asian',
  '7' = 'Any other mixed background',
  '8' = 'Indian',
  '9' = 'Pakistani',
  '10' = 'Bangladeshi',
  '11' = 'Any other Asian background',
  '12' = 'Black Caribbean',
  '13' = 'Black African',
  '14' = 'Any other Black background',
  '15' = 'Chinese',
  '16' = 'Any other ethnic background',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Final dataframe
final_df <- data.frame(NSID = full_data$NSID, ethnicity = consolidated_ethn)
final_df$ethnicity <- factor(final_df$ethnicity, levels = as.numeric(names(ethn_labels)), labels = ethn_labels)

write_csv(final_df, 'data/output/cleaned_data.csv')