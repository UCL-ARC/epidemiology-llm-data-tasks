library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

load_data <- function(fname) {
  readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(.default = 'c'))
}

df1 <- load_data('wave_one_lsype_young_person_2020.tab')
df2 <- load_data('wave_two_lsype_young_person_2020.tab')
df4 <- load_data('wave_four_lsype_young_person_2020.tab')
df8 <- load_data('ns8_2015_derived.tab')
df9 <- load_data('ns9_2022_derived_variables.tab')

# Merge datasets
merged_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# 2. Define Mapping for Missing Values
# Standard Missing-Value Codes:
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / not interviewed
# -2 = Schedule not applicable / script error / information lost
# -1 = Item not applicable

clean_ethnicity <- function(val, wave) {
  val <- as.numeric(val)
  if (is.na(val)) return(-3)
  
  if (wave == 'w1') {
    if (val == -999.0) return(-2) # Missing - household data lost
    if (val == -94.0) return(-8)  # Insufficient information
    if (val == -92.0) return(-9)  # Refused
    if (val == -91.0) return(-1)  # Not applicable
    if (val == -1.0) return(-8)   # Don't know
  } else if (wave == 'w2') {
    if (val == -998.0) return(-2) # Interviewer missed question
    if (val == -997.0) return(-2) # Script error
    if (val == -995.0) return(-2) # Missing history section data
    if (val == -99.0) return(-3)  # YP not interviewed
    if (val == -92.0) return(-9)  # Refused
    if (val == -91.0) return(-1)  # Not applicable
    if (val == -1.0) return(-8)   # Don't Know
  } else if (wave == 'w4') {
    if (val == -94.0) return(-8)  # Insufficient information
    if (val == -1.0) return(-8)   # Don't know
  } else if (wave == 'w8') {
    if (val == -9.0) return(-9)   # Refused
    if (val == -8.0) return(-8)  # Insufficient information
    if (val == -1.0) return(-1)   # Not applicable
  } else if (wave == 'w9') {
    if (val == -8.0) return(-8)   # Insufficient information
  }
  
  return(val)
}

# Apply cleaning to each wave source
merged_df <- merged_df %>%
  mutate(
    eth_w1 = sapply(W1ethnic2YP, clean_ethnicity, wave = 'w1'),
    eth_w2 = sapply(W2ethnicYP, clean_ethnicity, wave = 'w2'),
    eth_w4 = sapply(w4ethnic2YP, clean_ethnicity, wave = 'w4'),
    eth_w8 = sapply(W8DETHN15, clean_ethnicity, wave = 'w8'),
    eth_w9 = sapply(W9DETHN15, clean_ethnicity, wave = 'w9')
  )

# 3. Consolidation (Earliest-valid-first)
# Substantive responses are >= 1
get_consolidated_eth <- function(row) {
  vals <- c(row[['eth_w1']], row[['eth_w2']], row[['eth_w4']], row[['eth_w8']], row[['eth_w9']])
  # Find first substantive value
  for (v in vals) {
    if (!is.na(v) && v >= 1) return(v)
  }
  # Fall back to first missing code that isn't NA (if any)
  for (v in vals) {
    if (!is.na(v)) return(v)
  }
  return(-3)
}

merged_df$eth <- apply(merged_df[, c('eth_w1', 'eth_w2', 'eth_w4', 'eth_w8', 'eth_w9')], 1, get_consolidated_eth)

# 4. Factor Labels for 'eth'
eth_labels <- c(
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

merged_df$eth <- factor(merged_df$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

# Final output
final_df <- merged_df %>%
  select(NSID, eth)

readr::write_csv(final_df, 'data/output/cleaned_data.csv')
