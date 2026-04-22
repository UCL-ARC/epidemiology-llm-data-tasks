library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define missing value mapping function based on instructions
harmonize_missing <- function(val, wave) {
  if (is.na(val)) return(-3)
  
  if (wave == 'w1') {
    if (val == -999) return(-3)
    if (val == -94) return(-8)
    if (val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  }
  if (wave == 'w2') {
    if (val %in% c(-998, -997, -995, -99)) return(-3)
    if (val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  }
  if (wave == 'w4') {
    if (val == -94) return(-8)
    if (val == -1) return(-8)
  }
  if (wave == 'w8') {
    if (val == -9) return(-9)
    if (val == -8) return(-8)
    if (val == -1) return(-1)
  }
  if (wave == 'w9') {
    if (val == -8) return(-8)
  }
  
  return(val)
}

files <- c(
  'w1' = 'wave_one_lsype_young_person_2020.tab',
  'w2' = 'wave_two_lsype_young_person_2020.tab',
  'w4' = 'wave_four_lsype_young_person_2020.tab',
  'w8' = 'ns8_2015_derived.tab',
  'w9' = 'ns9_2022_derived_variables.tab'
)

load_and_clean <- function(wave_id, filename) {
  # Explicitly specify NSID as character to avoid join issues if types differ
  df <- read_delim(paste0('data/input/', filename), delim = "\t", col_types = cols(NSID = col_character(), .default = 'd'))
  
  var_name <- switch(wave_id,
                    'w1' = 'W1ethnic2YP',
                    'w2' = 'W2ethnicYP',
                    'w4' = 'w4ethnic2YP',
                    'w8' = 'W8DETHN15',
                    'w9' = 'W9DETHN15')
  
  df_subset <- df %>%
    select(NSID, !!sym(var_name))
  
  # Handle potential duplicates in NSID within a single wave to prevent cartesian product
  df_subset <- df_subset %>%
    distinct(NSID, .keep_all = TRUE)

  df_subset[[var_name]] <- map_dbl(df_subset[[var_name]], ~harmonize_missing(.x, wave_id))
  
  age_suffix <- switch(wave_id,
                      'w1' = '14',
                      'w2' = '15',
                      'w4' = '17',
                      'w8' = '25',
                      'w9' = '32')
  
  colnames(df_subset)[2] <- paste0('eth', age_suffix)
  
  return(df_subset)
}

data_list <- map2(names(files), files, load_and_clean)

# Merge datasets
final_df <- reduce(data_list, full_join, by = 'NSID')

eth_cols <- c('eth14', 'eth15', 'eth17', 'eth25', 'eth32')

get_earliest_valid <- function(row) {
  vals <- row[eth_cols]
  pos_indices <- which(vals > 0)
  if (length(pos_indices) > 0) {
    return(vals[pos_indices[1]])
  } else {
    # Fallback to the first available value (missing code or NA)
    vals[is.na(vals)] <- -3
    return(vals[1])
  }
}

# Apply row-wise function
final_df$eth <- apply(final_df[, eth_cols, drop = FALSE], 1, get_earliest_valid)

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
  '-8' = 'Don\'t know/insufficient information',
  '-1' = 'Item not applicable',
  '-3' = 'Not asked/interviewed',
  '-2' = 'Schedule not applicable'
)

final_df$eth <- factor(final_df$eth, levels = names(eth_labels), labels = eth_labels)

output_df <- final_df %>%
  select(NSID, eth)

write_csv(output_df, 'data/output/cleaned_data.csv')