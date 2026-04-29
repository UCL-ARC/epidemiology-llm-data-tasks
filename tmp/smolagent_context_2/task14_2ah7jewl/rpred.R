library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

load_file <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric'))
}

# Note: NSID is string, but read_delim numeric default might fail. 
# Let's be more specific for NSID
load_file_fixed <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'))
}

data_list <- map(files, load_file_fixed)
names(data_list) <- files

full_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# Standard Missing Mapping function
map_missing <- function(val, wave_group) {
  # wave_group: 1 (1-7), 2 (8-9)
  res <- case_when(
    is.na(val) ~ -3,
    val == -92 ~ -9,
    val == -91 ~ -1,
    val == -99 ~ -3,
    val == -999 ~ -2,
    val == -998 ~ -2,
    val == -997 ~ -2,
    val == -995 ~ -2,
    val == -1 & wave_group == 1 ~ -8, # Special requirement
    val == -1 & wave_group == 2 ~ -1, # Standard
    TRUE ~ val
  )
  return(res)
}

# Detailed scheme (8 categories)
# 1: Owned outright, 2: Mortgage, 3: Shared, 4: Council, 5: HA, 6: Private, 7: Rent free, 8: Other

# Collapsed scheme (6 categories)
# 1: Owned outright, 2: Mortgage, 3: Shared, 4: Rent it (Council/HA/Private), 5: Rent free, 6: Other

collapse_tenure <- function(detailed_val) {
  case_when(
    detailed_val == 1 ~ 1,
    detailed_val == 2 ~ 2,
    detailed_val == 3 ~ 3,
    detailed_val %in% c(4, 5, 6) ~ 4,
    detailed_val == 7 ~ 5,
    detailed_val == 8 ~ 6,
    TRUE ~ detailed_val
  )
}

# Processing Waves
# Wave 1-4
waves_1_4 <- list(c('W1hous12HH', 14), c('W2Hous12HH', 15), c('W3hous12HH', 16), c('W4Hous12HH', 17))

# Wave 5-7 complex logic
process_w5_7 <- function(df, type_var, own_var, rent_var, age) {
  detailed <- apply(df[, c(type_var, own_var, rent_var)], 1, function(row) {
    type <- as.numeric(row[1])
    own <- as.numeric(row[2])
    rent <- as.numeric(row[3])
    
    # Substantive paths
    # Owned subtype mapping: 1->1, 2->2, 3->3, 4->8
    if (!is.na(own) && own >= 1 && own <= 3) return(own)
    if (!is.na(own) && own == 4) return(8)
    
    # Rented subtype mapping: 1->4, 2->5, 3->6, 4->7, 5->8
    if (!is.na(rent) && rent >= 1 && rent <= 4) return(rent + 3)
    if (!is.na(rent) && rent == 5) return(8)
    
    # Type 'Something else' mapping
    if (!is.na(type) && type == 3) return(8)
    
    # Missing codes priority: subtype then type
    m_own <- map_missing(own, 1)
    m_rent <- map_missing(rent, 1)
    m_type <- map_missing(type, 1)
    
    if (!is.na(m_own) && m_own < 0) return(m_own)
    if (!is.na(m_rent) && m_rent < 0) return(m_rent)
    return(m_type)
  })
  return(detailed)
}

# Apply logic to all waves

# W1-4
for (w in waves_1_4) {
  var_name <- w[1]
  age <- w[2]
  full_df[[paste0('hownteen', age)]] <- map_missing(full_df[[var_name]], 1)
  # Correct substantive codes for detailed’s range (1-8)
  # Metadata for W1-4 shows 1-8 are substantive
  # Now create collapsed
  full_df[[paste0('hown', age)]] <- collapse_tenure(full_df[[paste0('hownteen', age)]])
}

# W5-7
w5_7_vars <- list(
  list(type='W5Hous12HH', own='W5Hous12BHH', rent='W5Hous12CHH', age=18),
  list(type='W6Hous12YP', own='W6Hous12bYP', rent='W6Hous12cYP', age=19),
  list(type='W7Hous12YP', own='W7Hous12bYP', rent='W7Hous12cYP', age=20)
)

for (w in w5_7_vars) {
  detailed_vals <- process_w5_7(full_df, w$type, w$own, w$rent, w$age)
  full_df[[paste0('hownteen', w$age)]] <- detailed_vals
  full_df[[paste0('hown', w$age)]] <- collapse_tenure(detailed_vals)
}

# W8-9
# W8: 1->1, 2->2, 3->3, 4->4, 5->5, 6->6, 7->7. Collapsed: 6,7 -> 6
# W9: 1->1, 2->2, 3->3, 4->4, 5->5, 6->6, 7->7. Collapsed: 6,7 -> 6

# Wave 8
full_df$hown25 <- map_missing(full_df$W8TENURE, 2)
full_df$hown25 <- case_when(full_df$hown25 %in% c(6, 7) ~ 6, TRUE ~ full_df$hown25)

# Wave 9
full_df$hown32 <- map_missing(full_df$W9DTENURE, 2)
full_df$hown32 <- case_when(full_df$hown32 %in% c(6, 7) ~ 6, TRUE ~ full_df$hown32)

# Final selection
final_cols <- c('NSID')
for(a in 14:20) final_cols <- c(final_cols, paste0('hownteen', a))
for(a in c(14:20, 25, 32)) final_cols <- c(final_cols, paste0('hown', a))

final_df <- full_df[, final_cols]

# Labeling
detailed_labels <- c(
  '1' = 'Owned outright',
  '2' = 'Being bought on a mortgage/bank loan',
  '3' = 'Shared ownership',
  '4' = 'Rented from a Council or New Town',
  '5' = 'Rented from a Housing Association',
  '6' = 'Rented privately',
  '7' = 'Rent free',
  '8' = 'Some other arrangement',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Script error',
  '-1' = 'Not applicable'
)

collapsed_labels <- c(
  '1' = 'Owned outright',
  '2' = 'Being bought on a mortgage/bank loan',
  '3' = 'Shared ownership',
  '4' = 'Rent it',
  '5' = 'Rent free',
  '6' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Script error',
  '-1' = 'Not applicable'
)

for(col in names(final_df)) {
  if(grepl('hownteen', col)) {
    final_df[[col]] <- factor(final_df[[col]], levels = as.numeric(names(detailed_labels)), labels = detailed_labels)
  } else if(grepl('hown', col)) {
    final_df[[col]] <- factor(final_df[[col]], levels = as.numeric(names(collapsed_labels)), labels = collapsed_labels)
  }
}

write_csv(final_df, 'data/output/cleaned_data.csv')
