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

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))) 
# Note: NSID is string, others numeric. Let's ensure NSID is treated as character.

# Actually, read_delim might fail if NSID is not the first. Let's use a more robust approach.
load_tab <- function(file) {
  read_delim(paste0('data/input/', file), delim = '\t', col_types = cols(.default = 'numeric')) %>% 
    mutate(NSID = as.character(NSID))
}

# Redoing the list loading to be safer
# Since the metadata says NSID is string, I'll read and then convert.

# Re-load based on the provided metadata filenames
df1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))
df9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t') %>% mutate(NSID = as.character(NSID))

# Merge datasets
full_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df5, by = 'NSID') %>%
  full_join(df6, by = 'NSID') %>%
  full_join(df7, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# Helper for missing values
standardize_missing <- function(x, wave_group) {
  # wave_group 1: waves 1-7, wave_group 2: waves 8-9
  res <- x
  # -1 mapping
  if(wave_group == 1) {
    res[x == -1] <- -8
  }
  
  # General mappings based on labels
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -99] <- -3
  res[x == -999] <- -2
  res[x == -998] <- -2
  res[x == -997] <- -2
  res[x == -995] <- -2
  
  res[is.na(res)] <- -3
  return(res)
}

# Collapsed mapping function
collapse_tenure <- function(val) {
  if (is.na(val) || val < 0) return(val)
  if (val %in% c(4, 5, 6)) return(4) # Rent it
  if (val == 8) return(6) # Other (for waves 1-4 detailed 8 -> collapsed 6)
  return(val)
}

# Process Waves 1-4
# Source: W1hous12HH, W2Hous12HH, W3hous12HH, W4Hous12HH
process_wave_early <- function(df, var_name, age_suffix) {
  raw_var <- df[[var_name]]
  clean_var <- standardize_missing(raw_var, 1)
  
  # detailed (hownteenXX)
  detailed <- clean_var
  
  # collapsed (hownXX)
  collapsed <- sapply(clean_var, function(v) {
    if(v < 0) return(v)
    if(v %in% c(4, 5, 6)) return(4)
    if(v == 8) return(6)
    return(v)
  })
  
  return(list(detailed = detailed, collapsed = collapsed))
}

# Wave 1
w1_res <- process_wave_early(full_df, 'W1hous12HH', '14')
full_df$hownteen14 <- w1_res$detailed
full_df$hown14 <- w1_res$collapsed

# Wave 2
w2_res <- process_wave_early(full_df, 'W2Hous12HH', '15')
full_df$hownteen15 <- w2_res$detailed
full_df$hown15 <- w2_res$collapsed

# Wave 3
w3_res <- process_wave_early(full_df, 'W3hous12HH', '16')
full_df$hownteen16 <- w3_res$detailed
full_df$hown16 <- w3_res$collapsed

# Wave 4
w4_res <- process_wave_early(full_df, 'W4Hous12HH', '17')
full_df$hownteen17 <- w4_res$detailed
full_df$hown17 <- w4_res$collapsed

# Process Waves 5-7
process_wave_mid <- function(df, type_var, own_sub_var, rent_sub_var, age_suffix) {
  type <- df[[type_var]]
  own_sub <- df[[own_sub_var]]
  rent_sub <- df[[rent_sub_var]]
  
  detailed <- numeric(length(type))
  collapsed <- numeric(length(type))
  
  for(i in 1:length(type)) {
    # Priority: owned-subtype before rented-subtype
    val_det <- NA
    val_col <- NA
    
    # Check owned
    os <- own_sub[i]
    if(!is.na(os) && os > 0) {
      if(os == 1) { val_det <- 1; val_col <- 1 }
      else if(os == 2) { val_det <- 2; val_col <- 2 }
      else if(os == 3) { val_det <- 3; val_col <- 3 }
      else if(os == 4) { val_det <- 8; val_col <- 6 }
    } else {
      # Check rented
      rs <- rent_sub[i]
      if(!is.na(rs) && rs > 0) {
        if(rs == 1) { val_det <- 4; val_col <- 4 }
        else if(rs == 2) { val_det <- 5; val_col <- 4 }
        else if(rs == 3) { val_det <- 6; val_col <- 4 }
        else if(rs == 4) { val_det <- 7; val_col <- 5 }
        else if(rs == 5) { val_det <- 8; val_col <- 6 }
      }
    }
    
    # Fallback to missing codes
    if(is.na(val_det)) {
      # Use the specific missing codes from subtype
      m_val <- own_sub[i]
      if(is.na(m_val)) m_val <- rent_sub[i]
      if(is.na(m_val)) m_val <- -3
      val_det <- standardize_missing(m_val, 1)
      val_col <- val_det
    }
    
    detailed[i] <- val_det
    collapsed[i] <- val_col
  }
  return(list(detailed = detailed, collapsed = collapsed))
}

# Wave 5
w5_res <- process_wave_mid(full_df, 'W5Hous12HH', 'W5Hous12BHH', 'W5Hous12CHH', '18')
full_df$hown18 <- w5_res$collapsed

# Wave 6
w6_res <- process_wave_mid(full_df, 'W6Hous12YP', 'W6Hous12bYP', 'W6Hous12cYP', '19')
full_df$hown19 <- w6_res$collapsed

# Wave 7
w7_res <- process_wave_mid(full_df, 'W7Hous12YP', 'W7Hous12bYP', 'W7Hous12cYP', '20')
full_df$hown20 <- w7_res$collapsed

# Process Waves 8-9
process_wave_late <- function(df, var_name, age_suffix) {
  raw_var <- df[[var_name]]
  clean_var <- standardize_missing(raw_var, 2)
  
  collapsed <- sapply(clean_var, function(v) {
    if(v < 0) return(v)
    if(v == 6 || v == 7) return(6) # Squatting (6) and Other (7) collapse to 6
    return(v)
  })
  return(collapsed)
}

full_df$hown26 <- process_wave_late(full_df, 'W8TENURE', '26')
full_df$hown32 <- process_wave_late(full_df, 'W9DTENURE', '32')

# Define labels for the collapsed variables (hownXX)
collapsed_labels <- c(
  "1" = "Own outright",
  "2" = "Own, buying with help of mortgage/loan",
  "3" = "Part rent, part mortgage (shared equity)",
  "4" = "Rent it",
  "5" = "Live rent-free, incl. relatives/friends",
  "6" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Define labels for detailed variables (hownteenXX)
detailed_labels <- c(
  "1" = "Owned outright",
  "2" = "Being bought on a mortgage or bank loan",
  "3" = "Shared ownership (owns & rents property)",
  "4" = "Rented from a Council or New Town",
  "5" = "Rented from a Housing Association",
  "6" = "Rented privately",
  "7" = "Rent free",
  "8" = "Some other arrangement",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Apply factors
hown_cols <- paste0('hown', c(14, 15, 16, 17, 18, 19, 20, 26, 32))
for(col in hown_cols) {
  full_df[[col]] <- factor(full_df[[col]], levels = names(collapsed_labels), labels = collapsed_labels)
}

hownteen_cols <- paste0('hownteen', c(14, 15, 16, 17))
for(col in hownteen_cols) {
  full_df[[col]] <- factor(full_df[[col]], levels = names(detailed_labels), labels = detailed_labels)
}

# Final selection
final_vars <- c('NSID', hownteen_cols, hown_cols)
final_df <- full_df %>% select(all_of(final_vars))

write_csv(final_df, 'data/output/cleaned_data.csv')
