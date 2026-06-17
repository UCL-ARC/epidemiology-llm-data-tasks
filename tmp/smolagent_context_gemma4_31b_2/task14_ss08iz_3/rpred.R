library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
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

load_data <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
}

data_list <- map(files, load_data)

# Merge all datasets - Fix: Use the data frames in the list, not the names
full_df <- data_list %>% 
  reduce(full_join, by = 'NSID')

# Harmonisation function for Sweeps 1-4
process_early_wave <- function(var_name) {
  detailed <- full_df[[var_name]]
  detailed <- case_when(
    detailed == -999.0 ~ -2,
    detailed == -997.0 ~ -2,
    detailed == -99.0 ~ -2,
    detailed == -92.0 ~ -9,
    detailed == -91.0 ~ -1,
    detailed == -1.0 ~ -8,
    TRUE ~ detailed
  )
  
  collapsed <- case_when(
    detailed %in% c(4, 5, 6) ~ 4,
    detailed == 8 ~ 6,
    TRUE ~ detailed
  )
  
  return(list(detailed = detailed, collapsed = collapsed))
}

res14 <- process_early_wave('W1hous12HH')
res15 <- process_early_wave('W2Hous12HH')
res16 <- process_early_wave('W3hous12HH')
res17 <- process_early_wave('W4Hous12HH')

# Sweeps 5-7 processing
process_mid_wave <- function(type_var, own_var, rent_var) {
  own <- full_df[[own_var]]
  rent <- full_df[[rent_var]]
  
  clean_sub <- function(x) {
    case_when(
      x == -999.0 ~ -2,
      x == -92.0 ~ -9,
      x == -91.0 ~ -1,
      x == -1.0 ~ -8,
      TRUE ~ x
    )
  }
  
  own_c <- clean_sub(own)
  rent_c <- clean_sub(rent)
  
  # Detailed 8-cat: 1:Owned outright, 2:Mortgage, 3:Shared, 4:Council, 5:Assoc, 6:Private, 7:Rent free, 8:Other
  val_detailed_fixed <- case_when(
    own_c == 4 | rent_c == 5 ~ 8,
    own_c >= 1 & own_c <= 3 ~ own_c,
    rent_c >= 1 & rent_c <= 4 ~ rent_c + 3,
    TRUE ~ -3
  )
  
  final_detailed <- case_when(
    val_detailed_fixed >= 1 & val_detailed_fixed <= 8 ~ val_detailed_fixed,
    own_c < 0 ~ own_c,
    rent_c < 0 ~ rent_c,
    TRUE ~ -3
  )
  
  final_collapsed <- case_when(
    final_detailed >= 1 & final_detailed <= 3 ~ final_detailed,
    final_detailed %in% c(4, 5, 6) ~ 4,
    final_detailed == 7 ~ 5,
    final_detailed == 8 ~ 6,
    TRUE ~ final_detailed
  )
  
  return(list(detailed = final_detailed, collapsed = final_collapsed))
}

res18 <- process_mid_wave('W5Hous12HH', 'W5Hous12BHH', 'W5Hous12CHH')
res19 <- process_mid_wave('W6Hous12YP', 'W6Hous12bYP', 'W6Hous12cYP')
res20 <- process_mid_wave('W7Hous12YP', 'W7Hous12bYP', 'W7Hous12cYP')

# Sweeps 8-9 processing
process_late_wave <- function(var_name) {
  val <- full_df[[var_name]]
  val_clean <- case_when(
    var_name == 'W8TENURE' & val == -9.0 ~ -9,
    var_name == 'W8TENURE' & val == -8.0 ~ -8,
    var_name == 'W8TENURE' & val == -1.0 ~ -1,
    var_name == 'W9DTENURE' & val == -8.0 ~ -8,
    val < 0 ~ val,
    TRUE ~ val
  )
  
  collapsed <- case_when(
    val_clean >= 1 & val_clean <= 5 ~ val_clean,
    val_clean == 6 ~ 6,
    val_clean == 7 ~ 6,
    TRUE ~ val_clean
  )
  
  return(collapsed)
}

res25 <- process_late_wave('W8TENURE')
res32 <- process_late_wave('W9DTENURE')

# Combine all
final_df <- full_df %>%
  select(NSID) %>%
  mutate(
    hownteen14 = res14$detailed, hown14 = res14$collapsed,
    hownteen15 = res15$detailed, hown15 = res15$collapsed,
    hownteen16 = res16$detailed, hown16 = res16$collapsed,
    hownteen17 = res17$detailed, hown17 = res17$collapsed,
    hownteen18 = res18$detailed, hown18 = res18$collapsed,
    hownteen19 = res19$detailed, hown19 = res19$collapsed,
    hownteen20 = res20$detailed, hown20 = res20$collapsed,
    hown25 = res25,
    hown32 = res32
  )

write_csv(final_df, 'data/output/cleaned_data.csv')
