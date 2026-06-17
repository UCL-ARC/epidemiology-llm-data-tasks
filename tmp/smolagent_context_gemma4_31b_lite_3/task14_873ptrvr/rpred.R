library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
load_tab_with_nsid <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'numeric'))
}

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

all_data <- list()
for (f in files) {
  all_data[[f]] <- load_tab_with_nsid(f)
}

merged_df <- all_data[[1]]
for (i in 2:length(all_data)) {
  merged_df <- full_join(merged_df, all_data[[i]], by = 'NSID')
}

map_missing <- function(val, mapping) {
  res <- rep(-3, length(val))
  for (code in names(mapping)) {
    res[val == as.numeric(code)] <- mapping[[code]]
  }
  res[is.na(val)] <- -3
  return(res)
}

process_tenure_split <- function(df, var_main, var_owned, var_rented) {
  missing_map <- list('-999.0' = -2, '-998.0' = -2, '-997.0' = -2, '-995.0' = -2, '-99.0' = -2, '-92.0' = -9, '-91.0' = -1, '-1.0' = -8)
  
  val_main <- df[[var_main]]
  val_own <- df[[var_owned]]
  val_rent <- df[[var_rented]]
  
  detailed <- rep(-3, length(val_main))
  
  detailed[val_main == 1 & val_own == 1] <- 1
  detailed[val_main == 1 & val_own == 2] <- 2
  detailed[val_main == 1 & val_own == 3] <- 3
  detailed[val_main == 1 & val_own == 4] <- 7
  
  detailed[val_main == 2 & val_rent == 1] <- 4
  detailed[val_main == 2 & val_rent == 2] <- 4
  detailed[val_main == 2 & val_rent == 3] <- 4
  detailed[val_main == 2 & val_rent == 4] <- 5
  detailed[val_main == 2 & val_rent == 5] <- 7
  
  detailed[val_main == 3] <- 7
  
  m_codes <- map_missing(val_main, missing_map)
  detailed[m_codes != -3] <- m_codes[m_codes != -3]
  
  return(detailed)
}

final_df <- data.frame(NSID = merged_df$NSID)

# Ages 14, 15, 16, 17
waves_early <- list(
  list(age = 14, var = 'W1hous12HH'),
  list(age = 15, var = 'W2Hous12HH'),
  list(age = 16, var = 'W3hous12HH'),
  list(age = 17, var = 'W4Hous12HH')
)

for (w in waves_early) {
  val <- merged_df[[w$var]]
  m_codes <- map_missing(val, list('-999.0'=-2, '-998.0'=-2, '-997.0'=-2, '-995.0'=-2, '-99.0'=-2, '-92.0'=-9, '-91.0'=-1, '-1.0'=-8))
  
  detailed <- rep(-3, length(val))
  detailed[val == 1] <- 1
  detailed[val == 2] <- 2
  detailed[val == 3] <- 3
  detailed[val == 4] <- 4
  detailed[val == 5] <- 4
  detailed[val == 6] <- 4
  detailed[val == 7] <- 5
  detailed[val == 8] <- 7
  detailed[m_codes != -3] <- m_codes[m_codes != -3]
  
  final_df[[paste0('hownteen', w$age)]] <- detailed
  final_df[[paste0('hown', w$age)]] <- detailed
}

# Ages 18, 19, 20
waves_mid <- list(
  list(age = 18, var = 'W5Hous12HH', own = 'W5Hous12BHH', rent = 'W5Hous12CHH'),
  list(age = 19, var = 'W6Hous12YP', own = 'W6Hous12bYP', rent = 'W6Hous12cYP'),
  list(age = 20, var = 'W7Hous12YP', own = 'W7Hous12bYP', rent = 'W7Hous12cYP')
)

for (w in waves_mid) {
  detailed <- process_tenure_split(merged_df, w$var, w$own, w$rent)
  final_df[[paste0('hownteen', w$age)]] <- detailed
  final_df[[paste0('hown', w$age)]] <- detailed
}

# Age 25
val8 <- merged_df[['W8TENURE']]
m_codes8 <- map_missing(val8, list('-9.0'=-9, '-8.0'=-8, '-1.0'=-1))
detailed8 <- rep(-3, length(val8))
detailed8[val8 == 1] <- 1
detailed8[val8 == 2] <- 2
detailed8[val8 == 3] <- 3
detailed8[val8 == 4] <- 4
detailed8[val8 == 5] <- 5
detailed8[val8 == 6] <- 6
detailed8[val8 == 7] <- 7
detailed8[m_codes8 != -3] <- m_codes8[m_codes8 != -3]
final_df[['hown25']] <- detailed8

# Age 32
val9 <- merged_df[['W9DTENURE']]
m_codes9 <- map_missing(val9, list('-8.0'=-8))
detailed9 <- rep(-3, length(val9))
detailed9[val9 == 1] <- 1
detailed9[val9 == 2] <- 2
detailed9[val9 == 3] <- 3
detailed9[val9 == 4] <- 4
detailed9[val9 == 5] <- 5
detailed9[val9 == 6] <- 6
detailed9[val9 == 7] <- 7
detailed9[m_codes9 != -3] <- m_codes9[m_codes9 != -3]
final_df[['hown32']] <- detailed9

labels_tenure <- c(
  '1' = 'Own outright',
  '2' = 'Own, buying with help of mortgage/loan',
  '3' = 'Part rent, part mortgage (shared equity)',
  '4' = 'Rent it',
  '5' = 'Live rent-free, incl. relatives/friends',
  '6' = 'Squatting',
  '7' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

for (col in names(final_df)) {
  if (grepl('^hown', col)) {
    final_df[[col]] <- factor(final_df[[col]], levels = as.numeric(names(labels_tenure)), labels = labels_tenure)
  }
}

readr::write_csv(final_df, 'data/output/cleaned_data.csv')
