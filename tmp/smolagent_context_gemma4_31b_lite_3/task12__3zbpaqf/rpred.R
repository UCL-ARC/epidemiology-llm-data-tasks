library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the data files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab'
)

load_data <- function(filename) {
  path <- paste0('data/input/', filename)
  if (!file.exists(path)) return(NULL)
  
  df <- read_delim(path, delim = '\t', show_col_types = FALSE)
  names(df) <- trimws(names(df))
  
  if ('NSID' %in% names(df)) {
    df <- df %>%
      mutate(NSID = as.character(NSID))
  }
  
  return(df)
}

all_data_list <- map(files, load_data)
valid_data <- keep(all_data_list, ~ !is.null(.x) && 'NSID' %in% names(.x))

if (length(valid_data) == 0) {
  stop('No valid data files with NSID found')
}

cohort_frame <- valid_data[[1]]
if (length(valid_data) > 1) {
  for (i in 2:length(valid_data)) {
    cohort_frame <- full_join(cohort_frame, valid_data[[i]], by = 'NSID')
  }
}

# Process variables to major NS-SEC categories
process_var_num <- function(source_var, wave_id) {
  if (!(source_var %in% names(cohort_frame))) {
    return(rep(-3, nrow(cohort_frame)))
  }
  
  vals <- cohort_frame[[source_var]]
  
  # Use & instead of && for vectorization in case_when
  cleaned <- case_when(
    is.na(vals) ~ -3,
    (wave_id == 'W4' & vals == -99) ~ -3,
    (wave_id == 'W8' & vals == -9) ~ -9,
    (wave_id == 'W8' & vals == -8) ~ -8,
    (wave_id == 'W8' & vals == -1) ~ -1,
    (wave_id == 'W9' & vals == -1) ~ -1,
    (wave_id == 'W9' & vals >= -9 & vals <= -1) ~ -3,
    vals == -91 ~ -1,
    vals < 0 ~ -3,
    TRUE ~ floor(vals)
  )
  return(cleaned)
}

final_df <- data.frame(NSID = cohort_frame$NSID)
final_df$nssec17 <- process_var_num('W4nsseccatYP', 'W4')
final_df$nssec18 <- process_var_num('W5nsseccatYP', 'W5')
final_df$nssec19 <- process_var_num('w6nsseccatYP', 'W6')
final_df$nssec20 <- process_var_num('W7NSSECCat', 'W7')
final_df$nssec25 <- process_var_num('W8DNSSEC17', 'W8')
final_df$nssec32 <- process_var_num('W9NSSEC', 'W9')

write_csv(final_df, 'data/output/cleaned_data.csv')
