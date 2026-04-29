library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# To avoid memory limits and join overflow, we load only necessary columns
# and ensure NSID is treated as a character to avoid precision issues during join
files_meta <- list(
  wave1 = list(file = 'data/input/wave_one_lsype_young_person_2020.tab', cols = c('NSID')),
  wave4 = list(file = 'data/input/wave_four_lsype_young_person_2020.tab', cols = c('NSID', 'W4empsYP')),
  wave5 = list(file = 'data/input/wave_five_lsype_young_person_2020.tab', cols = c('NSID', 'W5mainactYP')),
  wave6 = list(file = 'data/input/wave_six_lsype_young_person_2020.tab', cols = c('NSID', 'W6TCurrentAct')),
  wave7 = list(file = 'data/input/wave_seven_lsype_young_person_2020.tab', cols = c('NSID', 'W7TCurrentAct')),
  wave8 = list(file = 'data/input/ns8_2015_derived.tab', cols = c('NSID', 'W8DACTIVITYC')),
  wave9 = list(file = 'data/input/ns9_2022_derived_variables.tab', cols = c('NSID', 'W9DACTIVITYC'))
)

load_subset <- function(meta) {
  df <- read_delim(meta$file, delim = "\t", col_select = all_of(meta$cols), col_types = cols(NSID = "c", .default = "d"))
  return(df)
}

data_list <- map(files_meta, load_subset)

# Merge using full_join by NSID
# Using a loop instead of reduce to be more explicit and handle potential duplicates if any
merged_data <- data_list[[1]]
for(i in 2:length(data_list)) {
  merged_data <- full_join(merged_data, data_list[[i]], by = "NSID")
}

# Harmonize Missing Values function
harmonize_missing <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -999 | x == -998 | x == -997 | x == -995 ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    TRUE ~ x
  )
}

# Recoding logic
merged_data <- merged_data %>%
  mutate(
    # Age 14
    ecoact14 = -3,
    
    # Age 17
    ecoact17_raw = harmonize_missing(W4empsYP),
    ecoact17 = case_when(
      ecoact17_raw == 1 | ecoact17_raw == 2 ~ 1, 
      ecoact17_raw == 4 | ecoact17_raw == 5 ~ 2, 
      ecoact17_raw == 3 ~ 3, 
      ecoact17_raw == 6 ~ 4, 
      ecoact17_raw == 7 | ecoact17_raw == 8 | ecoact17_raw == 9 ~ 5, 
      ecoact17_raw <= 0 ~ ecoact17_raw, 
      TRUE ~ 5
    ),
    
    # Age 18 (Detailed)
    ecoactadu18_raw = harmonize_missing(W5mainactYP),
    ecoactadu18 = case_when(
      ecoactadu18_raw <= 0 ~ ecoactadu18_raw, 
      TRUE ~ ecoactadu18_raw
    ),
    ecoact18 = case_when(
      ecoactadu18_raw == 3 ~ 1, 
      ecoactadu18_raw == 1 | ecoactadu18_raw == 2 | ecoactadu18_raw == 4 | ecoactadu18_raw == 5 | ecoactadu18_raw == 6 ~ 2, 
      ecoactadu18_raw == 7 ~ 3, 
      ecoactadu18_raw == 8 ~ 4, 
      ecoactadu18_raw == 9 | ecoactadu18_raw == 10 | ecoactadu18_raw == 11 ~ 5, 
      ecoactadu18_raw <= 0 ~ ecoactadu18_raw,
      TRUE ~ 5
    ),
    
    # Age 19
    ecoact19_raw = harmonize_missing(W6TCurrentAct),
    ecoact19 = case_when(
      ecoact19_raw == 3 ~ 1, 
      ecoact19_raw == 1 | ecoact19_raw == 2 | ecoact19_raw == 4 | ecoact19_raw == 5 | ecoact19_raw == 10 ~ 2, 
      ecoact19_raw == 8 ~ 3, 
      ecoact19_raw == 7 ~ 4, 
      ecoact19_raw == 6 | ecoact19_raw == 9 | ecoact19_raw == 11 ~ 5, 
      ecoact19_raw <= 0 ~ ecoact19_raw,
      TRUE ~ 5
    ),
    
    # Age 20
    ecoact20_raw = harmonize_missing(W7TCurrentAct),
    ecoact20 = case_when(
      ecoact20_raw == 3 | ecoact20_raw == 9 ~ 1, 
      ecoact20_raw == 1 | ecoact20_raw == 2 | ecoact20_raw == 4 | ecoact20_raw == 5 ~ 2, 
      ecoact20_raw == 8 ~ 3, 
      ecoact20_raw == 7 ~ 4, 
      ecoact20_raw == 6 | ecoact20_raw == 10 | ecoact20_raw == 11 | ecoact20_raw == 12 | ecoact20_raw == 13 | ecoact20_raw == 14 | ecoact20_raw == 15 ~ 5, 
      ecoact20_raw <= 0 ~ ecoact20_raw,
      TRUE ~ 5
    ),
    
    # Age 25 (Detailed)
    ecoactadu25_raw = harmonize_missing(W8DACTIVITYC),
    ecoactadu25 = case_when(
      ecoactadu25_raw <= 0 ~ ecoactadu25_raw,
      TRUE ~ ecoactadu25_raw
    ),
    ecoact25 = case_when(
      ecoactadu25_raw == 1 | ecoactadu25_raw == 2 ~ 1, 
      ecoactadu25_raw == 5 | ecoactadu25_raw == 6 | ecoactadu25_raw == 7 ~ 2, 
      ecoactadu25_raw == 4 ~ 3, 
      ecoactadu25_raw == 9 ~ 4, 
      ecoactadu25_raw == 3 | ecoactadu25_raw == 8 | ecoactadu25_raw == 10 ~ 5, 
      ecoactadu25_raw <= 0 ~ ecoactadu25_raw,
      TRUE ~ 5
    ),
    
    # Age 32 (Detailed)
    ecoactadu32_raw = harmonize_missing(W9DACTIVITYC),
    ecoactadu32 = case_when(
      ecoactadu32_raw <= 0 ~ ecoactadu32_raw,
      TRUE ~ ecoactadu32_raw
    ),
    ecoact32 = case_when(
      ecoactadu32_raw == 1 | ecoactadu32_raw == 2 ~ 1, 
      ecoactadu32_raw == 5 | ecoactadu32_raw == 6 | ecoactadu32_raw == 7 ~ 2, 
      ecoactadu32_raw == 4 ~ 3, 
      ecoactadu32_raw == 9 ~ 4, 
      ecoactadu32_raw == 3 | ecoactadu32_raw == 8 | ecoactadu32_raw == 10 ~ 5, 
      ecoactadu32_raw <= 0 ~ ecoactadu32_raw,
      TRUE ~ 5
    )
  )

# Convert to factors
final_vars <- c("ecoact14", "ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32", "ecoactadu18", "ecoactadu25", "ecoactadu32")
merged_data[final_vars] <- lapply(merged_data[final_vars], as.factor)

# Final Select
final_output <- merged_data %>%
  select(NSID, all_of(final_vars))

# Write to CSV
write_csv(final_output, "data/output/cleaned_data.csv")
