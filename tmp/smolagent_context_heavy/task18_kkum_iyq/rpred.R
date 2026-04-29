library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

load_data <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
}

# Load files explicitly
data1 <- load_data('wave_one_lsype_young_person_2020.tab')
data2 <- load_data('wave_two_lsype_young_person_2020.tab')
data4 <- load_data('wave_four_lsype_young_person_2020.tab')
data8_sc <- load_data('ns8_2015_self_completion.tab')
data8_der <- load_data('ns8_2015_derived.tab')
data9_main <- load_data('ns9_2022_main_interview.tab')
data9_der <- load_data('ns9_2022_derived_variables.tab')

# Merge datasets
full_frame <- data1 %>%
  full_join(data2, by = 'NSID') %>%
  full_join(data4, by = 'NSID') %>%
  full_join(data8_sc, by = 'NSID') %>%
  full_join(data8_der, by = 'NSID') %>%
  full_join(data9_main, by = 'NSID') %>%
  full_join(data9_der, by = 'NSID')

# Helper for missing value harmonisation for pre-derived scores
harmonise_pre_derived <- function(x) {
  x <- as.numeric(x)
  res <- rep(-3, length(x))
  
  # Handle substantive values first
  valid_idx <- !is.na(x) & x >= 0
  res[valid_idx] <- x[valid_idx]
  
  # Map specific negative codes
  # -97 (YP refused self completion) and -92 (Refused) map to -9
  res[!is.na(x) & (x == -97 | x == -92)] <- -9
  # -99 (YP not interviewed) map to -3
  res[!is.na(x) & x == -99] <- -3
  # -96 (YP using interpreter) map to -2
  res[!is.na(x) & x == -96] <- -2
  # -91 map to -1
  res[!is.na(x) & x == -91] <- -1
  # -1 map to -1
  res[!is.na(x) & x == -1] <- -1
  # -8 map to -8
  res[!is.na(x) & x == -8] <- -8
  
  # Remaining NAs already default to -3
  return(res)
}

# GHQ Item-Summation Function
calculate_ghq_sum <- function(df, vars) {
  items <- df %>% select(all_of(vars))
  
  all_na <- apply(items, 1, function(row) all(is.na(row)))
  any_neg <- apply(items, 1, function(row) any(!is.na(row) & row < 0))
  row_sums <- rowSums(items, na.rm = TRUE)
  
  final_score <- rep(-3, nrow(df))
  
  # Case 1: All 12 items are NA -> -3
  final_score[all_na] <- -3
  # Case 2: Any item has a negative value -> -8
  final_score[any_neg] <- -8
  # Case 3: Otherwise -> sum
  otherwise_idx <- !all_na & !any_neg
  final_score[otherwise_idx] <- row_sums[otherwise_idx]
  
  return(final_score)
}

# Wave 15 (Wave 2)
vars15 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')
full_frame$ghqtl15 <- calculate_ghq_sum(full_frame, vars15)
full_frame$ghq15 <- harmonise_pre_derived(full_frame$W2ghq12scr)

# Wave 17 (Wave 4)
vars17 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')
full_frame$ghqtl17 <- calculate_ghq_sum(full_frame, vars17)
full_frame$ghq17 <- harmonise_pre_derived(full_frame$W4ghq12scr)

# Wave 25 (Wave 8)
vars25 <- c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4', 'W8GHQ12_5', 'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8', 'W8GHQ12_9', 'W8GHQ12_10', 'W8GHQ12_11', 'W8GHQ12_12')
full_frame$ghqtl25 <- calculate_ghq_sum(full_frame, vars25)
full_frame$ghq25 <- harmonise_pre_derived(full_frame$W8DGHQSC)

# Wave 32 (Wave 9)
vars32 <- c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4', 'W9GHQ12_5', 'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8', 'W9GHQ12_9', 'W9GHQ12_10', 'W9GHQ12_11', 'W9GHQ12_12')
full_frame$ghqtl32 <- calculate_ghq_sum(full_frame, vars32)
full_frame$ghq32 <- harmonise_pre_derived(full_frame$W9DGHQSC)

# Final selection
final_df <- full_frame %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

write_csv(final_df, 'data/output/cleaned_data.csv')
