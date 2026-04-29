library(haven)
library(dplyr)
library(purrr)
library(labelled)

# Define input files
files <- list(
  wave_14 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_15 = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_17 = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_25_items = 'data/input/ns8_2015_self_completion.tab',
  wave_25_derived = 'data/input/ns8_2015_derived.tab',
  wave_32_items = 'data/input/ns9_2022_main_interview.tab',
  wave_32_derived = 'data/input/ns9_2022_derived_variables.tab'
)

# Load each file and select ID + GHQ variables
wave14 <- readr::read_delim(files$wave_14, delim = '\t')

wave15 <- readr::read_delim(files$wave_15, delim = '\t') %>%
  dplyr::select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, 
                W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP,
                W2noconfYP, W2wthlessYP, W2happyYP, W2ghq12scr)

wave17 <- readr::read_delim(files$wave_17, delim = '\t') %>%
  dplyr::select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP,
                W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP,
                W4NoConfYP, W4WthlessYP, W4HappyYP, W4ghq12scr)

wave25_items <- readr::read_delim(files$wave_25_items, delim = '\t') %>%
  dplyr::select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4,
                W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8,
                W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)

wave25_derived <- readr::read_delim(files$wave_25_derived, delim = '\t') %>%
  dplyr::select(NSID, W8DGHQSC)

wave32_items <- readr::read_delim(files$wave_32_items, delim = '\t') %>%
  dplyr::select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4,
                W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8,
                W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)

wave32_derived <- readr::read_delim(files$wave_32_derived, delim = '\t') %>%
  dplyr::select(NSID, W9DGHQSC)

# Harmonize pre-derived scores for wave 2 and 4 (earlier sweeps)
wave15 <- wave15 %>%
  dplyr::mutate(ghq15 = case_when(
    W2ghq12scr == -96 | W2ghq12scr == -99 ~ -3,
    W2ghq12scr == -97 | W2ghq12scr == -92 ~ -9,
    is.na(W2ghq12scr) ~ -3,
    TRUE ~ W2ghq12scr
  ))

wave17 <- wave17 %>%
  dplyr::mutate(ghq17 = case_when(
    W4ghq12scr == -96 | W4ghq12scr == -99 ~ -3,
    W4ghq12scr == -97 | W4ghq12scr == -92 ~ -9,
    is.na(W4ghq12scr) ~ -3,
    TRUE ~ W4ghq12scr
  ))

# Harmonize pre-derived scores for wave 8 and 9 (later sweeps)
wave25_derived <- wave25_derived %>%
  dplyr::mutate(ghq25 = case_when(
    is.na(W8DGHQSC) ~ -3,
    TRUE ~ W8DGHQSC
  ))

wave32_derived <- wave32_derived %>%
  dplyr::mutate(ghq32 = case_when(
    is.na(W9DGHQSC) ~ -3,
    TRUE ~ W9DGHQSC
  ))

# Compute GHQ-12 sum scores
compute_ghqtl <- function(data, item_vars) {
  data %>%
    dplyr::mutate(
      .ghqtl = case_when(
        dplyr::if_all(all_of(item_vars), ~ is.na(.x)) ~ -3,
        dplyr::if_any(all_of(item_vars), ~ .x < 0) ~ -8,
        TRUE ~ rowSums(across(all_of(item_vars)), na.rm = TRUE)
      )
    )
}

wave15 <- compute_ghqtl(wave15, c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 
                                   'W2decideYP', 'W2strainYP', 'W2difficYP', 
                                   'W2activYP', 'W2probsYP', 'W2depressYP',
                                   'W2noconfYP', 'W2wthlessYP', 'W2happyYP')) %>%
  dplyr::mutate(ghqtl15 = .ghqtl)

wave17 <- compute_ghqtl(wave17, c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 
                                   'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 
                                   'W4ActivYP', 'W4ProbsYP', 'W4DepressYP',
                                   'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')) %>%
  dplyr::mutate(ghqtl17 = .ghqtl)

wave25_items <- compute_ghqtl(wave25_items, c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4',
                                               'W8GHQ12_5', 'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8',
                                               'W8GHQ12_9', 'W8GHQ12_10', 'W8GHQ12_11', 'W8GHQ12_12')) %>%
  dplyr::mutate(ghqtl25 = .ghqtl)

wave32_items <- compute_ghqtl(wave32_items, c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4',
                                               'W9GHQ12_5', 'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8',
                                               'W9GHQ12_9', 'W9GHQ12_10', 'W9GHQ12_11', 'W9GHQ12_12')) %>%
  dplyr::mutate(ghqtl32 = .ghqtl)

# Merge all datasets
final_data <- dplyr::full_join(wave15, wave17, by = 'NSID') %>%
  dplyr::full_join(wave25_items, by = 'NSID') %>%
  dplyr::full_join(wave25_derived, by = 'NSID') %>%
  dplyr::full_join(wave32_items, by = 'NSID') %>%
  dplyr::full_join(wave32_derived, by = 'NSID')

# Define ghq variables
gqh_vars <- c('ghq15', 'ghq17', 'ghq25', 'ghq32', 'ghqtl15', 'ghqtl17', 'ghqtl25', 'ghqtl32')

# Create label map with character keys
lbl_map <- c('-1' = 'Item not applicable',
             '-2' = 'Script error/information lost',
             '-3' = 'Not asked at the fieldwork stage/participated/interviewed',
             '-8' = 'Don\'t know/insufficient information',
             '-9' = 'Refusal')

# Convert each variable to labeled factor
for (var in gqh_vars) {
  # Convert to integer
  final_data[[var]] <- as.integer(final_data[[var]])
  # Convert to factor with proper levels
  uniq_vals <- unique(final_data[[var]])
  final_data[[var]] <- factor(final_data[[var]], levels = as.integer(uniq_vals))
  # Add labels
  attr(final_data[[var]], 'labels') <- lbl_map
}

# Keep only required variables
final_data <- dplyr::select(final_data, NSID, all_of(gqh_vars))

# Write output using readr
readr::write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Output file created successfully!\n')
cat('Rows:', nrow(final_data), '\n')
cat('Columns:', names(final_data), '\n')