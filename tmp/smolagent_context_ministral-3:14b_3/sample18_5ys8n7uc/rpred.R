
# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Define paths to input files
input_files <- list(
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_eight_self = 'data/input/ns8_2015_self_completion.tab',
  wave_eight_derived = 'data/input/ns8_2015_derived.tab',
  wave_nine = 'data/input/ns9_2022_main_interview.tab',
  wave_nine_derived = 'data/input/ns9_2022_derived_variables.tab'
)

# Define GHQ item variables
ghq_items <- list(
  wave_two = c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP',
               'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP',
               'W2wthlessYP', 'W2happyYP'),
  wave_four = c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP',
                'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP',
                'W4WthlessYP', 'W4HappyYP'),
  wave_eight = c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4', 'W8GHQ12_5',
                 'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8', 'W8GHQ12_9', 'W8GHQ12_10',
                 'W8GHQ12_11', 'W8GHQ12_12'),
  wave_nine = c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4', 'W9GHQ12_5',
                'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8', 'W9GHQ12_9', 'W9GHQ12_10',
                'W9GHQ12_11', 'W9GHQ12_12')
)

# Process wave two data
wave_two <- readr::read_delim(input_files$wave_two, delim = '\t')
wave_two_data <- wave_two %>%
  select(NSID, all_of(ghq_items$wave_two), W2ghq12scr) %>%
  mutate(
    ghq15 = case_when(
      W2ghq12scr %in% c(-96, -99) ~ -3,
      W2ghq12scr %in% c(-97, -92) ~ -9,
      is.na(W2ghq12scr) ~ -3,
      TRUE ~ W2ghq12scr
    ),
    ghqtl15 = {
      row_sums <- rowSums(select(., all_of(ghq_items$wave_two)), na.rm = TRUE)
      ifelse(all(is.na(row_sums)), -3,
             ifelse(any(row_sums < 0), -8, row_sums))
    }
  ) %>%
  select(NSID, ghq15, ghqtl15)

# Process wave four data
wave_four <- readr::read_delim(input_files$wave_four, delim = '\t')
wave_four_data <- wave_four %>%
  select(NSID, all_of(ghq_items$wave_four), W4ghq12scr) %>%
  mutate(
    ghq17 = case_when(
      W4ghq12scr %in% c(-96, -99) ~ -3,
      W4ghq12scr %in% c(-97, -92) ~ -9,
      is.na(W4ghq12scr) ~ -3,
      TRUE ~ W4ghq12scr
    ),
    ghqtl17 = {
      row_sums <- rowSums(select(., all_of(ghq_items$wave_four)), na.rm = TRUE)
      ifelse(all(is.na(row_sums)), -3,
             ifelse(any(row_sums < 0), -8, row_sums))
    }
  ) %>%
  select(NSID, ghq17, ghqtl17)

# Process wave eight data
wave_eight_self <- readr::read_delim(input_files$wave_eight_self, delim = '\t')
wave_eight_derived <- readr::read_delim(input_files$wave_eight_derived, delim = '\t')

wave_eight_data <- wave_eight_derived %>%
  select(NSID, W8DGHQSC) %>%
  mutate(ghq25 = ifelse(is.na(W8DGHQSC), -3, W8DGHQSC)) %>%
  select(NSID, ghq25)

wave_eight_self_data <- wave_eight_self %>%
  select(NSID, all_of(ghq_items$wave_eight)) %>%
  mutate(ghqtl25 = {
    row_sums <- rowSums(select(., all_of(ghq_items$wave_eight)), na.rm = TRUE)
    ifelse(all(is.na(row_sums)), -3,
           ifelse(any(row_sums < 0), -8, row_sums))
  }) %>%
  select(NSID, ghqtl25)

# Process wave nine data
wave_nine <- readr::read_delim(input_files$wave_nine, delim = '\t')
wave_nine_derived <- readr::read_delim(input_files$wave_nine_derived, delim = '\t')

wave_nine_data <- wave_nine_derived %>%
  select(NSID, W9DGHQSC) %>%
  mutate(ghq32 = ifelse(is.na(W9DGHQSC), -3, W9DGHQSC)) %>%
  select(NSID, ghq32)

wave_nine_self_data <- wave_nine %>%
  select(NSID, all_of(ghq_items$wave_nine)) %>%
  mutate(ghqtl32 = {
    row_sums <- rowSums(select(., all_of(ghq_items$wave_nine)), na.rm = TRUE)
    ifelse(all(is.na(row_sums)), -3,
           ifelse(any(row_sums < 0), -8, row_sums))
  }) %>%
  select(NSID, ghqtl32)

# Merge all datasets
final_data <- full_join(wave_two_data, wave_four_data, by = 'NSID') %>%
  full_join(wave_eight_data, by = 'NSID') %>%
  full_join(wave_eight_self_data, by = 'NSID') %>%
  full_join(wave_nine_data, by = 'NSID') %>%
  full_join(wave_nine_self_data, by = 'NSID')

# Convert to factors with appropriate levels
derived_vars <- c('ghq15', 'ghq17', 'ghq25', 'ghq32', 'ghqtl15', 'ghqtl17', 'ghqtl25', 'ghqtl32')
for (var in derived_vars) {
  if (var %in% names(final_data)) {
    final_data[[var]] <- factor(final_data[[var]],
                              levels = c(-9, -8, -3, -2, -1, 0:12),
                              levels = unique(final_data[[var]]))
  }
}

# Select only required variables
final_data <- final_data %>%
  select(NSID, all_of(derived_vars))

# Write final output file
write.csv(final_data, 'data/output/cleaned_data.csv', row.names = FALSE)
