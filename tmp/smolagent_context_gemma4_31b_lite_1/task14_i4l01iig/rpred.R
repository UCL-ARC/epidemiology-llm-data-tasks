library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
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

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))
names(data_list) <- files

# Merge datasets
full_df <- data_list %>% reduce(full_join, by = 'NSID')

# Helper function for missing values mapping
map_missing <- function(val) {
  case_when(
    is.na(val) ~ -3,
    val == -92 ~ -9,
    val == -91 ~ -1,
    val == -1 ~ -8,
    val %in% c(-999, -998, -997, -995, -99) ~ -2,
    TRUE ~ val
  )
}

# Age 14 (Wave 1)
full_df <- full_df %>%
  mutate(
    hownteen14 = map_missing(W1hous12HH),
    hown14 = case_when(
      hownteen14 == 1 ~ 1,
      hownteen14 == 2 ~ 2,
      hownteen14 == 3 ~ 3,
      hownteen14 %in% c(4, 5, 6) ~ 4, 
      hownteen14 == 7 ~ 5, 
      hownteen14 == 8 ~ 7, 
      TRUE ~ hownteen14
    )
  )

# Age 15 (Wave 2)
full_df <- full_df %>%
  mutate(
    hownteen15 = map_missing(W2Hous12HH),
    hown15 = case_when(
      hownteen15 == 1 ~ 1,
      hownteen15 == 2 ~ 2,
      hownteen15 == 3 ~ 3,
      hownteen15 %in% c(4, 5, 6) ~ 4,
      hownteen15 == 7 ~ 5,
      hownteen15 == 8 ~ 7,
      TRUE ~ hownteen15
    )
  )

# Age 16 (Wave 3)
full_df <- full_df %>%
  mutate(
    hownteen16 = map_missing(W3hous12HH),
    hown16 = case_when(
      hownteen16 == 1 ~ 1,
      hownteen16 == 2 ~ 2,
      hownteen16 == 3 ~ 3,
      hownteen16 %in% c(4, 5, 6) ~ 4,
      hownteen16 == 7 ~ 5,
      hownteen16 == 8 ~ 7,
      TRUE ~ hownteen16
    )
  )

# Age 17 (Wave 4)
full_df <- full_df %>%
  mutate(
    hownteen17 = map_missing(W4Hous12HH),
    hown17 = case_when(
      hownteen17 == 1 ~ 1,
      hownteen17 == 2 ~ 2,
      hownteen17 == 3 ~ 3,
      hownteen17 %in% c(4, 5, 6) ~ 4,
      hownteen17 == 7 ~ 5,
      hownteen17 == 8 ~ 7,
      TRUE ~ hownteen17
    )
  )

# Age 18 (Wave 5)
full_df <- full_df %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH == 1 ~ case_when(
        W5Hous12BHH == 1 ~ 1, W5Hous12BHH == 2 ~ 2, W5Hous12BHH == 3 ~ 3, W5Hous12BHH == 4 ~ 7, TRUE ~ map_missing(W5Hous12BHH)
      ),
      W5Hous12HH == 2 ~ case_when(
        W5Hous12CHH %in% c(1, 2, 3) ~ 4, W5Hous12CHH == 4 ~ 5, W5Hous12CHH == 5 ~ 7, TRUE ~ map_missing(W5Hous12CHH)
      ),
      W5Hous12HH == 3 ~ 7,
      TRUE ~ map_missing(W5Hous12HH)
    ),
    hown18 = case_when(
      hownteen18 == 1 ~ 1,
      hownteen18 == 2 ~ 2,
      hownteen18 == 3 ~ 3,
      hownteen18 == 4 ~ 4,
      hownteen18 == 5 ~ 5,
      hownteen18 == 7 ~ 7,
      TRUE ~ hownteen18
    )
  )

# Age 19 (Wave 6)
full_df <- full_df %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12YP == 1 ~ case_when(
        W6Hous12bYP == 1 ~ 1, W6Hous12bYP == 2 ~ 2, W6Hous12bYP == 3 ~ 3, W6Hous12bYP == 4 ~ 7, TRUE ~ map_missing(W6Hous12bYP)
      ),
      W6Hous12YP == 2 ~ case_when(
        W6Hous12cYP %in% c(1, 2, 3) ~ 4, W6Hous12cYP == 4 ~ 5, W6Hous12cYP == 5 ~ 7, TRUE ~ map_missing(W6Hous12cYP)
      ),
      W6Hous12YP == 3 ~ 7,
      TRUE ~ map_missing(W6Hous12YP)
    ),
    hown19 = case_when(
      hownteen19 == 1 ~ 1,
      hownteen19 == 2 ~ 2,
      hownteen19 == 3 ~ 3,
      hownteen19 == 4 ~ 4,
      hownteen19 == 5 ~ 5,
      hownteen19 == 7 ~ 7,
      TRUE ~ hownteen19
    )
  )

# Age 20 (Wave 7)
full_df <- full_df %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12YP == 1 ~ case_when(
        W7Hous12bYP == 1 ~ 1, W7Hous12bYP == 2 ~ 2, W7Hous12bYP == 3 ~ 3, W7Hous12bYP == 4 ~ 7, TRUE ~ map_missing(W7Hous12bYP)
      ),
      W7Hous12YP == 2 ~ case_when(
        W7Hous12cYP %in% c(1, 2, 3) ~ 4, W7Hous12cYP == 4 ~ 5, W7Hous12cYP == 5 ~ 7, TRUE ~ map_missing(W7Hous12cYP)
      ),
      W7Hous12YP == 3 ~ 7,
      TRUE ~ map_missing(W7Hous12YP)
    ),
    hown20 = case_when(
      hownteen20 == 1 ~ 1,
      hownteen20 == 2 ~ 2,
      hownteen20 == 3 ~ 3,
      hownteen20 == 4 ~ 4,
      hownteen20 == 5 ~ 5,
      hownteen20 == 7 ~ 7,
      TRUE ~ hownteen20
    )
  )

# Age 25 (Wave 8)
full_df <- full_df %>%
  mutate(
    hown25 = case_when(
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE == 4 ~ 4,
      W8TENURE == 5 ~ 5,
      W8TENURE == 6 ~ 6,
      W8TENURE == 7 ~ 7,
      TRUE ~ map_missing(W8TENURE)
    )
  )

# Age 32 (Wave 9)
full_df <- full_df %>%
  mutate(
    hown32 = case_when(
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4,
      W9DTENURE == 5 ~ 5,
      W9DTENURE == 6 ~ 6,
      W9DTENURE == 7 ~ 7,
      TRUE ~ map_missing(W9DTENURE)
    )
  )

# Final selection
final_vars <- c('NSID', 
                'hownteen14', 'hown14', 
                'hownteen15', 'hown15', 
                'hownteen16', 'hown16', 
                'hownteen17', 'hown17', 
                'hownteen18', 'hown18', 
                'hownteen19', 'hown19', 
                'hownteen20', 'hown20', 
                'hown25', 'hown32')

final_df <- full_df %>% select(all_of(final_vars))

# Correct way to apply value labels
cat_labels_vec <- c(
  'Owned outright' = 1,
  'Own, buying with help of mortgage/loan' = 2,
  'Part rent, part mortgage (shared equity)' = 3,
  'Rent it' = 4,
  'Live rent-free' = 5,
  'Squatting' = 6,
  'Other' = 7,
  'Refusal' = -9,
  'Don\'t know' = -8,
  'Prefer not to say' = -7,
  'Not asked' = -3,
  'Schedule not applicable' = -2,
  'Not applicable' = -1
)

final_df <- final_df %>%
  mutate(across(starts_with('hown'), ~set_value_labels(.x, cat_labels_vec)))

write_csv(final_df, 'data/output/cleaned_data.csv')
