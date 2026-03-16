
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Step 1: Load datasets
wave_one <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_six <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
ns8_derived <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_derived <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Step 2: Merge datasets
merged_data <- full_join(wave_one, wave_four, by = 'NSID') %>%
  full_join(wave_six, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Step 3: Define missing value harmonization function
harmonize_missing <- function(x) {
  x %>%
    mutate(across(where(is.numeric), ~ case_when(
      . == -999 ~ -3,
      . == -997 ~ -9,
      . == -97 ~ -8,
      . == -92 ~ -9,
      . == -91 ~ -8,
      . == -99 ~ -3,
      . == -94 ~ -8,
      . == -995 ~ -9,
      . == -998 ~ -9,
      is.na(.) ~ -3,
      TRUE ~ .
    )))
}

# Step 4: Apply missing value harmonization
merged_data <- harmonize_missing(merged_data)

# Step 5: Create marital status variables
merged_data <- merged_data %>%
  mutate(
    marstat_collapsed = case_when(
      W6MarStatYP == 1 | W6MarStatYP == -3 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP %in% c(3, 4) ~ 3,
      W6MarStatYP == 5 ~ 4,
      W8DMARSTAT == 1 | W8DMARSTAT == -3 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT %in% c(3, 7) ~ 3,
      W8DMARSTAT %in% c(4, 8) ~ 3,
      W8DMARSTAT == 5 ~ 4,
      W8DMARSTAT == 6 ~ 2,
      W9DMARSTAT == 1 | W9DMARSTAT == -3 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT %in% c(3, 4) ~ 3,
      W9DMARSTAT == 5 ~ 4,
      W9DMARSTAT == 6 ~ 2,
      TRUE ~ -3
    )
  )

# Step 6: Create factor with labels
merged_data$marstat_collapsed <- factor(
  merged_data$marstat_collapsed,
  levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4),
  labels = c('Refused', 'Dont know', 'Prefer not to say', 'Not asked',
             'Schedule not applicable', 'Not applicable', 'Single',
             'Married', 'Separated/Divorced', 'Widowed')
)

# Step 7: Select output variables
output_vars <- c('NSID', 'marstat_collapsed', 'W6MarStatYP', 'W8DMARSTAT', 'W9DMARSTAT')

# Step 8: Write output
write.csv(merged_data %>% select(all_of(output_vars)), 'data/output/cleaned_data.csv', row.names = FALSE)
