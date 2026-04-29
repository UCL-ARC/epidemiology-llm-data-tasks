
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Step 1: Load datasets
wave_one <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_six <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
ns8_derived <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_derived <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Step 2: Merge datasets using NSID as the key
merged_data <- full_join(wave_one, wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Step 3: Define missing value harmonization function (only for numeric columns)
harmonize_missing <- function(x) {
  x %>%
    mutate(across(
      where(is.numeric),
      ~ case_when(
        . %in% c(-999, -997, -97, -92, -91, -99, -94, -995, -998, -9999) ~
          case_when(
            . %in% c(-92, -9) ~ -9,
            . %in% c(-91, -8) ~ -8,
            . %in% c(-1, -995) ~ -1,
            . %in% c(-997, -998, -9999) ~ -3,
            TRUE ~ .
          ),
        is.na(.) ~ -3,
        TRUE ~ .
      )
    ))
}

# Step 4: Harmonize missing values for numeric variables only
merged_data <- harmonize_missing(merged_data)

# Step 5: Define function to create age-specific variables for marital status
create_marital_status_vars <- function(data) {
  # Wave 6 (Age 19) Marital Status
  data <- data %>%
    mutate(
      partnr19 = ifelse(W6MarStatYP %in% c(1, 2, 3, 4, 5), W6MarStatYP, -3)
    )

  # Wave 8 (Age 25) Marital Status
  data <- data %>%
    mutate(
      partnr25 = ifelse(W8DMARSTAT %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9), W8DMARSTAT, -3),
      partnr25 = case_when(
        partnr25 == 1 ~ 1,
        partnr25 == 2 ~ 2,
        partnr25 == 3 ~ 3,
        partnr25 == 4 ~ 4,
        partnr25 == 5 ~ 5,
        partnr25 %in% c(6, 7, 8, 9) ~ 6,
        TRUE ~ -3
      )
    )

  # Wave 9 (Age 32) Marital Status
  data <- data %>%
    mutate(
      partnr32 = ifelse(W9DMARSTAT %in% c(1, 2, 3, 4, 5, 6, 7, 8), W9DMARSTAT, -3),
      partnr32 = case_when(
        partnr32 == 1 ~ 1,
        partnr32 == 2 ~ 2,
        partnr32 == 3 ~ 4,
        partnr32 == 4 ~ 3,
        partnr32 == 5 ~ 5,
        partnr32 %in% c(6, 7, 8) ~ 6,
        TRUE ~ -3
      )
    )

  # Create collapsed harmonized variable
  data <- data %>%
    mutate(
      partnr = case_when(
        partnr19 %in% c(1, 2, 3, 4, 5) ~ partnr19,
        partnr25 %in% c(1, 2, 3, 4, 5, 6) ~ partnr25,
        partnr32 %in% c(1, 2, 3, 4, 5, 6) ~ partnr32,
        TRUE ~ -3
      )
    )

  # Create detailed adult variables
  data <- data %>%
    mutate(
      partnradu25 = ifelse(partnr25 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9), partnr25, -3),
      partnradu32 = ifelse(partnr32 %in% c(1, 2, 3, 4, 5, 6, 7, 8), partnr32, -3)
    )

  return(data)
}

# Step 6: Apply marital status variable creation
merged_data <- create_marital_status_vars(merged_data)

# Step 7: Create factor variables with labels for marital status variables
merged_data <- merged_data %>%
  mutate(
    partnr = factor(partnr,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6),
                    labels = c('Refusal', 'Dont know/insufficient info', 'Prefer not to say',
                               'Not asked/fieldwork error', 'Schedule error', 'Item not applicable',
                               'Single', 'Married', 'Separated', 'Divorced', 'Widowed', 'Civil partnership')),
    partnr19 = factor(partnr19,
                      levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5),
                      labels = c('Refusal', 'Dont know/insufficient info', 'Prefer not to say',
                                'Not asked/fieldwork error', 'Schedule error', 'Item not applicable',
                                'Single', 'Married', 'Separated', 'Divorced', 'Widowed')),
    partnr25 = factor(partnr25,
                      levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6),
                      labels = c('Refusal', 'Dont know/insufficient info', 'Prefer not to say',
                                'Not asked/fieldwork error', 'Schedule error', 'Item not applicable',
                                'Single', 'Married', 'Separated', 'Divorced', 'Widowed', 'Civil partnership')),
    partnradu25 = factor(partnradu25,
                        levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c('Refusal', 'Dont know/insufficient info', 'Prefer not to say',
                                  'Not asked/fieldwork error', 'Schedule error', 'Item not applicable',
                                  'Single', 'Married', 'Separated', 'Divorced', 'Widowed',
                                  'Civil partner', 'Separated CP', 'Former CP', 'Surviving CP')),
    partnr32 = factor(partnr32,
                      levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6),
                      labels = c('Refusal', 'Dont know/insufficient info', 'Prefer not to say',
                                'Not asked/fieldwork error', 'Schedule error', 'Item not applicable',
                                'Single', 'Married', 'Divorced', 'Separated', 'Widowed', 'Civil partnership')),
    partnradu32 = factor(partnradu32,
                        levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c('Refusal', 'Dont know/insufficient info', 'Prefer not to say',
                                  'Not asked/fieldwork error', 'Schedule error', 'Item not applicable',
                                  'Single', 'Married', 'Divorced', 'Separated', 'Widowed',
                                  'Civil partner', 'Former CP', 'Surviving CP'))
  )

# Step 8: Select only the ID and derived variables for output
output_vars <- c('NSID', 'partnr', 'partnr19', 'partnr25', 'partnradu25', 'partnr32', 'partnradu32')

# Step 9: Write output to CSV
write_csv(merged_data %>% select(all_of(output_vars)), 'data/output/cleaned_data.csv')

# Print confirmation
message('Data cleaning and preprocessing complete. Output saved to data/output/cleaned_data.csv')
