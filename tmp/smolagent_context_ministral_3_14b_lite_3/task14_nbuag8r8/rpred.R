
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Function to map missing values
map_missing <- function(x) {
  ifelse(is.na(x), -3,
         ifelse(x == -999 | x == -998 | x == -997 | x == -995, -2,
                ifelse(x == -99, -3,
                       ifelse(x == -94, -8,
                              ifelse(x == -92, -9,
                                     ifelse(x == -91 | x == -1, -1, x))))))
}

# Create tenure variables for waves 1-4 and 8-9
wave1 <- wave1 %>%
  mutate(hown14 = map_missing(W1hous12HH)) %>%
  mutate(hown14 = case_when(hown14 %in% c(4,5,6) ~ 4, TRUE ~ hown14))

wave2 <- wave2 %>%
  mutate(hown15 = map_missing(W2Hous12HH)) %>%
  mutate(hown15 = case_when(hown15 %in% c(4,5,6) ~ 4, TRUE ~ hown15))

wave3 <- wave3 %>%
  mutate(hown16 = map_missing(W3hous12HH)) %>%
  mutate(hown16 = case_when(hown16 %in% c(4,5,6) ~ 4, TRUE ~ hown16))

wave4 <- wave4 %>%
  mutate(hown17 = map_missing(W4Hous12HH)) %>%
  mutate(hown17 = case_when(hown17 %in% c(4,5,6) ~ 4, TRUE ~ hown17))

wave8 <- wave8 %>%
  mutate(hown25 = map_missing(W8TENURE)) %>%
  mutate(hown25 = case_when(hown25 %in% c(4,5) ~ 4, TRUE ~ hown25))

wave9 <- wave9 %>%
  mutate(hown32 = map_missing(W9DTENURE)) %>%
  mutate(hown32 = case_when(hown32 %in% c(4,5,6) ~ 4, TRUE ~ hown32))

# Combine datasets
combined_data <- full_join(wave1, wave2, by = 'NSID')
combined_data <- full_join(combined_data, wave3, by = 'NSID')
combined_data <- full_join(combined_data, wave4, by = 'NSID')
combined_data <- full_join(combined_data, wave8, by = 'NSID')
combined_data <- full_join(combined_data, wave9, by = 'NSID')

# Select only NSID and derived variables
final_vars <- c('NSID', 'hown14', 'hown15', 'hown16', 'hown17', 'hown25', 'hown32')
combined_data <- combined_data %>% select(all_of(final_vars))

# Write output
write_csv(combined_data, 'data/output/cleaned_data.csv')
