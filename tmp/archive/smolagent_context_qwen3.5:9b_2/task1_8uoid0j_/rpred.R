# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Define all files to load
files <- c(
  'data/input/wave_one_lsype_young_person_2020.tab',
  'data/input/wave_two_lsype_young_person_2020.tab',
  'data/input/wave_three_lsype_young_person_2020.tab',
  'data/input/wave_four_lsype_young_person_2020.tab',
  'data/input/wave_five_lsype_young_person_2020.tab',
  'data/input/wave_six_lsype_young_person_2020.tab',
  'data/input/wave_seven_lsype_young_person_2020.tab',
  'data/input/ns8_2015_main_interview.tab',
  'data/input/ns9_2022_main_interview.tab'
)

# Load all datasets
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_five <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave_six <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Create wave indicator variable
wave_one <- wave_one %>% mutate(wave = 1)
wave_two <- wave_two %>% mutate(wave = 2)
wave_three <- wave_three %>% mutate(wave = 3)
wave_four <- wave_four %>% mutate(wave = 4)
wave_five <- wave_five %>% mutate(wave = 5)
wave_six <- wave_six %>% mutate(wave = 6)
wave_seven <- wave_seven %>% mutate(wave = 7)
ns8 <- ns8 %>% mutate(wave = 8)
ns9 <- ns9 %>% mutate(wave = 9)

# Merge all datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = 'NSID')
merged_data <- full_join(merged_data, wave_three, by = 'NSID')
merged_data <- full_join(merged_data, wave_four, by = 'NSID')
merged_data <- full_join(merged_data, wave_five, by = 'NSID')
merged_data <- full_join(merged_data, wave_six, by = 'NSID')
merged_data <- full_join(merged_data, wave_seven, by = 'NSID')
merged_data <- full_join(merged_data, ns8, by = 'NSID')
merged_data <- full_join(merged_data, ns9, by = 'NSID')

# Standardize sex variable across waves with standard missing codes
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      !is.na(W1sexYP) & W1sexYP == -99 ~ -9,
      !is.na(W1sexYP) & W1sexYP == -92 ~ -9,
      !is.na(W1sexYP) & W1sexYP == -91 ~ -1,
      !is.na(W1sexYP) & W1sexYP %in% c(1, 2) ~ as.numeric(W1sexYP),
      !is.na(W2SexYP) & W2SexYP %in% c(-998, -997, -995, -99, -92, -91) ~ -9,
      !is.na(W2SexYP) & W2SexYP == -1 ~ -8,
      !is.na(W2SexYP) & W2SexYP %in% c(1, 2) ~ as.numeric(W2SexYP),
      !is.na(W3sexYP) & W3sexYP %in% c(-99, -92, -91) ~ -9,
      !is.na(W3sexYP) & W3sexYP %in% c(1, 2) ~ as.numeric(W3sexYP),
      !is.na(W4SexYP) & W4SexYP %in% c(-99, -92, -91) ~ -9,
      !is.na(W4SexYP) & W4SexYP == -1 ~ -8,
      !is.na(W4SexYP) & W4SexYP %in% c(1, 2) ~ as.numeric(W4SexYP),
      !is.na(W5SexYP) & W5SexYP == -1 ~ -8,
      !is.na(W5SexYP) & W5SexYP %in% c(1, 2) ~ as.numeric(W5SexYP),
      !is.na(W6Sex) & W6Sex %in% c(-92, -91) ~ -9,
      !is.na(W6Sex) & W6Sex %in% c(1, 2) ~ as.numeric(W6Sex),
      !is.na(W7Sex) & W7Sex %in% c(-91) ~ -1,
      !is.na(W7Sex) & W7Sex %in% c(1, 2) ~ as.numeric(W7Sex),
      !is.na(W8CMSEX) & W8CMSEX == -9 ~ -9,
      !is.na(W8CMSEX) & W8CMSEX == -8 ~ -8,
      !is.na(W8CMSEX) & W8CMSEX == -1 ~ -1,
      !is.na(W8CMSEX) & W8CMSEX %in% c(1, 2) ~ as.numeric(W8CMSEX),
      !is.na(W9DSEX) & W9DSEX %in% c(1, 2) ~ as.numeric(W9DSEX),
      TRUE ~ NA_real_
    )
  )

# Create factor variable with labels
merged_data <- merged_data %>%
  mutate(
    sex = factor(sex, levels = c(-9, -8, 1, 2, -1, -3), 
                 labels = c('Refusal', 'Don\'t know', 'Male', 'Female', 'Not applicable', 'Not asked'))
  )

# Select only NSID and derived sex variable
output_data <- merged_data %>%
  select(NSID, sex)

# Output the cleaned data
write_delim(output_data, 'data/output/cleaned_data.csv', delim = ',')

# Verify output
print(paste('Successfully created cleaned data with', nrow(output_data), 'cases and', ncol(output_data), 'variables'))
print(head(output_data))