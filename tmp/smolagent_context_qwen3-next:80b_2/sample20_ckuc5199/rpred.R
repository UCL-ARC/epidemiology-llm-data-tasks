library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Process wave1 (age 14)
selected1 <- wave1 %>%
  select(NSID, W1alceverYP, W1alcmonYP) %>%
  rename(ever_14 = W1alceverYP, mon_14 = W1alcmonYP)

# Process wave2 (age 15)
selected2 <- wave2 %>%
  select(NSID, W2alceverYP, W2alcfreqYP) %>%
  rename(ever_15 = W2alceverYP, freq_15 = W2alcfreqYP)

# Process wave3 (age 16)
selected3 <- wave3 %>%
  select(NSID, W3alceverYP, W3alcfreqYP) %>%
  rename(ever_16 = W3alceverYP, freq_16 = W3alcfreqYP)

# Process wave4 (age 17)
selected4 <- wave4 %>%
  select(NSID, W4AlcEverYP, W4AlcFreqYP) %>%
  rename(ever_17 = W4AlcEverYP, freq_17 = W4AlcFreqYP)

# Process wave6 (age 19)
selected6 <- wave6 %>%
  select(NSID, W6AlcEverYP, W6AlcFreqYP) %>%
  rename(ever_19 = W6AlcEverYP, freq_19 = W6AlcFreqYP)

# Process wave7 (age 20)
selected7 <- wave7 %>%
  select(NSID, W7AlcEverYP, W7AlcFreqYP) %>%
  rename(ever_20 = W7AlcEverYP, freq_20 = W7AlcFreqYP)

# Process wave8 (age 25)
selected8 <- wave8 %>%
  select(NSID, starts_with('W8AUDIT'))
if ('W8AUDIT1' %in% names(selected8)) {
  selected8 <- selected8 %>% rename(freq_25 = W8AUDIT1)
} else if ('W8AUDIT6' %in% names(selected8)) {
  selected8 <- selected8 %>% rename(freq_25 = W8AUDIT6)
} else {
  selected8 <- selected8 %>% mutate(freq_25 = NA)
}

# Process wave9 (age 32)
selected9 <- wave9 %>%
  select(NSID, W9AUDIT1) %>%
  rename(freq_32 = W9AUDIT1)

# Merge all datasets
merged_data <- selected1 %>%
  full_join(selected2, by = 'NSID') %>%
  full_join(selected3, by = 'NSID') %>%
  full_join(selected4, by = 'NSID') %>%
  full_join(selected6, by = 'NSID') %>%
  full_join(selected7, by = 'NSID') %>%
  full_join(selected8, by = 'NSID') %>%
  full_join(selected9, by = 'NSID')

# Convert missing values (<= -1) to NA
merged_data <- merged_data %>%
  mutate(across(everything(), ~ ifelse(.x <= -1, NA, .x)))

# Create drinker indicators
merged_data <- merged_data %>%
  mutate(
    drinker_14 = ifelse(is.na(ever_14) | is.na(mon_14), NA, ifelse(ever_14 == 1 & mon_14 == 1, TRUE, FALSE)),
    drinker_15 = ifelse(is.na(ever_15), NA, ifelse(ever_15 == 1, TRUE, FALSE)),
    drinker_16 = ifelse(is.na(ever_16), NA, ifelse(ever_16 == 1, TRUE, FALSE)),
    drinker_17 = ifelse(is.na(ever_17), NA, ifelse(ever_17 == 1, TRUE, FALSE)),
    drinker_19 = ifelse(is.na(ever_19), NA, ifelse(ever_19 == 1, TRUE, FALSE)),
    drinker_20 = ifelse(is.na(ever_20), NA, ifelse(ever_20 == 1, TRUE, FALSE)),
    drinker_25 = ifelse(is.na(freq_25), NA, ifelse(freq_25 != 1, TRUE, FALSE)),
    drinker_32 = ifelse(is.na(freq_32), NA, ifelse(freq_32 != 1, TRUE, FALSE))
  )

# Calculate alcfst
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(
    alcfst = {
      drinker_vec <- c(drinker_14, drinker_15, drinker_16, drinker_17, drinker_19, drinker_20, drinker_25, drinker_32)
      ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
      if (any(drinker_vec, na.rm = TRUE)) {
        min(ages[drinker_vec], na.rm = TRUE)
      } else if (all(!drinker_vec, na.rm = TRUE)) {
        99
      } else {
        -8
      }
    }
  ) %>%
  ungroup()

# Convert to factor
merged_data <- merged_data %>%
  mutate(alcfst = factor(alcfst, 
                         levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
                         labels = c('14', '15', '16', '17', '19', '20', '25', '32', 'Never had alcohol', 'Don\'t know/insufficient information')))

# Output
output <- merged_data %>% select(NSID, alcfst)
write_csv(output, 'data/output/cleaned_data.csv')