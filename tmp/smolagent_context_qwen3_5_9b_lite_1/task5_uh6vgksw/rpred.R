library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Recode Wave 6 (Age 19) W6MarStatYP
wave6 <- wave6 %>%
  mutate(W6MarStatYP = case_when(
    W6MarStatYP == -997 ~ -2,
    W6MarStatYP == -97 ~ -1,
    W6MarStatYP == -92 ~ -9,
    W6MarStatYP == -91 ~ -1,
    W6MarStatYP == -1 ~ -8,
    TRUE ~ W6MarStatYP
  )) %>%
  mutate(W6MarStatYP = ifelse(is.na(W6MarStatYP) | W6MarStatYP < 1, -9, W6MarStatYP))

# Recode Wave 8 (Age 25) W8DMARSTAT
ns8 <- ns8 %>%
  mutate(W8DMARSTAT = case_when(
    W8DMARSTAT == -9 ~ -9,
    W8DMARSTAT == -8 ~ -8,
    W8DMARSTAT == -1 ~ -1,
    TRUE ~ W8DMARSTAT
  ))

# Recode Wave 9 (Age 32) W9DMARSTAT
ns9 <- ns9 %>%
  mutate(W9DMARSTAT = case_when(
    W9DMARSTAT == -9 ~ -9,
    W9DMARSTAT == -8 ~ -8,
    TRUE ~ W9DMARSTAT
  ))

# Merge all datasets
full_data <- full_join(wave1, wave4, by = 'NSID')
full_data <- full_join(full_data, wave6, by = 'NSID')
full_data <- full_join(full_data, ns8, by = 'NSID')
full_data <- full_join(full_data, ns9, by = 'NSID')

# Create partnr19 (Age 19)
full_data$partnr19 <- full_data$W6MarStatYP
full_data$partnr19 <- ifelse(is.na(full_data$partnr19) | full_data$partnr19 < 1, -9, full_data$partnr19)

# Create partnr25 (Age 25)
full_data$partnr25 <- full_data$W8DMARSTAT
full_data$partnr25 <- ifelse(is.na(full_data$partnr25), -9, full_data$partnr25)

# Create partnr32 (Age 32)
full_data$partnr32 <- full_data$W9DMARSTAT
full_data$partnr32 <- ifelse(is.na(full_data$partnr32), -9, full_data$partnr32)

# Create partnradu25 (Age 25, adult only - exclude Single and Civil Partner)
full_data$partnradu25 <- full_data$W8DMARSTAT
full_data$partnradu25 <- ifelse(full_data$W8DMARSTAT %in% c(1, 6), -1, full_data$partnradu25)
full_data$partnradu25 <- ifelse(is.na(full_data$partnradu25), -9, full_data$partnradu25)

# Create partnradu32 (Age 32, adult only - exclude Single and former Civil Partner)
full_data$partnradu32 <- full_data$W9DMARSTAT
full_data$partnradu32 <- ifelse(full_data$W9DMARSTAT %in% c(1, 7), -1, full_data$partnradu32)
full_data$partnradu32 <- ifelse(is.na(full_data$partnradu32), -9, full_data$partnradu32)

# Convert to numeric for all output variables
full_data$partnr19 <- as.numeric(full_data$partnr19)
full_data$partnr25 <- as.numeric(full_data$partnr25)
full_data$partnr32 <- as.numeric(full_data$partnr32)
full_data$partnradu25 <- as.numeric(full_data$partnradu25)
full_data$partnradu32 <- as.numeric(full_data$partnradu32)

# Write output
write_csv(full_data, 'data/output/cleaned_data.csv')

print('Script completed successfully')