library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
file1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Convert necessary variables to numeric
file6 <- file6 %>% mutate(W6MarStatYP = as.numeric(W6MarStatYP))
file8 <- file8 %>% mutate(W8DMARSTAT = as.numeric(W8DMARSTAT))
file9 <- file9 %>% mutate(W9DMARSTAT = as.numeric(W9DMARSTAT))

# Merge datasets
full_frame <- file1 %>%
  full_join(file4, by = 'NSID') %>%
  full_join(file6, by = 'NSID') %>%
  full_join(file8, by = 'NSID') %>%
  full_join(file9, by = 'NSID')

# Harmonisation logic
# Standard missing codes:
# -9 Refusal, -8 Don't know/insufficient, -7 Prefer not to say, -3 Not asked, -2 Schedule error/lost, -1 Not applicable

# 1. partnr19 (from W6MarStatYP)
full_frame <- full_frame %>%
  mutate(partnr19 = case_when(
    W6MarStatYP == 1 ~ 1, # Single
    W6MarStatYP == 2 ~ 2, # Married
    W6MarStatYP == 3 ~ 3, # Separated
    W6MarStatYP == 4 ~ 4, # Divorced
    W6MarStatYP == 5 ~ 5, # Widowed
    W6MarStatYP == -92 ~ -9, # Refused
    W6MarStatYP == -97 ~ -7, # Respondent declined
    W6MarStatYP == -91 ~ -1, # Not applicable
    W6MarStatYP == -1 ~ -8, # Don't know
    W6MarStatYP == -997 ~ -2, # Script error
    is.na(W6MarStatYP) ~ -3, 
    TRUE ~ -3
  ))

# 2. partnradu25 (from W8DMARSTAT)
full_frame <- full_frame %>%
  mutate(partnradu25 = case_when(
    W8DMARSTAT == 1 ~ 1, # Single
    W8DMARSTAT == 2 ~ 2, # Married
    W8DMARSTAT == 3 ~ 3, # Separated
    W8DMARSTAT == 4 ~ 4, # Divorced
    W8DMARSTAT == 5 ~ 5, # Widowed
    W8DMARSTAT == 6 ~ 6, # Civil Partner
    W8DMARSTAT == 7 ~ 7, # Separated CP
    W8DMARSTAT == 8 ~ 8, # Former CP
    W8DMARSTAT == 9 ~ 9, # Surviving CP
    W8DMARSTAT == -9 ~ -9, # Refused
    W8DMARSTAT == -8 ~ -8, # Insufficient
    W8DMARSTAT == -1 ~ -1, # Not applicable
    is.na(W8DMARSTAT) ~ -3,
    TRUE ~ -3
  ))

# 3. partnradu32 (from W9DMARSTAT)
full_frame <- full_frame %>%
  mutate(partnradu32 = case_when(
    W9DMARSTAT == 1 ~ 1, # Single
    W9DMARSTAT == 2 ~ 2, # Married
    W9DMARSTAT == 3 ~ 3, # Divorced
    W9DMARSTAT == 4 ~ 4, # Separated
    W9DMARSTAT == 5 ~ 5, # Widowed
    W9DMARSTAT == 6 ~ 6, # Civil Partner
    W9DMARSTAT == 7 ~ 7, # Former CP
    W9DMARSTAT == 8 ~ 8, # Surviving CP
    W9DMARSTAT == -9 ~ -9, # Refused
    W9DMARSTAT == -8 ~ -8, # Insufficient
    is.na(W9DMARSTAT) ~ -3,
    TRUE ~ -3
  ))

# Collapse detailed adult variables into partnr25 and partnr32
# Target categories for partnr: 1:Single, 2:Married/CP, 3:Separated, 4:Divorced/Former CP, 5:Widowed/Surviving CP

full_frame <- full_frame %>%
  mutate(partnr25 = case_when(
    partnradu25 == 1 ~ 1,
    partnradu25 %in% c(2, 6) ~ 2,
    partnradu25 %in% c(3, 7) ~ 3,
    partnradu25 %in% c(4, 8) ~ 4,
    partnradu25 %in% c(5, 9) ~ 5,
    partnradu25 == -9 ~ -9,
    partnradu25 == -8 ~ -8,
    partnradu25 == -1 ~ -1,
    TRUE ~ -3
  ))

full_frame <- full_frame %>%
  mutate(partnr32 = case_when(
    partnradu32 == 1 ~ 1,
    partnradu32 %in% c(2, 6) ~ 2,
    partnradu32 == 4 ~ 3, # Legally separated
    partnradu32 %in% c(3, 7) ~ 4, # Divorced/Former CP
    partnradu32 %in% c(5, 8) ~ 5, # Widowed/Surviving CP
    partnradu32 == -9 ~ -9,
    partnradu32 == -8 ~ -8,
    TRUE ~ -3
  ))

# Factor Labels
partnr_labels <- c("1" = "Single", "2" = "Married/CP", "3" = "Separated", "4" = "Divorced", "5" = "Widowed", "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Schedule error", "-1" = "Not applicable")

# Final selection
final_data <- full_frame %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Apply labels to collapsed variables
final_data$partnr19 <- factor(final_data$partnr19, levels = names(partnr_labels), labels = partnr_labels)
final_data$partnr25 <- factor(final_data$partnr25, levels = names(partnr_labels), labels = partnr_labels)
final_data$partnr32 <- factor(final_data$partnr32, levels = names(partnr_labels), labels = partnr_labels)

write_csv(final_data, 'data/output/cleaned_data.csv')
