library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

combined <- full_join(wave1, wave4, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Convert to character first, then handle missing values
combined <- combined %>%
  mutate(
    partnr19 = ifelse(is.na(W6MarStatYP) | W6MarStatYP %in% c(-997, -97), -2,
      ifelse(W6MarStatYP %in% c(-92, -91, -1), as.character(W6MarStatYP),
      ifelse(W6MarStatYP %in% c(1, 2, 3, 4, 5), 'X', NA_character_))),
    partnr25 = ifelse(is.na(W8DMARSTAT) | W8DMARSTAT %in% c(-9, -8, -1), -3,
      ifelse(W8DMARSTAT %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9), as.character(W8DMARSTAT), NA_character_)),
    partnr32 = ifelse(is.na(W9DMARSTAT) | W9DMARSTAT %in% c(-9, -8), -3,
      ifelse(W9DMARSTAT %in% c(1, 2, 3, 4, 5, 6, 7, 8), as.character(W9DMARSTAT), NA_character_)),
    partnradu25 = ifelse(is.na(W8DMARSTAT) | W8DMARSTAT %in% c(-9, -8, -1), -3,
      as.character(W8DMARSTAT)),
    partnradu32 = ifelse(is.na(W9DMARSTAT) | W9DMARSTAT %in% c(-9, -8), -3,
      as.character(W9DMARSTAT))
  ) %>%
  mutate(
    partnr19 = case_when(
      partnr19 == '1' ~ 'Single',
      partnr19 == '2' ~ 'Married',
      partnr19 == '3' ~ 'Separated',
      partnr19 == '4' ~ 'Divorced',
      partnr19 == '5' ~ 'Widowed',
      TRUE ~ as.character(partnr19)
    ),
    partnr25 = case_when(
      partnr25 == '1' ~ 'Single',
      partnr25 == '2' ~ 'Married',
      partnr25 == '3' ~ 'Separated',
      partnr25 == '4' ~ 'Divorced',
      partnr25 == '5' ~ 'Widowed',
      partnr25 == '6' ~ 'Civil Partner',
      partnr25 == '7' ~ 'Separated CP',
      partnr25 == '8' ~ 'Former CP',
      partnr25 == '9' ~ 'Surviving CP',
      TRUE ~ as.character(partnr25)
    ),
    partnr32 = case_when(
      partnr32 == '1' ~ 'Single',
      partnr32 == '2' ~ 'Married',
      partnr32 == '3' ~ 'Divorced',
      partnr32 == '4' ~ 'Legally separated',
      partnr32 == '5' ~ 'Widowed',
      partnr32 == '6' ~ 'Civil Partner',
      partnr32 == '7' ~ 'Former CP',
      partnr32 == '8' ~ 'Surviving CP',
      TRUE ~ as.character(partnr32)
    )
  )

output <- select(combined, NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)
write_csv(output, 'data/output/cleaned_data.csv')
print('Done')
