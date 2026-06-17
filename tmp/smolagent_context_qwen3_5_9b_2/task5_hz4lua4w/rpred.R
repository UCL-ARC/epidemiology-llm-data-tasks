library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets
merged <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Process W6MarStatYP for partnr19 (Age 19)
merged <- merged %>%
  mutate(
    partnr19 = case_when(
      is.na(W6MarStatYP) ~ as.integer(-3),
      W6MarStatYP %in% c(-997, -97, -999) ~ as.integer(-2),
      W6MarStatYP == -998 ~ as.integer(-2),
      W6MarStatYP == -92 ~ as.integer(-9),
      W6MarStatYP == -91 ~ as.integer(-1),
      W6MarStatYP %in% c(-1, -9) ~ as.integer(-8),
      TRUE ~ as.integer(W6MarStatYP)
    )
  )

# Process W8DMARSTAT for partnradu25 (Age 25) - detailed
merged <- merged %>%
  mutate(
    partnradu25 = case_when(
      is.na(W8DMARSTAT) ~ NA_character_,
      W8DMARSTAT %in% c(-9, -8, -1) ~ NA_character_,
      TRUE ~ as.character(W8DMARSTAT)
    )
  ) %>%
  mutate(
    partnradu25 = case_when(
      partnradu25 == '1' ~ 'Single never married or CP',
      partnradu25 == '2' ~ 'Married',
      partnradu25 == '3' ~ 'Separated but still married',
      partnradu25 == '4' ~ 'Divorced',
      partnradu25 == '5' ~ 'Widowed',
      partnradu25 == '6' ~ 'Civil Partner',
      partnradu25 == '7' ~ 'Separated but still in CP',
      partnradu25 == '8' ~ 'Former Civil Partner',
      partnradu25 == '9' ~ 'Surviving Civil Partner',
      TRUE ~ partnradu25
    )
  )

# Create partnr25 from W8DMARSTAT - collapsed harmonised
merged <- merged %>%
  mutate(
    partnr25 = case_when(
      is.na(W8DMARSTAT) ~ as.integer(-3),
      W8DMARSTAT %in% c(-9, -8, -1) ~ as.integer(-2),
      W8DMARSTAT %in% c(-97, -999) ~ as.integer(-9),
      W8DMARSTAT == -92 ~ as.integer(-9),
      W8DMARSTAT == -91 ~ as.integer(-1),
      W8DMARSTAT %in% c(1, 6) ~ as.integer(1),
      W8DMARSTAT == 2 ~ as.integer(2),
      W8DMARSTAT %in% c(3, 7) ~ as.integer(3),
      W8DMARSTAT %in% c(4, 8) ~ as.integer(4),
      W8DMARSTAT %in% c(5, 9) ~ as.integer(5),
      TRUE ~ as.integer(W8DMARSTAT)
    )
  )

# Process W9DMARSTAT for partnradu32 (Age 32) - detailed
merged <- merged %>%
  mutate(
    partnradu32 = case_when(
      is.na(W9DMARSTAT) ~ NA_character_,
      W9DMARSTAT %in% c(-9, -8, -1) ~ NA_character_,
      TRUE ~ as.character(W9DMARSTAT)
    )
  ) %>%
  mutate(
    partnradu32 = case_when(
      partnradu32 == '1' ~ 'Single never married or never in CP',
      partnradu32 == '2' ~ 'Married',
      partnradu32 == '3' ~ 'Divorced',
      partnradu32 == '4' ~ 'Legally separated',
      partnradu32 == '5' ~ 'Widowed',
      partnradu32 == '6' ~ 'Civil Partner',
      partnradu32 == '7' ~ 'Former Civil Partner (dissolved)',
      partnradu32 == '8' ~ 'Surviving Civil Partner',
      TRUE ~ partnradu32
    )
  )

# Create partnr32 from W9DMARSTAT - collapsed harmonised
merged <- merged %>%
  mutate(
    partnr32 = case_when(
      is.na(W9DMARSTAT) ~ as.integer(-3),
      W9DMARSTAT %in% c(-9, -8, -1) ~ as.integer(-2),
      TRUE ~ as.integer(W9DMARSTAT)
    )
  )

# Select final variables
output <- merged %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(paste('Rows:', nrow(output), 'Columns:', ncol(output)))
print(head(output))