library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Define wave-to-age mapping
wave_to_age <- c(wave4 = 17, wave5 = 18, wave6 = 19, wave7 = 20, wave8 = 25, wave9 = 32)

# Define missing value codes
mval <- c(-9, -8, -7, -3, -2, -1)

# Process each wave's NS-SEC variable
# Wave 4 (Age 17) - W4nsseccatYP
nssec17 <- wave4 %>%
  mutate(nssec17 = case_when(
    !is.na(W4nsseccatYP) & between(W4nsseccatYP, -99, -1) ~ -3,
    !is.na(W4nsseccatYP) & W4nsseccatYP == -91 ~ -3,
    !is.na(W4nsseccatYP) ~ floor(W4nsseccatYP),
    TRUE ~ -3
  )) %>%
  select(NSID, nssec17)

# Wave 5 (Age 18) - W5nsseccatYP
nssec18 <- wave5 %>%
  mutate(nssec18 = case_when(
    !is.na(W5nsseccatYP) & between(W5nsseccatYP, -999, -1) ~ -3,
    !is.na(W5nsseccatYP) & W5nsseccatYP == -91 ~ -3,
    !is.na(W5nsseccatYP) ~ floor(W5nsseccatYP),
    TRUE ~ -3
  )) %>%
  select(NSID, nssec18)

# Wave 6 (Age 19) - w6nsseccatYP
nssec19 <- wave6 %>%
  mutate(nssec19 = case_when(
    !is.na(w6nsseccatYP) & between(w6nsseccatYP, -999, -1) ~ -3,
    !is.na(w6nsseccatYP) & w6nsseccatYP == -91 ~ -3,
    !is.na(w6nsseccatYP) ~ floor(w6nsseccatYP),
    TRUE ~ -3
  )) %>%
  select(NSID, nssec19)

# Wave 7 (Age 20) - W7NSSECCat
nssec20 <- wave7 %>%
  mutate(nssec20 = case_when(
    !is.na(W7NSSECCat) & between(W7NSSECCat, -999, -1) ~ -3,
    !is.na(W7NSSECCat) & W7NSSECCat == -91 ~ -3,
    !is.na(W7NSSECCat) ~ floor(W7NSSECCat),
    TRUE ~ -3
  )) %>%
  select(NSID, nssec20)

# Wave 8 (Age 25) - W8DNSSEC17 with special rule for full-time students
nssec25 <- wave8 %>%
  mutate(
    w8_activity = factor(W8DACTIVITYC, 
      levels = c(-9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      labels = c('Refused', 'Insufficient information', 'Not applicable', 
                 'Employee', 'Self employed', 'Unpaid/voluntary work', 'Unemployed',
                 'Education: School/college/university', 'Apprenticeship',
                 "On gov't scheme for employment training", 'Sick or disabled',
                 'Looking after home or family', 'Something else')
    )
  ) %>%
  mutate(
    nssec25 = case_when(
      # Full-time education gets category 15
      (is.na(W8DNSSEC17) | is.na(W8DACTIVITYC)) & w8_activity == 'Education: School/college/university' ~ 15,
      # Missing NS-SEC with non-education activity gets -3
      is.na(W8DNSSEC17) & !(w8_activity == 'Education: School/college/university') & !is.na(W8DACTIVITYC) ~ -3,
      # Missing NS-SEC with missing activity gets -3
      is.na(W8DNSSEC17) & is.na(W8DACTIVITYC) ~ -3,
      # NS-SEC is missing and handling NAs
      is.na(W8DNSSEC17) ~ -3,
      # Map missing codes
      W8DNSSEC17 %in% c(-9, -8, -1) ~ -3,
      # Take floor of fractional codes
      TRUE ~ floor(W8DNSSEC17)
    )
  ) %>%
  select(NSID, nssec25)

# Wave 9 (Age 32) - W9NSSEC
nssec32 <- wave9 %>%
  mutate(nssec32 = case_when(
    !is.na(W9NSSEC) & between(W9NSSEC, -9, -1) ~ -3,
    !is.na(W9NSSEC) & W9NSSEC %in% c(-1) ~ -3,
    !is.na(W9NSSEC) ~ W9NSSEC,
    TRUE ~ -3
  )) %>%
  select(NSID, nssec32)

# Merge all datasets by NSID
result <- full_join(wave1, nssec17, by = 'NSID') %>%
  full_join(nssec18, by = 'NSID') %>%
  full_join(nssec19, by = 'NSID') %>%
  full_join(nssec20, by = 'NSID') %>%
  full_join(nssec25, by = 'NSID') %>%
  full_join(nssec32, by = 'NSID')

# Remove any remaining intermediate variables
result <- result %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write_csv(result, 'data/output/cleaned_data.csv')

print('Script completed successfully')
