library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
df_wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
df_wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
df_wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
df_wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
df_wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
df_wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
df_wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Create master dataset with full_join
master <- df_wave1 %>% full_join(df_wave4, by = 'NSID') %>% full_join(df_wave5, by = 'NSID') %>% full_join(df_wave6, by = 'NSID') %>% full_join(df_wave7, by = 'NSID') %>% full_join(df_wave8, by = 'NSID') %>% full_join(df_wave9, by = 'NSID')

# Function to collapse NS-SEC operational categories to major NS-SEC
collapse_nssec <- function(x) {
  result <- as.numeric(x)
  result <- case_when(
    result %in% c(1.0) ~ 1.0,  # Employers in large organisations
    result %in% c(2.0) ~ 2.0,  # Higher managerial and administrative occupations
    result %in% c(3.0, 3.1, 3.2, 3.3, 3.4) ~ 3.0,  # Higher professional occupations
    result %in% c(4.0, 4.1, 4.2, 4.3, 4.4) ~ 4.0,  # Lower professional and higher technical occupations
    result %in% c(5.0) ~ 5.0,  # Lower managerial and administrative occupations
    result %in% c(6.0) ~ 6.0,  # Higher supervisory occupations
    result %in% c(7.0, 7.1, 7.2, 7.3, 7.4) ~ 7.0,  # Intermediate occupations
    result %in% c(8.0, 8.1, 8.2) ~ 8.0,  # Employers in small establishments
    result %in% c(9.0, 9.1, 9.2) ~ 9.0,  # Own account workers
    result %in% c(10.0) ~ 10.0,  # Lower supervisory occupations
    result %in% c(11.0, 11.1, 11.2) ~ 11.0,  # Lower technical occupations
    result %in% c(12.0, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7) ~ 12.0,  # Semi-routine occupations
    result %in% c(13.0, 13.1, 13.2, 13.3, 13.4, 13.5) ~ 13.0,  # Routine occupations
    result %in% c(14.0, 14.1, 14.2) ~ 14.0,  # Never worked and Long-term unemployed
    result %in% c(15.0) ~ 15.0,  # Full-time students
    is.na(result) | result %in% c(-1.0, -2.0, -3.0, -4.0, -7.0, -8.0, -9.0) ~ -1.0,  # Missing
    TRUE ~ as.numeric(result)
  )
  return(result)
}

# For each wave, recode the NS-SEC variable to major categories
# Handle different missing value codes and convert to standard codes

# Age 17 (wave 4) - missing: -99.0, -91.0
master <- master %>% mutate(
  nssec17 = case_when(
    is.na(W4nsseccatYP) | W4nsseccatYP %in% c(-99.0, -91.0) ~ -1.0,
    W4nsseccatYP > 0 ~ collapse_nssec(W4nsseccatYP),
    TRUE ~ -1.0
  )
)

# Age 18 (wave 5) - missing: -91.0
master <- master %>% mutate(
  nssec18 = case_when(
    is.na(W5nsseccatYP) | W5nsseccatYP %in% c(-91.0) ~ -1.0,
    W5nsseccatYP > 0 ~ collapse_nssec(W5nsseccatYP),
    TRUE ~ -1.0
  )
)

# Age 19 (wave 6) - missing: -999.0 thru -1.0
master <- master %>% mutate(
  nssec19 = case_when(
    is.na(w6nsseccatYP) | w6nsseccatYP %in% c(-999.0, -98.0, -97.0, -96.0, -95.0, -94.0, -93.0, -92.0, -91.0, -90.0, -8.0, -7.0, -6.0, -5.0, -4.0, -3.0, -2.0, -1.0) ~ -1.0,
    w6nsseccatYP > 0 ~ collapse_nssec(w6nsseccatYP),
    TRUE ~ -1.0
  )
)

# Age 20 (wave 7) - missing: -999.0 thru -1.0
master <- master %>% mutate(
  nssec20 = case_when(
    is.na(W7NSSECCat) | W7NSSECCat %in% c(-999.0, -98.0, -97.0, -96.0, -95.0, -94.0, -93.0, -92.0, -91.0, -90.0, -8.0, -7.0, -6.0, -5.0, -4.0, -3.0, -2.0, -1.0) ~ -1.0,
    W7NSSECCat > 0 ~ collapse_nssec(W7NSSECCat),
    TRUE ~ -1.0
  )
)

# Age 25 (wave 8) - W8DNSSEC17 uses 2010 NS-SEC operational categories, missing: -9.0, -8.0, -1.0
master <- master %>% mutate(
  nssec25 = case_when(
    is.na(W8DNSSEC17) | W8DNSSEC17 %in% c(-9.0, -8.0, -1.0) ~ -1.0,
    W8DNSSEC17 > 0 ~ collapse_nssec(W8DNSSEC17),
    TRUE ~ -1.0
  )
)

# Age 32 (wave 9) - W9NSSEC is 2020 NS-SEC, missing: -9.0, -1.0
master <- master %>% mutate(
  nssec32 = case_when(
    is.na(W9NSSEC) | W9NSSEC %in% c(-9.0, -1.0) ~ -1.0,
    W9NSSEC > 0 ~ collapse_nssec(W9NSSEC),
    TRUE ~ -1.0
  )
)

# Remove raw source variables
master <- master %>% select(-W4nsseccatYP, -W5nsseccatYP, -w6nsseccatYP, -W7NSSECCat, -W8DNSSEC17, -W9NSSEC)

# Convert any remaining NA to -3
master <- master %>% mutate(across(everything(), ~ ifelse(is.na(.), -3, .)))

# Write output
write_csv(master, 'data/output/cleaned_data.csv')

cat('Script completed successfully\n')
print(dim(master))
print(head(master[, c('NSID', 'nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25', 'nssec32')]))