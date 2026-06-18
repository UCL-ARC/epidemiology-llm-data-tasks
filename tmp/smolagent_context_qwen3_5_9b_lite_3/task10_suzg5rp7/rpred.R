library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all survey files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets
all_data <- full_join(wave1, wave4, by = 'NSID')
all_data <- full_join(all_data, wave5, by = 'NSID')
all_data <- full_join(all_data, wave6, by = 'NSID')
all_data <- full_join(all_data, wave7, by = 'NSID')
all_data <- full_join(all_data, wave8, by = 'NSID')
all_data <- full_join(all_data, wave9, by = 'NSID')

# Check columns exist
print(sprintf('W4empsYP in data: %s', 'W4empsYP' %in% names(all_data)))
print(sprintf('W5mainactYP in data: %s', 'W5mainactYP' %in% names(all_data)))
print(sprintf('W6TCurrentAct in data: %s', 'W6TCurrentAct' %in% names(all_data)))
print(sprintf('W7TCurrentAct in data: %s', 'W7TCurrentAct' %in% names(all_data)))
print(sprintf('W8DACTIVITYC in data: %s', 'W8DACTIVITYC' %in% names(all_data)))
print(sprintf('W9DACTIVITYC in data: %s', 'W9DACTIVITYC' %in% names(all_data)))

# Wave 4 (age 17) - W4empsYP (6-category collapsed)
all_data <- all_data %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP %in% c(1, 2, 4, 5, 6) ~ 1,  # Paid work or training/education
      W4empsYP %in% c(3, 7, 8, 9) ~ 2,     # Unemployed/Sick/Other
      W4empsYP < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W4empsYP)
    )
  )

# Wave 5 (age 18) - W5mainactYP (6-category collapsed)
all_data <- all_data %>%
  mutate(
    ecoact18 = case_when(
      W5mainactYP %in% c(3, 6) ~ 1,    # In paid work/On scheme
      W5mainactYP %in% c(4, 5, 9, 10, 11) ~ 2,  # Education/Waiting
      W5mainactYP %in% c(7) ~ 3,       # Unemployed
      W5mainactYP %in% c(8) ~ 4,       # Looking after family
      W5mainactYP %in% c(1, 2) ~ 5,    # Other (apprenticeship, split)
      W5mainactYP < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W5mainactYP)
    )
  )

# Wave 6 (age 19) - W6TCurrentAct (6-category collapsed)
all_data <- all_data %>%
  mutate(
    ecoact19 = case_when(
      W6TCurrentAct %in% c(3, 5) ~ 1,  # Paid work/Apprenticeship
      W6TCurrentAct %in% c(2, 4, 6, 9, 10, 11) ~ 2,  # Education/Training/Waiting
      W6TCurrentAct %in% c(8) ~ 3,      # Unemployed
      W6TCurrentAct %in% c(7) ~ 4,      # Looking after family
      W6TCurrentAct %in% c(1) ~ 5,      # University course
      W6TCurrentAct < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W6TCurrentAct)
    )
  )

# Wave 7 (age 20) - W7TCurrentAct (6-category collapsed)
all_data <- all_data %>%
  mutate(
    ecoact20 = case_when(
      W7TCurrentAct %in% c(3, 5) ~ 1,  # Paid work/Apprenticeship
      W7TCurrentAct %in% c(2, 4, 6, 9) ~ 2,  # Education/Training
      W7TCurrentAct %in% c(8) ~ 3,       # Unemployed
      W7TCurrentAct %in% c(7, 13, 14) ~ 4,  # Looking after family/Break/Ill
      W7TCurrentAct %in% c(10, 11, 12, 15) ~ 5,  # Other
      W7TCurrentAct < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W7TCurrentAct)
    )
  )

# Wave 8 (age 25) - W8DACTIVITYC (6-category collapsed)
all_data <- all_data %>%
  mutate(
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(1, 2) ~ 1,  # Employee/Self employed
      W8DACTIVITYC %in% c(5, 6) ~ 2,   # Education/Apprenticeship
      W8DACTIVITYC %in% c(4, 7, 10) ~ 3,  # Unemployed/On gov't scheme/Something else
      W8DACTIVITYC %in% c(3, 8, 9) ~ 4,  # Unpaid work/Sick/Looking after family
      W8DACTIVITYC < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W8DACTIVITYC)
    )
  )

# Wave 9 (age 32) - W9DACTIVITYC (6-category collapsed)
all_data <- all_data %>%
  mutate(
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(1, 2) ~ 1,  # Employee/Self employed
      W9DACTIVITYC %in% c(5, 6) ~ 2,   # Education/Apprenticeship
      W9DACTIVITYC %in% c(4, 7, 10) ~ 3,  # Unemployed/On gov't scheme/Something else
      W9DACTIVITYC %in% c(3, 8, 9) ~ 4,  # Unpaid work/Sick/Looking after family
      W9DACTIVITYC < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W9DACTIVITYC)
    )
  )

# Wave 8 (age 25) - ecoactadu25 (detailed, 10 categories)
all_data <- all_data %>%
  mutate(
    ecoactadu25 = case_when(
      W8DACTIVITYC == 1 ~ 1,           # Employee - in paid work
      W8DACTIVITYC == 2 ~ 2,           # Self employed
      W8DACTIVITYC == 3 ~ 3,           # In unpaid/voluntary work
      W8DACTIVITYC == 4 ~ 4,           # Unemployed
      W8DACTIVITYC == 5 ~ 5,           # Education: School/college/university
      W8DACTIVITYC == 6 ~ 6,           # Apprenticeship
      W8DACTIVITYC == 7 ~ 7,           # On gov't scheme for employment training
      W8DACTIVITYC == 8 ~ 8,           # Sick or disabled
      W8DACTIVITYC == 9 ~ 9,           # Looking after home or family
      W8DACTIVITYC == 10 ~ 10,         # Something else
      W8DACTIVITYC < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W8DACTIVITYC)
    )
  )

# Wave 9 (age 32) - ecoactadu32 (detailed, 10 categories)
all_data <- all_data %>%
  mutate(
    ecoactadu32 = case_when(
      W9DACTIVITYC == 1 ~ 1,           # Employee - in paid work
      W9DACTIVITYC == 2 ~ 2,           # Self employed
      W9DACTIVITYC == 3 ~ 3,           # In unpaid/voluntary work
      W9DACTIVITYC == 4 ~ 4,           # Unemployed
      W9DACTIVITYC == 5 ~ 5,           # Education: School/college/university
      W9DACTIVITYC == 6 ~ 6,           # Apprenticeship
      W9DACTIVITYC == 7 ~ 7,           # On gov't scheme for employment training
      W9DACTIVITYC == 8 ~ 8,           # Sick or disabled
      W9DACTIVITYC == 9 ~ 9,           # Looking after home or family
      W9DACTIVITYC == 10 ~ 10,         # Something else
      W9DACTIVITYC < 0 ~ -999,  # Missing codes
      TRUE ~ as.integer(W9DACTIVITYC)
    )
  )

# Check that variables were created
print(sprintf('ecoact17 exists: %s', 'ecoact17' %in% names(all_data)))
print(sprintf('ecoact18 exists: %s', 'ecoact18' %in% names(all_data)))
print(sprintf('ecoact19 exists: %s', 'ecoact19' %in% names(all_data)))
print(sprintf('ecoact20 exists: %s', 'ecoact20' %in% names(all_data)))
print(sprintf('ecoact25 exists: %s', 'ecoact25' %in% names(all_data)))
print(sprintf('ecoact32 exists: %s', 'ecoact32' %in% names(all_data)))
print(sprintf('ecoactadu25 exists: %s', 'ecoactadu25' %in% names(all_data)))
print(sprintf('ecoactadu32 exists: %s', 'ecoactadu32' %in% names(all_data)))

# Select only required variables
output_vars <- c('NSID', 'ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32', 'ecoactadu25', 'ecoactadu32')
final_data <- select(all_data, all_of(output_vars))

print('Script completed successfully')
print(head(final_data))

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')
print('Output written to data/output/cleaned_data.csv')
print(sprintf('Output size: %d rows, %d columns', nrow(final_data), ncol(final_data)))
