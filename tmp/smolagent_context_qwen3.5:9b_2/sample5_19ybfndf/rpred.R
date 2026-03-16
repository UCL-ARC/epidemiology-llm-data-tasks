library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = cols())
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols())

# Wave 6: Harmonize missing values
wave6_harm <- wave6 %>%
  mutate(
    W6MarStatYP_std = case_when(
      W6MarStatYP %in% c(-999, -97, -92) ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -9,
      is.na(W6MarStatYP) ~ -3,
      TRUE ~ as.numeric(W6MarStatYP)
    )
  ) %>%
  mutate(
    W6MarStatYP_std = factor(W6MarStatYP_std,
      levels = c(-3, -9, -1, 1, 2, 3, 4, 5),
      labels = c('Not_asked', 'Refused', 'Not_applicable', 'Single_never_married', 'Married', 'Separated', 'Divorced', 'Widowed')
    )
  )

# Wave 8: Harmonize missing values
ns8_harm <- ns8 %>%
  mutate(
    W8DMARSTAT_std = case_when(
      is.na(W8DMARSTAT) ~ -3,
      TRUE ~ as.numeric(W8DMARSTAT)
    )
  ) %>%
  mutate(
    W8DMARSTAT_std = factor(W8DMARSTAT_std,
      levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c('Not_asked', 'Refused', 'Insufficient_info', 'Not_applicable', 'Single_never_married', 'Married', 'Separated', 'Divorced', 'Widowed', 'Civil_partner', 'Separated_CP', 'Former_CP', 'Surviving_CP')
    )
  )

# Wave 9: Harmonize missing values
ns9_harm <- ns9 %>%
  mutate(
    W9DMARSTAT_std = case_when(
      is.na(W9DMARSTAT) ~ -3,
      TRUE ~ as.numeric(W9DMARSTAT)
    )
  ) %>%
  mutate(
    W9DMARSTAT_std = factor(W9DMARSTAT_std,
      levels = c(-3, -9, -8, 1, 2, 3, 4, 5, 6, 7, 8),
      labels = c('Not_asked', 'Refused', 'Insufficient_info', 'Single_never_married_or_never_CP', 'Married', 'Divorced', 'Legally_separated', 'Widowed', 'Civil_partner', 'Former_CP_dissolved', 'Surviving_CP_died')
    )
  )

# Merge all waves
cleaned_data <- full_join(wave1, wave4, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave6_harm, by = 'NSID')
cleaned_data <- full_join(cleaned_data, ns8_harm, by = 'NSID')
cleaned_data <- full_join(cleaned_data, ns9_harm, by = 'NSID')

# Create collapsed harmonized marital status in single mutate chain
cleaned_data <- cleaned_data %>%
  mutate(
    # Wave 6 collapsed to 5 categories
    MarStatCollapsed_W6 = factor(case_when(
      W6MarStatYP_std %in% c(-3, -9, -1) ~ NA_character_,
      W6MarStatYP_std == 1 ~ 'Single_never_married',
      W6MarStatYP_std == 2 ~ 'Married',
      W6MarStatYP_std %in% c(3, 4) ~ 'Separated_divorced',
      W6MarStatYP_std == 5 ~ 'Widowed'
    ), levels = c('Single_never_married', 'Married', 'Separated_divorced', 'Widowed'), exclude = c(NA_character_)),
    
    # Wave 8 collapsed to 5 categories
    MarStatCollapsed_W8 = factor(case_when(
      W8DMARSTAT_std %in% c(-3, -9, -8, -1) ~ NA_character_,
      W8DMARSTAT_std == 1 ~ 'Single_never_married',
      W8DMARSTAT_std == 2 ~ 'Married',
      W8DMARSTAT_std %in% c(3, 8) ~ 'Separated',
      W8DMARSTAT_std %in% c(4, 5) ~ 'Divorced_widowed',
      W8DMARSTAT_std %in% c(6, 7, 9) ~ 'Civil_partner'
    ), levels = c('Single_never_married', 'Married', 'Separated', 'Divorced_widowed', 'Civil_partner'), exclude = c(NA_character_)),
    
    # Wave 9 collapsed to 5 categories
    MarStatCollapsed_W9 = factor(case_when(
      W9DMARSTAT_std %in% c(-3, -9, -8) ~ NA_character_,
      W9DMARSTAT_std == 1 ~ 'Single_never_married',
      W9DMARSTAT_std == 2 ~ 'Married',
      W9DMARSTAT_std %in% c(3, 4) ~ 'Separated_divorced',
      W9DMARSTAT_std %in% c(5, 6, 7, 8) ~ 'Civil_partner_widowed'
    ), levels = c('Single_never_married', 'Married', 'Separated_divorced', 'Civil_partner_widowed'), exclude = c(NA_character_))
  )

# Output variables - age-specific harmonized + collapsed
output_vars <- c(
  'NSID',
  'W6MarStatYP_std',
  'W8DMARSTAT_std',
  'W9DMARSTAT_std',
  'MarStatCollapsed_W6',
  'MarStatCollapsed_W8',
  'MarStatCollapsed_W9'
)

final_data <- cleaned_data %>%
  select(all_of(output_vars))

write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
cat(sprintf('Total rows: %d\n', nrow(final_data)))
cat(sprintf('Total columns: %d\n', ncol(final_data)))
