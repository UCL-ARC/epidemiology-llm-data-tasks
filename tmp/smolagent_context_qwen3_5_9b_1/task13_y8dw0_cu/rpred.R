library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the mapping of wave names to ages
wave_age <- c(
  'wave_one_lsype_family_background_2020.tab' = 14,
  'wave_two_lsype_family_background_2020.tab' = 15,
  'wave_three_lsype_family_background_2020.tab' = 16,
  'wave_four_lsype_family_background_2020.tab' = 17,
  'wave_five_lsype_family_background_2020.tab' = 18
)

# Define the source variables for each wave
source_vars <- c(
  'wave_one_lsype_family_background_2020.tab' = c('W1nsseccatmum', 'W1nsseccatdad'),
  'wave_two_lsype_family_background_2020.tab' = c('W2nsseccatmum', 'W2nsseccatdad'),
  'wave_three_lsype_family_background_2020.tab' = c('W3cnsseccatmum', 'W3cnsseccatdad'),
  'wave_four_lsype_family_background_2020.tab' = c('w4cnsseccatmum', 'w4cnsseccatdad'),
  'wave_five_lsype_family_background_2020.tab' = c('w5Cnsseccatmum', 'w5Cnsseccatdad')
)

# Function to collapse fractional codes to major category
collapse_nssec <- function(x) {
  # Convert to numeric if needed
  x <- as.numeric(as.character(x))
  
  # Handle special missing codes
  # -98: Parent not present -> -3 (Not applicable)
  ifelse(x == -98, -3, x)
}

# Function to recode NS-SEC codes to major category and return with labels
recode_nssec <- function(x) {
  x <- as.numeric(as.character(x))
  
  # Handle -98 (Parent not present) -> -3
  result <- ifelse(x == -98, -3, x)
  
  # Minor NA handling -> -3
  result[is.na(result)] <- -3
  
  # Collapse fractional codes (e.g., 3.1, 3.2 -> 3, 4.1, 4.2 -> 4)
  result <- ifelse(result > 0 & result < 100, floor(result), result)
  
  return(result)
}

# Define the 17-category labels
nssec_labels <- c(
  '-3' = 'Not applicable / Parent not present',
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial occupations',
  '3' = 'Higher professional occupations',
  '4' = 'Lower professional occupations',
  '5' = 'Lower managerial occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate occupations',
  '8' = 'Employers in small orgs non-professional',
  '9' = 'Own account workers non professional',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical craft',
  '12' = 'Semi routine occupations',
  '13' = 'Routine occupations',
  '14' = 'Never worked',
  '15' = 'Long-term unemployed',
  '16' = 'Full-time students',
  '17' = 'Not classifiable'
)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = 'c')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = 'c')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = 'c')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = 'c')
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', col_types = 'c')

# Process each wave and create output variables
# Wave 1 (age 14)
wave1 <- wave1 %>%
  mutate(
    nssecma14 = recode_nssec(W1nsseccatmum),
    nssecpa14 = recode_nssec(W1nsseccatdad)
  )

# Wave 2 (age 15)
wave2 <- wave2 %>%
  mutate(
    nssecma15 = recode_nssec(W2nsseccatmum),
    nssecpa15 = recode_nssec(W2nsseccatdad)
  )

# Wave 3 (age 16)
wave3 <- wave3 %>%
  mutate(
    nssecma16 = recode_nssec(W3cnsseccatmum),
    nssecpa16 = recode_nssec(W3cnsseccatdad)
  )

# Wave 4 (age 17)
wave4 <- wave4 %>%
  mutate(
    nssecma17 = recode_nssec(w4cnsseccatmum),
    nssecpa17 = recode_nssec(w4cnsseccatdad)
  )

# Wave 5 (age 18) - Note: measures partner, but still use the suffix
wave5 <- wave5 %>%
  mutate(
    nssecma18 = recode_nssec(w5Cnsseccatmum),
    nssecpa18 = recode_nssec(w5Cnsseccatdad)
  )

# Full join all datasets
combined <- full_join(
  wave1, wave2, by = 'NSID'
) %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID')

# Select and order final variables: NSID and the 10 derived variables
final_vars <- c('NSID',
                'nssecma14', 'nssecpa14',
                'nssecma15', 'nssecpa15',
                'nssecma16', 'nssecpa16',
                'nssecma17', 'nssecpa17',
                'nssecma18', 'nssecpa18')

cleaned_data <- combined %>%
  select(all_of(final_vars))

# Write output
write_csv(cleaned_data, 'data/output/cleaned_data.csv')

cat('Done!\n')

