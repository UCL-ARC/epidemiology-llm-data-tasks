library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Vector of labels for NS-SEC categories
nssec_labels <- c(
  'Employers in large organisations',
  'Higher managerial occupations',
  'Higher professional occupations',
  'Lower professional occupations',
  'Lower managerial occupations',
  'Higher supervisory occupations',
  'Intermediate clerical and administrative',
  'Intermediate sales and service',
  'Intermediate technical and auxiliary',
  'Intermediate engineering',
  'Employers in small orgs non-professional',
  'Employers in small orgs agriculture',
  'Own account workers non professional',
  'Own account workers agriculture',
  'Lower supervisory occupations',
  'Lower technical craft',
  'Lower technical process operative',
  'Semi routine sales',
  'Semi routine services',
  'Semi routine technical',
  'Semi routine operative',
  'Semi routine agricultural',
  'Semi routine clerical',
  'Semi routine childcare',
  'Routine sales and service',
  'Routine production',
  'Routine technical',
  'Routine operative',
  'Routine agricultural',
  'Never worked',
  'Long-term unemployed',
  'Full-time students',
  'Not classified or inadequately stated',
  'Not classifiable for other reasons'
)

# Create function to collapse fractional codes and handle missing values
collapse_and_clean <- function(x) {
  # Convert to numeric, handle non-numeric values
  x <- as.numeric(x)
  
  # If all NA or if NA, return NA
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  # Keep NA as is
  result <- ifelse(is.na(x), NA_real_, x)
  
  # Convert -98 to -3
  result <- ifelse(result == -98, -3, result)
  
  # For positive values, take integer part (collapse fractional codes)
  result <- ifelse(result > 0 & !is.na(result), as.integer(result), result)
  
  return(result)
}

# Process wave 1 (age 14)
result_w1 <- wave1 %>%
  mutate(
    nssecma14 = collapse_and_clean(W1nsseccatmum),
    nssecpa14 = collapse_and_clean(W1nsseccatdad)
  )

# Process wave 2 (age 15)
result_w2 <- wave2 %>%
  mutate(
    nssecma15 = collapse_and_clean(W2nsseccatmum),
    nssecpa15 = collapse_and_clean(W2nsseccatdad)
  )

# Process wave 3 (age 16)
result_w3 <- wave3 %>%
  mutate(
    nssecma16 = collapse_and_clean(W3cnsseccatmum),
    nssecpa16 = collapse_and_clean(W3cnsseccatdad)
  )

# Process wave 4 (age 17)
result_w4 <- wave4 %>%
  mutate(
    nssecma17 = collapse_and_clean(w4cnsseccatmum),
    nssecpa17 = collapse_and_clean(w4cnsseccatdad)
  )

# Process wave 5 (age 18)
result_w5 <- wave5 %>%
  mutate(
    nssecma18 = collapse_and_clean(w5Cnsseccatmum),
    nssecpa18 = collapse_and_clean(w5Cnsseccatdad)
  )

# Start with wave 1 and progressively join
final <- result_w1 %>%
  full_join(result_w2, by = 'NSID') %>%
  full_join(result_w3, by = 'NSID') %>%
  full_join(result_w4, by = 'NSID') %>%
  full_join(result_w5, by = 'NSID')

# Keep only ID and the 10 variable columns
final <- final %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write output
write_csv(final, 'data/output/cleaned_data.csv')
