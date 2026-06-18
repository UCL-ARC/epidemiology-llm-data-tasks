library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')

# Function to collapse NS-SEC codes to major categories
collapse_nssec <- function(x) {
  result <- x
  
  # Handle NA and missing values
  result[is.na(x)] <- NA
  
  # Map detailed codes to major categories
  # Main category 1: Employers in large organisations
  result[x == 1] <- '1'
  # Main category 2: Higher managerial
  result[x == 2] <- '2'
  # Main category 3: Higher professional
  result[x %in% c(3.1, 3.2, 3.3, 3.4)] <- '3'
  # Main category 4: Lower professional
  result[x %in% c(4.1, 4.2, 4.3, 4.4)] <- '4'
  # Main category 5: Lower managerial
  result[x == 5] <- '5'
  # Main category 6: Higher supervisory
  result[x == 6] <- '6'
  # Main category 7: Intermediate (7.x)
  result[x %in% c(7.1, 7.2, 7.3, 7.4)] <- '7'
  # Main category 8: Employers in small orgs
  result[x %in% c(8.1, 8.2)] <- '8'
  # Main category 9: Own account workers
  result[x %in% c(9.1, 9.2)] <- '9'
  # Main category 10: Lower supervisory
  result[x == 10] <- '10'
  # Main category 11: Lower technical
  result[x %in% c(11.1, 11.2)] <- '11'
  # Main category 12: Semi routine
  result[x %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7)] <- '12'
  # Main category 13: Routine
  result[x %in% c(13.1, 13.2, 13.3, 13.4, 13.5)] <- '13'
  # Main category 14: Never worked
  result[x == 14.1] <- '14'
  # Main category 14.2: Long-term unemployed
  result[x == 14.2] <- '14'
  # Main category 15: Full-time students
  result[x == 15.0] <- '15'
  # Main category 16: Not classified
  result[x == 16.0] <- '16'
  # Main category 17: Not classifiable
  result[x == 17.0] <- '17'
  
  return(result)
}

# Define missing value codes for each wave
missing_codes_1 <- c(-999, -99, -98, -94, -999)
missing_codes_2 <- c(-999, -99, -98, -94, -999)
missing_codes_3 <- c(-999, -99, -98, -94, -999)
missing_codes_4 <- c(-999, -99, -98, -94, -999)
missing_codes_5 <- c(-999, -98, -999)

# Process wave 1 (age 14)
wave1_mum <- wave1 %>%
  mutate(
    nssecma14 = collapse_nssec(W1nsseccatmum),
    nssecpa14 = collapse_nssec(W1nsseccatdad)
  ) %>%
  mutate(
    nssecma14 = factor(nssecma14, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable')),
    nssecpa14 = factor(nssecpa14, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable'))
  )

# Process wave 2 (age 15)
wave2_mum <- wave2 %>%
  mutate(
    nssecma15 = collapse_nssec(W2nsseccatmum),
    nssecpa15 = collapse_nssec(W2nsseccatdad)
  ) %>%
  mutate(
    nssecma15 = factor(nssecma15, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable')),
    nssecpa15 = factor(nssecpa15, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable'))
  )

# Process wave 3 (age 16)
wave3_mum <- wave3 %>%
  mutate(
    nssecma16 = collapse_nssec(W3cnsseccatmum),
    nssecpa16 = collapse_nssec(W3cnsseccatdad)
  ) %>%
  mutate(
    nssecma16 = factor(nssecma16, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable')),
    nssecpa16 = factor(nssecpa16, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable'))
  )

# Process wave 4 (age 17)
wave4_mum <- wave4 %>%
  mutate(
    nssecma17 = collapse_nssec(w4cnsseccatmum),
    nssecpa17 = collapse_nssec(w4cnsseccatdad)
  ) %>%
  mutate(
    nssecma17 = factor(nssecma17, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable')),
    nssecpa17 = factor(nssecpa17, levels = c(as.character(-999), as.character(-99), as.character(-98), as.character(-94), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable'))
  )

# Process wave 5 (age 18)
wave5_mum <- wave5 %>%
  mutate(
    nssecma18 = collapse_nssec(w5Cnsseccatmum),
    nssecpa18 = collapse_nssec(w5Cnsseccatdad)
  ) %>%
  mutate(
    nssecma18 = factor(nssecma18, levels = c(as.character(-999), as.character(-98), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable')),
    nssecpa18 = factor(nssecpa18, levels = c(as.character(-999), as.character(-98), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), labels = c('Missing', 'Missing', 'Employers', 'Higher managerial', 'Higher professional', 'Lower professional', 'Lower managerial', 'Higher supervisory', 'Intermediate', 'Employers small org', 'Own account', 'Lower supervisory', 'Lower technical craft', 'Semi routine', 'Routine', 'Never worked', 'Full-time students', 'Not classified', 'Not classifiable'))
  )

# Full join all datasets
final_data <- full_join(
  wave1_mum, wave2_mum, by = 'NSID'
) %>%
  full_join(wave3_mum, by = 'NSID') %>%
  full_join(wave4_mum, by = 'NSID') %>%
  full_join(wave5_mum, by = 'NSID')

# Select and order final variables
final_data <- final_data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')
