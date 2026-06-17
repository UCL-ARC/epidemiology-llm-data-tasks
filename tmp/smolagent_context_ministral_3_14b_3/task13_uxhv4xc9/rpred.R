
library(readr)
library(dplyr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')

# Define functions
collapse_frac_to_major <- function(x) {
  as.integer(as.character(x))
}

map_missing <- function(x) {
  replace(x, is.na(x) | x == -98, -3)
}

# Create variables for each wave
wave1_nssecma14 <- wave1 %>%
  transmute(NSID, nssecma14 = collapse_frac_to_major(map_missing(W1nsseccatmum)))

wave1_nssecpa14 <- wave1 %>%
  transmute(NSID, nssecpa14 = collapse_frac_to_major(map_missing(W1nsseccatdad)))

wave2_nssecma15 <- wave2 %>%
  transmute(NSID, nssecma15 = collapse_frac_to_major(map_missing(W2nsseccatmum)))

wave2_nssecpa15 <- wave2 %>%
  transmute(NSID, nssecpa15 = collapse_frac_to_major(map_missing(W2nsseccatdad)))

wave3_nssecma16 <- wave3 %>%
  transmute(NSID, nssecma16 = collapse_frac_to_major(map_missing(W3cnsseccatmum)))

wave3_nssecpa16 <- wave3 %>%
  transmute(NSID, nssecpa16 = collapse_frac_to_major(map_missing(W3cnsseccatdad)))

wave4_nssecma17 <- wave4 %>%
  transmute(NSID, nssecma17 = collapse_frac_to_major(map_missing(w4cnsseccatmum)))

wave4_nssecpa17 <- wave4 %>%
  transmute(NSID, nssecpa17 = collapse_frac_to_major(map_missing(w4cnsseccatdad)))

wave5_nssecma18 <- wave5 %>%
  transmute(NSID, nssecma18 = collapse_frac_to_major(map_missing(w5Cnsseccatmum)))

wave5_nssecpa18 <- wave5 %>%
  transmute(NSID, nssecpa18 = collapse_frac_to_major(map_missing(w5Cnsseccatdad)))

# Join all datasets
cleaned_data <- full_join(wave1_nssecma14, wave1_nssecpa14, by = 'NSID') %>%
  full_join(wave2_nssecma15, by = 'NSID') %>%
  full_join(wave2_nssecpa15, by = 'NSID') %>%
  full_join(wave3_nssecma16, by = 'NSID') %>%
  full_join(wave3_nssecpa16, by = 'NSID') %>%
  full_join(wave4_nssecma17, by = 'NSID') %>%
  full_join(wave4_nssecpa17, by = 'NSID') %>%
  full_join(wave5_nssecma18, by = 'NSID') %>%
  full_join(wave5_nssecpa18, by = 'NSID')

# Write output
write_csv(cleaned_data, 'data/output/cleaned_data.csv')
