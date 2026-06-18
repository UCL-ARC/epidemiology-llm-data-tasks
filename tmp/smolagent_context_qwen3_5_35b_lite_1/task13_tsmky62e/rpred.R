library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Merge all files by NSID
cleaned <- full_join(wave1, wave2, by = 'NSID')
cleaned <- full_join(cleaned, wave3, by = 'NSID')
cleaned <- full_join(cleaned, wave4, by = 'NSID')
cleaned <- full_join(cleaned, wave5, by = 'NSID')

# NS-SEC major category mapping (collapse to major categories)
collapse_nssec <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,
    x %in% c(3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4, 5) ~ 2,
    x %in% c(6, 7.1, 7.2, 7.3, 7.4) ~ 3,
    x %in% c(8.1, 8.2, 9.1, 9.2, 10, 11.1, 11.2) ~ 4,
    x %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7, 13.1, 13.2, 13.3, 13.4, 13.5) ~ 5,
    x %in% c(14.1, 14.2, 14.3, 15, 16, 17) ~ 6,
    TRUE ~ NA_real_
  )
}

standardize_missing <- function(x) {
  x <- if_else(x == -999, -2, x)
  x <- if_else(x == -99, -1, x)
  x <- if_else(x == -98, -1, x)
  x <- if_else(x == -94, -8, x)
  x[is.na(x)] <- -3
  return(x)
}

collapse_nssec_variable <- function(var, age) {
  var_std <- standardize_missing(var)
  var_collapsed <- collapse_nssec(var_std)
  var_collapsed[is.na(var_collapsed)] <- -3
  return(var_collapsed)
}

# Create the output variables
cleaned$nssecma14 <- collapse_nssec_variable(cleaned$W1nsseccatmum, 14)
cleaned$nssecpa14 <- collapse_nssec_variable(cleaned$W1nsseccatdad, 14)
cleaned$nssecma15 <- collapse_nssec_variable(cleaned$W2nsseccatmum, 15)
cleaned$nssecpa15 <- collapse_nssec_variable(cleaned$W2nsseccatdad, 15)
cleaned$nssecma16 <- collapse_nssec_variable(cleaned$W3cnsseccatmum, 16)
cleaned$nssecpa16 <- collapse_nssec_variable(cleaned$W3cnsseccatdad, 16)
cleaned$nssecma17 <- collapse_nssec_variable(cleaned$w4cnsseccatmum, 17)
cleaned$nssecpa17 <- collapse_nssec_variable(cleaned$w4cnsseccatdad, 17)
cleaned$nssecma18 <- collapse_nssec_variable(cleaned$w5Cnsseccatmum, 18)
cleaned$nssecpa18 <- collapse_nssec_variable(cleaned$w5Cnsseccatdad, 18)

# Select only NSID and the derived NS-SEC variables
output <- cleaned %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, 
         nssecma16, nssecpa16, nssecma17, nssecpa17,
         nssecma18, nssecpa18)

# Write to CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Cleaned data written to data/output/cleaned_data.csv\n')
cat('Number of observations:', nrow(output), '\n')
cat('Number of variables:', ncol(output), '\n')