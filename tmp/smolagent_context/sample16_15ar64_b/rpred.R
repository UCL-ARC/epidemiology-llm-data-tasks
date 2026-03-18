# Load required packages
library(haven)
library(dplyr)
library(readr)

# Load data files
wave_one <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge datasets
merged <- full_join(wave_one, wave_two, by = 'NSID')
merged <- full_join(merged, wave_three, by = 'NSID')
merged <- full_join(merged, wave_four, by = 'NSID')

# Standardize missing values
standardize_missing <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(-999, -99, -992, -94, -91)] <- -3
  x[x == -3] <- -1
  x[x == -996] <- -3
  x[is.na(x)] <- -3
  return(x)
}

# Apply standardization to income variables
merged$W1GrsswkHH <- standardize_missing(merged$W1GrsswkHH)
merged$W2GrsswkHH <- standardize_missing(merged$W2GrsswkHH)
merged$W3incestw <- standardize_missing(merged$W3incestw)
merged$w4IncEstW <- standardize_missing(merged$w4IncEstW)

# Create continuous variables
merged$incwhhcnt14 <- merged$W1GrsswkHH
merged$incwhhcnt15 <- merged$W2GrsswkHH

# Create banded variables for ages 14-15
create_bands <- function(x) {
  bands <- cut(x, breaks = c(-Inf, 0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, Inf),
               labels = FALSE, include.lowest = TRUE, right = FALSE)
  bands[bands < 0] <- x[bands < 0]
  return(factor(bands, levels = c(-9, -8, -1, -3, -2, 1:12),
               labels = c('Refusal', 'Don\'t know', 'Item not applicable', 
                         'Not asked', 'Script error', 
                         'Up to £49', '£50-£99', '£100-£199', '£200-£299', 
                         '£300-£399', '£400-£499', '£500-£599', 
                         '£600-£699', '£700-£799', '£800-£899', 
                         '£900-£999', '£1000+')))
}

# Apply banding
merged$incwhh14 <- create_bands(merged$incwhhcnt14)
merged$incwhh15 <- create_bands(merged$incwhhcnt15)

# Create factor variables for ages 16-17
merged$incwhh16 <- factor(merged$W3incestw, levels = c(-99, -92, -1, 1:12), 
                          labels = c('MP not interviewed', 'Refused', 'Don\'t know', 
                                    'Up to £49', '£50-£99', '£100-£199', '£200-£299', 
                                    '£300-£399', '£400-£499', '£500-£599', '£600-£699', 
                                    '£700-£799', '£800-£899', '£900-£999', '£1000+'))

merged$incwhh17 <- factor(merged$w4IncEstW, levels = c(-996, -99, -92, -1, 1:12), 
                          labels = c('No parent', 'MP not interviewed', 'Refused', 
                                    'Don\'t know', 'Up to £49', '£50-£99', '£100-£199', 
                                    '£200-£299', '£300-£399', '£400-£499', '£500-£599', 
                                    '£600-£699', '£700-£799', '£800-£899', '£900-£999', '£1000+'))

# Select final variables
final_data <- merged[, c('NSID', 'incwhh14', 'incwhh15', 'incwhhcnt14', 'incwhhcnt15', 'incwhh16', 'incwhh17')]

# Write output
write.csv(final_data, 'data/output/cleaned_data.csv', row.names = FALSE)
cat('Data processing complete. Output saved to data/output/cleaned_data.csv')