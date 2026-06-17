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

# Merge all waves by NSID first
all_data <- full_join(wave1, wave2, by = 'NSID')
all_data <- full_join(all_data, wave3, by = 'NSID')
all_data <- full_join(all_data, wave4, by = 'NSID')

cat('Merged data has', nrow(all_data), 'rows\n')

# Extract income variables from merged data
W1GrsswkHH <- all_data[['W1GrsswkHH']]
W2GrsswkHH <- all_data[['W2GrsswkHH']]
W3incestw <- all_data[['W3incestw']]
w4IncEstW <- all_data[['w4IncEstW']]

# Function to clean missing values
clean_income <- function(x) {
  x <- as.numeric(x)
  # Map missing codes
  x[x == -999] <- -2  # Missing in error
  x[x == -992] <- -9  # Refusal
  x[x == -99] <- -3   # HH not interviewed
  x[x == -94] <- -8   # Insufficient information
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -3] <- -8    # Not yet paid
  x[x == -1] <- -8    # Don't know
  x[x == -100] <- -2  # Schedule not applicable
  x
}

# Clean income variables
W1GrsswkHH <- clean_income(W1GrsswkHH)
W2GrsswkHH <- clean_income(W2GrsswkHH)
W3incestw <- clean_income(W3incestw)
w4IncEstW <- clean_income(w4IncEstW)

# Create continuous income for age 14 (wave1)
age14_inc <- W1GrsswkHH
age14_inc[age14_inc %in% c(-2, -8, -9, -1, -3)] <- NA

# Create continuous income for age 15 (wave2)
age15_inc <- W2GrsswkHH
age15_inc[age15_inc %in% c(-2, -8, -9, -1, -3)] <- NA

# Create banded income for ages 14-17
# Banded ranges: 1=<=49, 2=50-99, 3=100-199, 4=200-299, 5=300-399, 6=400-499, 7=500-599, 8=600-699, 9=700-799, 10=800-899, 11=900-999, 12=1000+

inc14 <- W1GrsswkHH
inc14[inc14 %in% c(-2, -8, -9, -1, -3)] <- -3  # Missing code

inc15 <- W2GrsswkHH
inc15[inc15 %in% c(-2, -8, -9, -1, -3)] <- -3

inc16 <- W3incestw
inc16[inc16 %in% c(-2, -8, -9, -1, -3)] <- -3

inc17 <- w4IncEstW
inc17[inc17 %in% c(-2, -8, -9, -1, -3)] <- -3

# Add income variables to merged data
all_data$age14_inc <- age14_inc
all_data$age15_inc <- age15_inc
all_data$inc14 <- inc14
all_data$inc15 <- inc15
all_data$inc16 <- inc16
all_data$inc17 <- inc17

# Keep only required variables: NSID and income variables
final_data <- all_data %>%
  select(NSID, age14_inc, age15_inc, inc14, inc15, inc16, inc17)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Script completed successfully!\n')
cat('Output file: data/output/cleaned_data.csv\n')
cat('Total rows:', nrow(final_data), '\n')
