library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  age14 = 'data/input/wave_one_lsype_family_background_2020.tab',
  age15 = 'data/input/wave_two_lsype_family_background_2020.tab',
  age16 = 'data/input/wave_three_lsype_family_background_2020.tab',
  age17 = 'data/input/wave_four_lsype_family_background_2020.tab'
)

# Create output directory if needed
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)

# Function to map missing codes to standard
code_missing_age14_age15 <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -3
  x[x == -992] <- -9
  x[x == -99] <- -9
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -3] <- -1  # Not yet paid -> Item not applicable
  x[x == -1] <- -8  # Don't know -> Don't know/insufficient information
  x
}

# Function to map missing codes for age 16
code_missing_age16 <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -3
  x[x == -992] <- -9
  x[x == -99] <- -9
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x
}

# Function to map missing codes for age 17
code_missing_age17 <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -996] <- -3  # No parent in household -> Not asked/interviewed
  x[x == -999] <- -3
  x[x == -992] <- -9
  x[x == -99] <- -9
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x
}

# Banding function for ages 14-15
band_income <- function(x) {
  x[is.na(x)] <- -1
  x[x > 0] <- round(x[x > 0])
  x[x < 0] <- -1
  x[is.na(x)] <- -1
  x[x == 0] <- -1
  
  x <- factor(x, levels = c(1:12, -9, -8, -1, -3),
              labels = c('1: Up to £49', '2: £50 up to £99', '3: £100 up to £199',
                        '4: £200 up to £299', '5: £300 up to £399', '6: £400 up to £499',
                        '7: £500 up to £599', '8: £600 up to £699', '9: £700 up to £799',
                        '10: £800 up to £899', '11: £900 up to £999', '12: £1,000 or more',
                        'Refusal', 'Don\'t know/insufficient information', 'Item not applicable', 'Not asked/interviewed'))
  x
}

# Load and process age 14
age14 <- read_delim(files$age14, delim = '\t', col_types = cols())
age14 <- age14 %>%
  rename(NSID = NSID, incwhhcnt14 = W1GrsswkHH) %>%
  filter(!is.na(NSID)) %>%
  mutate(incwhhcnt14 = code_missing_age14_age15(incwhhcnt14)) %>%
  mutate(incwhh14 = band_income(incwhhcnt14)) %>%
  select(NSID, incwhhcnt14, incwhh14)

# Load and process age 15
age15 <- read_delim(files$age15, delim = '\t', col_types = cols())
age15 <- age15 %>%
  rename(NSID = NSID, incwhhcnt15 = W2GrsswkHH) %>%
  filter(!is.na(NSID)) %>%
  mutate(incwhhcnt15 = code_missing_age14_age15(incwhhcnt15)) %>%
  mutate(incwhh15 = band_income(incwhhcnt15)) %>%
  select(NSID, incwhhcnt15, incwhh15)

# Load and process age 16
age16 <- read_delim(files$age16, delim = '\t', col_types = cols())
age16 <- age16 %>%
  rename(NSID = NSID, incwhh16 = W3incestw) %>%
  filter(!is.na(NSID)) %>%
  mutate(incwhh16 = code_missing_age16(incwhh16)) %>%
  mutate(incwhh16 = band_income(incwhh16)) %>%
  select(NSID, incwhh16)

# Load and process age 17
age17 <- read_delim(files$age17, delim = '\t', col_types = cols())
age17 <- age17 %>%
  rename(NSID = NSID, incwhh17 = w4IncEstW) %>%
  filter(!is.na(NSID)) %>%
  mutate(incwhh17 = code_missing_age17(incwhh17)) %>%
  mutate(incwhh17 = band_income(incwhh17)) %>%
  select(NSID, incwhh17)

# Merge all datasets
dataset <- full_join(age14, age15, by = 'NSID') %>%
  full_join(age16, by = 'NSID') %>%
  full_join(age17, by = 'NSID')

# Select final variables
dataset <- dataset %>%
  select(NSID, incwhh14, incwhhcnt14, incwhh15, incwhhcnt15, incwhh16, incwhh17)

# Write output
write_csv(dataset, 'data/output/cleaned_data.csv')

print('Processing complete!')
print(paste('Output written to: data/output/cleaned_data.csv'))
print(paste('Rows:', nrow(dataset)))
print(paste('Columns:', ncol(dataset)))