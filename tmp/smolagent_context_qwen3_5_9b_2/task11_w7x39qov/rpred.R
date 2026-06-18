library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all four wave files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Create a mapping function for missing values - map to -3 (not asked)
map_missing <- function(x) {
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[x == -996] <- -3
  return(x)
}

# Define value labels
value_labels <- c(
  '1' = 'Doing paid work for 30 or more hours a week',
  '2' = 'Doing paid work for fewer than 30 hours a week',
  '3' = 'Unemployed/ Looking for a job',
  '4' = 'On a training course or scheme',
  '5' = 'In full-time education/ at school',
  '6' = 'Looking after the family/ household',
  '7' = 'Retired from work altogether',
  '8' = 'Sick/ disabled',
  '9' = 'Other'
)

# Extract variables from each wave with missing value mapping
wave1_data <- wave1 %>%
  select(NSID, W1empsmum, W1empsdad) %>%
  mutate(
    ecoactma14 = map_missing(W1empsmum),
    ecoactpa14 = map_missing(W1empsdad)
  )

wave2_data <- wave2 %>%
  select(NSID, W2empsmum, W2empsdad) %>%
  mutate(
    ecoactma15 = map_missing(W2empsmum),
    ecoactpa15 = map_missing(W2empsdad)
  )

wave3_data <- wave3 %>%
  select(NSID, W3empsmum, W3empsdad) %>%
  mutate(
    ecoactma16 = map_missing(W3empsmum),
    ecoactpa16 = map_missing(W3empsdad)
  )

wave4_data <- wave4 %>%
  select(NSID, w4empsmum, w4empsdad) %>%
  mutate(
    ecoactma17 = map_missing(w4empsmum),
    ecoactpa17 = map_missing(w4empsdad)
  )

# Start with the largest wave and join others
data_combined <- wave1_data

# Join other waves on NSID
data_combined <- data_combined %>%
  full_join(wave2_data, by = 'NSID') %>%
  full_join(wave3_data, by = 'NSID') %>%
  full_join(wave4_data, by = 'NSID')

# Select only required variables: NSID and the 8 derived variables
# Remove raw source variables
result <- data_combined %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write output
write_csv(result, 'data/output/cleaned_data.csv')

cat('Dataset created with', nrow(result), 'rows and', ncol(result), 'columns\n')
cat('Variables:', paste(names(result), collapse=', '), '\n')
